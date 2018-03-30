use std::ops::{Index, IndexMut};

use word::{Header, Tag, UnpackedWord, Word};

#[derive(Debug, Fail)]
pub enum AllocError {
    #[fail(display = "out of memory")]
    OutOfMemory,
}

#[derive(Debug, Clone, Copy)]
pub struct Idx(usize);

#[derive(Debug, Clone, Copy)]
pub struct Slice {
    start: usize,
    len: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct HeapEnvironment {
    addr: usize,
    nlocals: usize,
}

impl HeapEnvironment {
    pub fn parent(self) -> Idx {
        Idx(self.addr + 1)
    }

    pub fn locals(self) -> Slice {
        Slice {
            start: self.addr + 3,
            len: self.nlocals,
        }
    }

    pub fn into_word<W: Word>(self) -> W {
        W::pack(UnpackedWord::UntaggedPointer(self.addr))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct HeapClosure(usize);

impl HeapClosure {
    pub fn environment(self) -> Idx {
        Idx(self.0 + 1)
    }

    pub fn template(self) -> Idx {
        Idx(self.0 + 2)
    }

    pub fn into_word<W: Word>(self) -> W {
        W::pack(UnpackedWord::UntaggedPointer(self.0))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct HeapCons(usize);

impl HeapCons {
    pub fn head(self) -> Idx {
        Idx(self.0 + 0)
    }

    pub fn tail(self) -> Idx {
        Idx(self.0 + 1)
    }

    pub fn into_word<W: Word>(self) -> W {
        W::pack(UnpackedWord::TaggedPointer(Tag::Cons, self.0))
    }
}

#[derive(Debug)]
pub struct Heap<W: Word> {
    /// The heap's contents.
    words: Vec<W>,

    /// Maximum heap size in words.
    size: usize,

    /// The "top" of the heap, or current allocation pointer.
    top: usize,
}

impl<W: Word> Index<Idx> for Heap<W> {
    type Output = W;

    fn index(&self, idx: Idx) -> &Self::Output {
        &self.words[idx.0]
    }
}

impl<W: Word> IndexMut<Idx> for Heap<W> {
    fn index_mut(&mut self, idx: Idx) -> &mut Self::Output {
        &mut self.words[idx.0]
    }
}

impl<W: Word> Index<Slice> for Heap<W> {
    type Output = [W];

    fn index(&self, slice: Slice) -> &Self::Output {
        &self.words[slice.start..][..slice.len]
    }
}

impl<W: Word> IndexMut<Slice> for Heap<W> {
    fn index_mut(&mut self, slice: Slice) -> &mut Self::Output {
        &mut self.words[slice.start..][..slice.len]
    }
}

impl<W: Word> Heap<W> {
    /// Create a new heap with the given size.
    pub fn new(size: usize) -> Self {
        let mut words = Vec::new();
        words.resize(size, W::default());
        Self {
            words,
            size,
            top: 0,
        }
    }

    /// Reset the heap. This is very simple; we simply set the allocation pointer to zero.
    pub fn clear(&mut self) {
        self.top = 0;
    }

    /// Allocate a fixed number of words on the heap without tagging, headers, or initialization.
    pub fn alloc_raw(&mut self, words: usize) -> Result<usize, AllocError> {
        if self.top + words >= self.size {
            return Err(AllocError::OutOfMemory);
        }

        let addr = self.top;
        self.top += words;
        Ok(addr)
    }

    /// Allocate an environment.
    pub fn alloc_environment(
        &mut self,
        parent: W,
        nlocals: usize,
    ) -> Result<HeapEnvironment, AllocError> {
        let addr = self.alloc_raw(3 + nlocals)?;
        self.words[addr + 0] = W::pack(UnpackedWord::Header(Header::Environment));
        self.words[addr + 1] = parent;
        self.words[addr + 2] = W::pack(UnpackedWord::Raw(nlocals as u64));
        Ok(HeapEnvironment { addr, nlocals })
    }

    /// Allocate a closure.
    pub fn alloc_closure(&mut self, env: W, template: usize) -> Result<HeapClosure, AllocError> {
        let addr = self.alloc_raw(3)?;
        self.words[addr + 0] = W::pack(UnpackedWord::Header(Header::Closure));
        self.words[addr + 1] = env;
        self.words[addr + 2] = W::pack(UnpackedWord::Raw(template as u64));
        Ok(HeapClosure(addr))
    }

    /// Allocate a cons cell.
    pub fn alloc_cons(&mut self) -> Result<HeapCons, AllocError> {
        Ok(HeapCons(self.alloc_raw(2)?))
    }
}

// A key-value map intended for stack storage and implemented as a linked list of pairs.
// Inefficient and silly.
#[derive(Debug, Clone, Copy)]
enum Link<'a, W: Word> {
    Nil,
    Cons {
        old: usize,
        new: W,
        next: &'a Link<'a, W>,
    },
}

impl<'a, W: Word> Link<'a, W> {
    fn get(&self, target: usize) -> Option<W> {
        match *self {
            Link::Nil => None,
            Link::Cons { old, new, .. } if old == target => Some(new),
            Link::Cons { next, .. } => next.get(target),
        }
    }
}

impl<W: Word> Heap<W> {
    fn copy_tagged(
        &mut self,
        tag: Tag,
        idx: usize,
        from: &Self,
        list: Link<W>,
    ) -> Result<W, AllocError> {
        match tag {
            Tag::Cons => {
                let cons = self.alloc_cons()?;
                let list = Link::Cons {
                    old: idx,
                    new: cons.into_word(),
                    next: &list,
                };
                self[cons.head()] = self.copy_word(from.words[idx + 0], from, list)?;
                self[cons.tail()] = self.copy_word(from.words[idx + 1], from, list)?;
                Ok(cons.into_word())
            }
        }
    }

    fn copy_untagged(&mut self, idx: usize, from: &Self, list: Link<W>) -> Result<W, AllocError> {
        let header = match list.get(idx) {
            Some(addr) => return Ok(addr),
            None => match from.words[idx].unpack().unwrap() {
                UnpackedWord::Header(header) => header,
                _ => unreachable!(),
            },
        };

        match header {
            Header::Moved(_) => {
                unreachable!("cannot copy an object which has been previously moved!")
            }
            Header::Environment => unreachable!("cannot copy environments!"),
            Header::Closure => unreachable!("cannot copy closures!"),
        }
    }

    fn copy_word(&mut self, word: W, from: &Self, list: Link<W>) -> Result<W, AllocError> {
        match word.unpack().unwrap() {
            UnpackedWord::Header(_) => unreachable!("headers are not valid values!"),
            UnpackedWord::Immediate(_) => Ok(word),
            UnpackedWord::TaggedPointer(tag, idx) => self.copy_tagged(tag, idx, from, list),
            UnpackedWord::UntaggedPointer(idx) => self.copy_untagged(idx, from, list),
            UnpackedWord::Raw(_) => unreachable!("raw words are not valid values!"),
        }
    }

    /// Deep-copy a value from another heap, non-destructively.
    pub fn copy_value(&mut self, word: W, from: &Heap<W>) -> Result<W, AllocError> {
        self.copy_word(word, from, Link::Nil)
    }
}

#[derive(Debug)]
pub struct ProcessHeap<W: Word> {
    heap: Heap<W>,
}

#[derive(Debug)]
pub struct MessageHeap<W: Word> {
    heap: Heap<W>,
    index: Vec<W>,
}

impl<W: Word> ProcessHeap<W> {}
