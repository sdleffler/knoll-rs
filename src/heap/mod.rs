#[macro_use]
mod macros;

use std::{iter, marker::PhantomData, ops::{Index, IndexMut, Range}};

#[doc(hidden)]
#[derive(Debug, Clone, Copy)]
pub enum Field {
    Word,
    WordSlice,
    Raw,
    RawSlice,
}

#[derive(Debug, Fail)]
pub enum HeapError {
    #[fail(display = "tag mismatch")]
    TagMismatch,

    #[fail(display = "expected header for untagged allocation requiring header, found non-header word")]
    BadHeader,

    #[fail(display = "expected a non-raw word, found raw word")]
    BadWord,

    #[fail(display = "access out of bounds")]
    OutOfBounds,

    #[fail(display = "out of memory")]
    OutOfMemory,
}

layout_struct! {
    extern struct Environment {
        parent: Word,
        locals: [Word],
    }
}

layout_struct! {
    extern struct Closure {
        environment: Word,
        template: Raw,
    }
}

/// Trait for types usable as "machine words" for the VM.
pub trait Word: Copy + Eq + 'static {
    type Tag: Type<Self> + Copy + Eq + 'static;

    fn pack(Unpacked<Self::Tag>) -> Self;
    fn unpack(self) -> Unpacked<Self::Tag>;

    /// Try to interpret the word as a value.
    fn value(self) -> Option<Value<Self::Tag>> {
        match self.unpack() {
            Unpacked::Value(value) => Some(value),
            Unpacked::Header(_) | Unpacked::Raw(_) => None,
        }
    }

    /// Try to interpret the word as a header. This might fail in conditions like debug mode, where
    /// headers are distinguishable from other values.
    fn header(self) -> Option<Header<Self::Tag>> {
        match self.unpack() {
            Unpacked::Value(_) | Unpacked::Raw(_) => None,
            Unpacked::Header(hdr) => Some(hdr),
        }
    }

    /// Try to interpret the word as a raw value. This might fail in conditions like debug mode,
    /// where raw words are safely encoded and can be detected.
    fn raw(self) -> Option<u64>;
}

/// Headers may be found preceding allocations. They provide information on what sort of data
/// is allocated following them, providing type information to the runtime.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Header<T> {
    /// A header indicating that the allocation here has been moved to another heap. This header
    /// also contains the pointer that the allocation was moved to.
    Moved(usize),

    /// A header indicating that the following allocation is an environment. This roughly
    /// corresponds to a lexical scope in source code.
    ///
    /// ``` ignore
    /// [Header::Environment, parent, n, local[0], local[1], .. local[n]].len() == n + 3
    /// ```
    Environment,

    /// A header indicating that the following allocation is a closure.
    ///
    /// ``` ignore
    /// [Header::Closure, environment, template].len() == 3
    /// ```
    Closure,

    /// A header indicating the type of an allocation.
    Tag(T),
}

/// Immediate values are values which are stored outside of allocations, for example small
/// integers and booleans. These types are small enough that they don't need to be boxed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Immediate {
    Int(i64),
    Bool(bool),
    Nil,
}

/// A pointer value consists of a tag, providing type information about a boxed value, and of
/// course the location of the value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pointer<T>(pub Option<T>, pub usize);

/// "Values" are words which represent runtime values. This is opposed to headers and "raw" words
/// which represent no structured data.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Value<T> {
    Immediate(Immediate),
    Pointer(Pointer<T>),
}

/// `UnpackedWord` also includes a `Raw` variant; this is because `UnpackedWord` is a valid `Word`
/// for running the VM with, and without a `Raw` variant `UnpackedWord` represents only valid words
/// which cannot be interpreted safely as raw data. This also means that when running the VM with
/// `UnpackedWord`s instead of packed 32-bit or 64-bit words, we can detect misinterpretation of
/// values as raw data and vice versa.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Unpacked<T> {
    Value(Value<T>),
    Header(Header<T>),

    /// A "raw" word is "just data", used for defining builtins and primitives in the VM; for
    /// example, in an allocated vector, the first word of the allocation might be a raw length.
    /// This means that it is encoded directly as an integer rather than as an immediate value.
    Raw(u64),
}

#[derive(Debug, Clone)]
pub enum WordIndex {
    Word(usize),
    WordSlice(Range<usize>),
}

pub trait Layout<'a, W: Word>: Sized {
    type ViewMut: AsRef<[W]> + AsMut<[W]>;

    type Words: IntoIterator<Item = &'a [W]>;
    type WordsMut: IntoIterator<Item = &'a mut [W]>;
    type WordIndices: IntoIterator<Item = WordIndex>;

    fn view_mut(&self, idx: usize, heap: &'a mut Heap<W>) -> Self::ViewMut;

    fn words(&self, idx: usize, heap: &'a Heap<W>) -> Self::Words;
    fn words_mut(&self, idx: usize, heap: &'a mut Heap<W>) -> Self::WordsMut;
    fn word_indices(&self, idx: usize, heap: &Heap<W>) -> Self::WordIndices;
}

pub trait Type<W: Word>: for<'a> Layout<'a, W> {
    fn check(pointer: Pointer<W::Tag>, heap: &Heap<W>) -> Result<Self, HeapError>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Address<W: Word, Of: Type<W>>(usize, Of, PhantomData<W>);

#[derive(Debug, Clone)]
pub struct Heap<W: Word> {
    words: Vec<W>,
    top: usize,
}

impl<W: Word> Heap<W> {
    pub fn bump(&mut self, len: usize) -> Result<usize, HeapError> {
        if self.top + len <= self.words.len() {
            let offset = self.top;
            self.top += len;
            Ok(offset)
        } else {
            Err(HeapError::OutOfMemory)
        }
    }

    pub fn alloc_raw(&mut self, words: &[W]) -> Result<usize, HeapError> {
        let start = self.bump(words.len())?;
        self.words[start..self.top].copy_from_slice(words);
        Ok(start)
    }

    pub fn address<'a, Of: Type<W>>(
        &'a self,
        addr: Pointer<W::Tag>,
    ) -> Result<Address<W, Of>, HeapError> {
        let of = Of::check(addr, self)?;
        Ok(Address(addr.1, of, PhantomData))
    }

    pub fn get_mut<'a, Of: Type<W>>(
        &'a mut self,
        addr: Address<W, Of>,
    ) -> <Of as Layout<'a, W>>::ViewMut {
        addr.1.view_mut(addr.0, self)
    }

    pub fn words(&self) -> &[W] {
        &self.words
    }

    pub fn words_mut(&mut self) -> &mut [W] {
        &mut self.words
    }
}

#[derive(Debug)]
pub struct Scan<'a, W: Word> {
    from: &'a mut Heap<W>,
    to: &'a mut Heap<W>,

    scan: usize,
}

impl<'a, W: Word> Scan<'a, W> {
    pub fn move_object<T: Type<W>>(&mut self, addr: usize, ty: T) -> Result<usize, HeapError> {
        let new_addr = {
            let mut view = ty.view_mut(addr, &mut self.from);
            let slice = view.as_mut();
            self.to.alloc_raw(slice)?
        };
        self.from.words[addr] = W::pack(Unpacked::Header(Header::Moved(new_addr)));

        Ok(new_addr)
    }

    pub fn scan_pointer(&mut self, ptr: Pointer<W::Tag>) -> Result<Pointer<W::Tag>, HeapError> {
        let maybe_header = self.from
            .words()
            .get(ptr.1)
            .ok_or(HeapError::OutOfBounds)?
            .header();

        match maybe_header {
            Some(Header::Moved(offset)) => Ok(Pointer(ptr.0, offset)),
            Some(Header::Environment) => self.move_object(ptr.1, OfEnvironment)
                .map(|addr| Pointer(None, addr)),
            Some(Header::Closure) => self.move_object(ptr.1, OfClosure)
                .map(|addr| Pointer(None, addr)),
            Some(Header::Tag(tag)) => {
                assert!(ptr.0.is_none());
                self.move_object(ptr.1, tag).map(|addr| Pointer(None, addr))
            }
            None => {
                assert!(ptr.0.is_some());
                self.move_object(ptr.1, ptr.0.unwrap())
                    .map(|addr| Pointer(ptr.0, addr))
            }
        }
    }

    pub fn scan_indices<I: IntoIterator<Item = usize>>(
        &mut self,
        indices: I,
    ) -> Result<(), HeapError> {
        for idx in indices {
            if let Unpacked::Value(Value::Pointer(ptr)) = self.to.words[idx].unpack() {
                self.to.words[idx] =
                    W::pack(Unpacked::Value(Value::Pointer(self.scan_pointer(ptr)?)));
            }
        }

        Ok(())
    }

    pub fn scan_header<T: Type<W>>(&mut self, hdr: T) -> Result<usize, HeapError> {
        let len = hdr.view_mut(self.scan, &mut self.to).as_ref().len();
        let indices = hdr.word_indices(self.scan, &self.to);

        for word_index in indices {
            match word_index {
                WordIndex::Word(i) => self.scan_indices(iter::once(i))?,
                WordIndex::WordSlice(r) => self.scan_indices(r)?,
            }
        }

        Ok(len)
    }

    pub fn scan_object(&mut self) -> Result<usize, HeapError> {
        match self.to.words[self.scan].unpack() {
            Unpacked::Value(value) => {
                if let Value::Pointer(ptr) = value {
                    self.to.words[self.scan] =
                        W::pack(Unpacked::Value(Value::Pointer(self.scan_pointer(ptr)?)));
                }

                Ok(1)
            }
            Unpacked::Header(hdr) => match hdr {
                Header::Moved(_) => unreachable!("`Moved` in to-space during collection"),
                Header::Environment => self.scan_header(OfEnvironment),
                Header::Closure => self.scan_header(OfClosure),
                Header::Tag(tag) => self.scan_header(tag),
            },
            Unpacked::Raw(_) => Err(HeapError::BadWord),
        }
    }

    pub fn go(mut self) -> Result<(), HeapError> {
        while self.scan < self.to.top {
            self.scan += self.scan_object()?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn environment_needs_header() {
        assert!(OfEnvironment::NEEDS_HEADER);
    }

    fn closure_needs_header() {
        assert!(OfClosure::NEEDS_HEADER);
    }

    layout! {
        type Tag = Tag;

        type ViewMut = ViewMut;

        type Words = Words;
        type WordsMut = WordsMut;

        type WordIndices = WordIndices;

        where {
            struct Cons {
                head: Word,
                tail: Word,
            }
        }
    }
}
