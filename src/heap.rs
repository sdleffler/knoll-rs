use std::{marker::PhantomData, ops::{Index, IndexMut, Range}};

#[derive(Debug, Fail)]
pub enum HeapError {
    #[fail(display = "no header found for untagged allocation")]
    Headerless,

    #[fail(display = "access out of bounds")]
    OutOfBounds,

    #[fail(display = "out of memory")]
    OutOfMemory,

    #[fail(display = "attempt to stare into the void")]
    Cthulhu,
}

/// Trait for types usable as "machine words" for the VM.
pub trait Word: Default + Copy + Eq + 'static {
    fn pack(Unpacked) -> Self;

    /// Try to interpret the word as a value.
    fn value(self) -> Option<Value>;

    /// Try to interpret the word as a header. This might fail in conditions like debug mode, where
    /// headers are distinguishable from other values.
    fn header(self) -> Option<Header>;

    /// Try to interpret the word as a raw value. This might fail in conditions like debug mode,
    /// where raw words are safely encoded and can be detected.
    fn raw(self) -> Option<u64>;
}

/// Tags indicate types of data. They are often found in the form of heap headers, or extracted
/// from values embedded in pointers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tag {
    /// An "environment" on the heap, the runtime representation of a lexical environment. It
    /// consists of a pointer to the parent environment, followed by the number of values stored in
    /// the environment, and then that many words where each word is one of said values. If the
    /// environment has no pointer, then the parent pointer points back to the environment header.
    ///
    /// ```
    /// [env, n, w0, w1 ..., wn].len() == n + 2
    /// ```
    Environment,

    /// A closure consists of a pointer to an environment and a "function template", encoded as raw
    /// data. The function template is an index into the program's registry of function templates.
    ///
    /// ```
    /// [env, template].len() == 2
    /// ```
    Closure,

    /// A cons-cell consists of two words, representing the head and tail of a linked list. The two
    /// fields of the cons pair are also often known as "car" and "cdr".
    ///
    /// ```
    /// [head, tail].len() == 2
    /// ```
    Cons,
}

/// Headers may be found preceding allocations. They provide information on what sort of data
/// is allocated following them, providing type information to the runtime.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Header {
    /// A header indicating that the allocation here has been moved to another heap. This header
    /// also contains the pointer that the allocation was moved to.
    Moved(usize),

    /// A header indicating the type of an allocation.
    Tag(Tag),
}

/// Immediate values are values which are stored outside of allocations, for example small
/// integers and booleans. These types are small enough that they don't need to be boxed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Immediate {
    Int(i32),
    Bool(bool),
    Nil,
}

/// A pointer value consists of a tag, providing type information about a boxed value, and of
/// course the location of the value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pointer(pub Option<Tag>, pub usize);

/// "Values" are words which represent runtime values. This is opposed to headers and "raw" words
/// which represent no structured data.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Value {
    Immediate(Immediate),
    Pointer(Pointer),
}

/// `UnpackedWord` also includes a `Raw` variant; this is because `UnpackedWord` is a valid `Word`
/// for running the VM with, and without a `Raw` variant `UnpackedWord` represents only valid words
/// which cannot be interpreted safely as raw data. This also means that when running the VM with
/// `UnpackedWord`s instead of packed 32-bit or 64-bit words, we can detect misinterpretation of
/// values as raw data and vice versa.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Unpacked {
    Value(Value),
    Header(Header),

    /// A "raw" word is "just data", used for defining builtins and primitives in the VM; for
    /// example, in an allocated vector, the first word of the allocation might be a raw length.
    /// This means that it is encoded directly as an integer rather than as an immediate value.
    Raw(u64),
}

#[derive(Debug, Clone)]
pub struct Heap<W: Word> {
    words: Vec<W>,
    top: usize,
}

impl<W: Word> Heap<W> {
    #[inline]
    pub fn alloc_raw<L: Layout<W>>(&mut self, size: usize) -> Result<Address<W, L>, HeapError> {
        if size + self.top <= self.words.len() {
            let addr = self.top;
            self.top += size;
            Ok(Address::from_offset(addr))
        } else {
            Err(HeapError::OutOfMemory)
        }
    }

    #[inline]
    pub fn alloc<L>(&mut self) -> Result<Address<W, L>, HeapError>
    where
        L: Layout<W, Args = ()>,
    {
        L::alloc(self, ())
    }

    #[inline]
    pub fn alloc_with<L>(&mut self, args: L::Args) -> Result<Address<W, L>, HeapError>
    where
        L: Layout<W>,
    {
        L::alloc(self, args)
    }

    #[inline]
    pub fn get_mut<'a, L>(
        &'a mut self,
        address: Address<W, L>,
    ) -> Result<<L as Reify<'a, W>>::Reified, HeapError>
    where
        L: Layout<W>,
    {
        L::reify(self, address.0)
    }

    #[inline]
    pub fn words(&self) -> &[W] {
        &self.words
    }

    #[inline]
    pub fn words_mut(&mut self) -> &mut [W] {
        &mut self.words
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Address<W: Word, L>(usize, PhantomData<(W, L)>)
where
    L: Layout<W>;

impl<W: Word, L: Layout<W>> Address<W, L> {
    #[inline]
    pub fn offset(self) -> usize {
        self.0
    }

    #[inline]
    pub fn from_offset(offset: usize) -> Self {
        Address(offset, PhantomData)
    }
}

pub trait Layout<W: Word>: Sized + for<'a> Reify<'a, W> {
    type Args;

    fn alloc(&mut Heap<W>, Self::Args) -> Result<Address<W, Self>, HeapError>;
}

pub trait Reify<'a, W: Word> {
    type Reified;

    fn reify(&'a mut Heap<W>, usize) -> Result<Self::Reified, HeapError>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Environment;

impl<W: Word> Layout<W> for Environment {
    type Args = usize;

    #[inline]
    fn alloc(heap: &mut Heap<W>, n_locals: usize) -> Result<Address<W, Self>, HeapError> {
        let addr = heap.alloc_raw(2 + n_locals)?;
        let words = heap.words_mut();
        let offset = addr.offset();
        words[offset + 1] = W::pack(Unpacked::Raw(n_locals as u64));
        Ok(addr)
    }
}

impl<'a, W: Word> Reify<'a, W> for Environment {
    type Reified = (&'a mut W, &'a mut [W]);

    #[inline]
    fn reify(heap: &'a mut Heap<W>, idx: usize) -> Result<Self::Reified, HeapError> {
        let words = heap.words_mut();
        let slice = words.get_mut(idx..).unwrap();
        let n_locals = slice.get_mut(1).unwrap().raw().unwrap() as usize;
        let (env, slice) = slice.split_first_mut().unwrap();
        let locals = slice.get_mut(2..2 + n_locals).unwrap();
        Ok((env, locals))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Closure;

impl<W: Word> Layout<W> for Closure {
    type Args = ();

    #[inline]
    fn alloc(heap: &mut Heap<W>, _args: ()) -> Result<Address<W, Self>, HeapError> {
        heap.alloc_raw(2)
    }
}

impl<'a, W: Word> Reify<'a, W> for Closure {
    type Reified = (&'a mut W, &'a mut W);

    #[inline]
    fn reify(heap: &'a mut Heap<W>, idx: usize) -> Result<Self::Reified, HeapError> {
        let slice = heap.words_mut().get_mut(idx..).unwrap();
        let (environment, slice) = slice.split_first_mut().unwrap();
        let template = slice.first_mut().unwrap();

        Ok((environment, template))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Cons;

impl<W: Word> Layout<W> for Cons {
    type Args = ();

    #[inline]
    fn alloc(heap: &mut Heap<W>, _args: ()) -> Result<Address<W, Self>, HeapError> {
        heap.alloc_raw(2)
    }
}

impl<'a, W: Word> Reify<'a, W> for Cons {
    type Reified = (&'a mut W, &'a mut W);

    #[inline]
    fn reify(heap: &'a mut Heap<W>, idx: usize) -> Result<Self::Reified, HeapError> {
        let slice = heap.words_mut().get_mut(idx..).unwrap();
        let (head, slice) = slice.split_first_mut().unwrap();
        let tail = slice.first_mut().unwrap();

        Ok((head, tail))
    }
}

#[derive(Debug, Clone)]
pub struct MessageHeap<W: Word> {
    heap: Heap<W>,
    index: Vec<W>,
}
