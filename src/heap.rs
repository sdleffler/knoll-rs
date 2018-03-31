use std::ops::{Index, IndexMut, Range};

#[derive(Debug, Fail)]
pub enum HeapError {
    #[fail(display = "no header found for untagged allocation")]
    Headerless,

    #[fail(display = "access out of bounds")]
    OutOfBounds,

    #[fail(display = "out of memory")]
    OutOfMemory,
}

/// Trait for types usable as "machine words" for the VM.
pub trait Word: Copy + Eq + 'static {
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
    /// [env, n, w0, w1 ..., wn].len() == n + 3
    /// ```
    Environment,

    /// A closure consists of a pointer to an environment and a "function template", encoded as raw
    /// data. The function template is an index into the program's registry of function templates.
    ///
    /// ```
    /// [env, template].len() == 3
    /// ```
    Closure,

    /// A cons-cell consists of two words, representing the head and tail of a linked list. The two
    /// fields of the cons pair are also often known as "car" and "cdr".
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
