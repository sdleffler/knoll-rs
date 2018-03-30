use failure::*;

/// Trait for types usable as "machine words" for the VM.
pub trait Word: Default + Copy + Eq + 'static {
    type Err: Fail;

    /// Attempt to unpack a "packed" word. This may fail if the word was packed from a raw value,
    /// *or* it may return a garbage value.
    ///
    /// Whether or not unpacking a raw word is an error or not is defined by the "packed" word
    /// type.
    fn unpack(self) -> Result<UnpackedWord, Self::Err>;
    fn pack(UnpackedWord) -> Self;

    /// Try to interpret the word as a raw value. This might fail in conditions like debug mode,
    /// where raw words are safely encoded and can be detected.
    fn raw(self) -> Option<u64>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Header {
    /// A header indicating that the allocation here has been moved to another heap. This header
    /// also contains the pointer that the allocation was moved to.
    Moved(usize),

    /// An "environment" on the heap, the runtime representation of a lexical environment. It
    /// consists of a pointer to the parent environment, followed by the number of values stored in
    /// the environment, and then that many words where each word is one of said values. If the
    /// environment has no pointer, then the parent pointer points back to the environment header.
    ///
    /// ```
    /// [Header::Environment, env, n, w0, w1 ..., wn].len() == n + 3
    /// ```
    Environment,

    /// A closure consists of a pointer to an environment and a "function template", encoded as raw
    /// data. The function template is an index into the program's registry of function templates.
    ///
    /// ```
    /// [Header::Closure, env, template].len() == 3
    /// ```
    Closure,
}

/// An "immediate" value is one which does not need to be heap allocated.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Immediate {
    Int(i32),
    Bool(bool),
    Nil,
}

/// A pointer tag. This is similar to a header, but accompanies a pointer rather than being pointed
/// to. It allows for elimination of some allocation headers, since the type information can be
/// included alongside the pointer instead of stored with the allocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tag {
    Cons,
}

/// Words come in four flavors: headers, immediate values, tagged pointer values, and untagged
/// pointer values. Headers are found on the heap, and are pointed to by untagged pointers; in
/// general, the assumption is that if you have an untagged pointer, it points to a header, but if
/// you have a tagged pointer, it points directly to data; the header has been moved to the tag, if
/// you will.
///
/// `UnpackedWord` also includes a `Raw` variant; this is because `UnpackedWord` is a valid `Word`
/// for running the VM with, and without a `Raw` variant `UnpackedWord` represents only valid words
/// which cannot be interpreted safely as raw data. This also means that when running the VM with
/// `UnpackedWord`s instead of packed 32-bit or 64-bit words, we can detect misinterpretation of
/// values as raw data and vice versa.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnpackedWord {
    /// Headers are found preceding allocations. They provide information on what sort of data is
    /// allocated following them, providing type information to the runtime.
    Header(Header),

    /// Immediate values are values which are stored outside of allocations, for example small
    /// integers and booleans. These types are small enough that they don't need to be boxed.
    Immediate(Immediate),

    /// A "tagged" pointer does not point to a header; instead, what would otherwise be header data
    /// is inferrable from its tag.
    TaggedPointer(Tag, usize),

    /// An "untagged" pointer points to a header and its following allocation.
    UntaggedPointer(usize),

    /// A "raw" word is "just data", used for defining builtins and primitives in the VM. This is
    /// typically unused unless running the VM in debug mode, as it cannot typically be inferred
    /// when data is raw or not.
    Raw(u64),
}
