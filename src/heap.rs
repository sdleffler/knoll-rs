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

    pub fn words(&self) -> &[W] {
        &self.words
    }

    pub fn words_mut(&mut self) -> &mut [W] {
        &mut self.words
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Address(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AddressRange(usize, usize);

#[macro_export]
macro_rules! __layout_offset {
    (@each $heap:ident $addr:ident => $field:ident: Word) => { { $addr += 1; } };
    (@each $heap:ident $addr:ident => $field:ident: Raw) => { { $addr += 1; } };
    (@each $heap:ident $addr:ident => $field:ident: [Word]) => { {
            $addr += $heap.words().get($addr).unwrap().raw().unwrap() as usize;
    } };
    (@each $heap:ident $addr:ident => $field:ident: [Raw]) => { {
            $addr += $heap.words().get($addr).unwrap().raw().unwrap() as usize;
    } };
    ($heap:ident, $addr:ident => $($prev_field:ident: $prev_type:tt)*) => {
        {
            #[allow(unused_mut)]
            let mut addr = $addr;
            $(__layout_offset!(@each $heap addr => $prev_field: $prev_type);)*
            addr
        }
    };
}

#[macro_export]
macro_rules! __layout_access {
    ([$($suffix:tt)*] $heap:ident, $addr:ident => $t:ident) => {{
        interpolate_idents! {
            $heap
                .[ words $($suffix)* ]()
                .[ get $($suffix)* ]($addr)
                .unwrap()
        }
    }};
    ([$($suffix:tt)*] $heap:ident, $addr:ident => [$t:ident]) => {{
        let len = $heap.words().get($addr).unwrap().raw().unwrap() as usize;
        interpolate_idents! {
            $heap
                .[ words $($suffix)* ]()
                .[ get $($suffix)* ]($addr + 1..$addr + 1 + len)
                .unwrap()
        }
    }};
}

#[macro_export]
macro_rules! __layout_needs_header {
    (@check Word) => { false };
    (@check Raw) => { true };
    (@check [Word]) => { true };
    (@check [Raw]) => { true };

    ($($type:tt)*) => { { true $(|| __layout_needs_header!(@check $type))* } };
}

#[macro_export]
macro_rules! __layout_type {
    ($var:ident => Word) => {
        $var
    };
    ($var:ident => Raw) => {
        $var
    };
    ($var:ident => [Word]) => {
        [$var]
    };
    ($var:ident => [Raw]) => {
        [$var]
    };
}

#[macro_export]
macro_rules! __layout_field {
    ($name:ident { $($prev_field:ident: $prev_type:tt)* => $field:ident : $type:tt }) => {
        impl<'a, W: Word> $name<'a, W> {
            pub fn $field(&'a self) -> &'a __layout_type!(W => $type) {
                let heap = &*self.heap;
                let addr = self.offset;
                __layout_offset!(heap, addr => $($prev_field : $prev_type)*);
                __layout_access!([] heap, addr => $type)
            }

            interpolate_idents! {
                pub fn [ $field _mut ] (&'a mut self) -> &'a mut __layout_type!(W => $type) {
                    let heap = &mut *self.heap;
                    let addr = self.offset;
                    __layout_offset!(heap, addr => $($prev_field : $prev_type)*);
                    __layout_access!([[_mut]] heap, addr => $type)
                }
            }
        }
    };
}

#[macro_export]
macro_rules! __layout_folded_fields {
    ($name:ident $($fold:tt)*) => {
        $(__layout_field!($name $fold);)*
    };
}

#[macro_export]
macro_rules! __layout_fold {
    (@rec $k:tt [$($prev_field:ident : $prev_type:tt)*] [$($agg:tt)*] $field:ident : $type:tt $($rest:tt)*) => {
        __layout_fold!(@rec
            $k
            [$($prev_field: $prev_type)* $field: $type]
            [$($agg)* { $($prev_field : $prev_type)* => $field : $type }]
            $($rest)*
        );
    };
    (@rec ($k:ident!($($kargs:tt)*)) [$($field:ident : $type:tt)*] [$($agg:tt)*]) => {
        $k!($($kargs)* $($agg)*);
    };
    ($k:ident!($($kargs:tt)*) $($field:ident : $type:tt)*) => { __layout_fold!(@rec ($k!($($kargs)*)) [] [] $($field : $type)*); };
}

#[macro_export]
macro_rules! __layout_struct {
    (struct $name:tt { $($field:ident : $type:tt)* }) => {
        pub struct $name<'a, W: Word> {
            offset: usize,
            heap: &'a mut Heap<W>,
        }
        
        __layout_fold!(__layout_folded_fields!($name) $($field : $type)*);

        impl<'a, W: Word> $name<'a, W> {
            pub fn as_slice(&'a self) -> &'a [W] {
                let heap = &*self.heap;
                let start = self.offset;
                let end = __layout_offset!(heap, start => $($field : $type)*);
                heap.words().get(start..end).unwrap()
            }

            pub fn as_mut_slice(&'a mut self) -> &'a mut [W] {
                let heap = &mut *self.heap;
                let start = self.offset;
                let end = __layout_offset!(heap, start => $($field : $type)*);
                heap.words_mut().get_mut(start..end).unwrap()
            }
        }
    };
}

#[macro_export]
macro_rules! __layout_def {
    (type Tag = $tag:ident; where { $(struct $name:ident $innards:tt)* }) => {
        pub enum $tag {
            $($name,)*
        }
    };
}

#[macro_export]
macro_rules! __layout_splat_defs {
    ($(type $foo:ident = $bar:ident;)* where $stuff:tt) => {
        $(__layout_def!(type $foo = $bar; where $stuff);)*
    };
}

#[macro_export]
macro_rules! layout {
    ($(type $foo:ident = $bar:ident;)* where { $(struct $name:ident { $($field:ident : $type:tt),* $(,)* })+ }) => {
        __layout_splat_defs!($(type $foo = $bar;)* where { $(struct $name { $($field : $type)* })* });
        $(__layout_struct!(struct $name { $($field : $type)* });)*
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    mod foo {
        use super::*;

        trace_macros!(true);
        layout! {
            type Tag = Tag;

            where {
                struct Environment {
                    parent: Word,
                    locals: [Word],
                }

                struct Closure {
                    environment: Word,
                    template: Raw,
                }

                struct Cons {
                    head: Word,
                    tail: Word,
                }
            }
        }
    }
}
