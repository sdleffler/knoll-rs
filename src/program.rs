use heap::MessageHeap;
use word::Word;

/// A function "template" contains a pointer to the function's code, along with a function-specific
/// pool of constants, stored as a single message heap.
#[derive(Debug)]
pub struct Template<W: Word> {
    label: usize,
    constants: MessageHeap<W>,
}

/// Programs have several important regions of data.
#[derive(Debug)]
pub struct Program<W: Word, I: Insn> {
    templates: Vec<Template<W>>,
    code: Vec<I>,
}

impl<W: Word, I: Insn> Program<W, I> {
    pub fn code(&self) -> &[I] {
        &self.code
    }
}
