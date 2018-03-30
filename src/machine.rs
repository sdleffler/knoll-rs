use insn::{Insn, UnpackedInsn};
use word::{Word, UnpackedWord};

#[derive(Debug, Fail)]
pub enum ExecError {
    #[fail(display = "machine halted by instruction")]
    Halt,
}

#[derive(Debug)]
pub struct Registers<W: Word> {
    /// The "environment" register.
    pub env: usize,

    /// The "template" register.
    pub tm: usize,

    /// The accumulator registers.
    pub accs: Vec<W>,

    /// The program counter (a.k.a. instruction pointer).
    pub pc: usize,
}

#[derive(Debug)]
pub struct Machine<W: Word, I: Insn> {
    registers: Registers<W>,
    program: Arc<Program<W, I>>,
    state: State,
    heap: ProcessHeap<W>,
}

impl<W: Word, I: Insn> Machine<W, I> {
    fn fetch(&self) -> I {
        self.program.code()[self.registers.pc]
    }

    pub fn local(&self, scope: usize, index: usize) -> Result<&W, ExecError> {
        let mut e = self.registers.env;
        while scope > 0 {
        }
    }

    pub fn step(&mut self) -> Result<(), ExecError> {
        use self::UnpackedInsn::*;

        match self.fetch().unpack() {
            Halt => Err(ExecError::Halt),
            _ => unimplemented!(),
        }
    }
}
