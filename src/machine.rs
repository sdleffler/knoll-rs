use insn::{Insn, UnpackedInsn};
use word::{Word, UnpackedWord};

#[derive(Debug, Fail)]
pub enum ExecError {
    #[fail(display = "machine halted by instruction")]
    Halt,
}

/// Trait for types usable as instructions.
pub trait Insn: Copy + Eq + 'static {
    type Hook;

    fn unpack(self) -> UnpackedInsn;
    fn pack(UnpackedInsn) -> Self;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Location {
    /// Local variables are registers as far as the VM is concerned.
    Local { scope: usize, index: usize },

    /// Accumulator registers are faster to access than local variables but do not necessarily
    /// persist across function calls.
    Register(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Math {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnpackedInsn<H> {
    /// Stop. Drop. Roll.
    Halt,
    
    /// Load a constant from the current template into a location.
    Load(usize, Location),

    /// Move a value from one location into another.
    Move(Location, Location),

    /// Perform an arithmetic operation on the builtin "small integer" type.
    Math(Math, Location, Location),

    /// "Hooked" instructions are non-builtins.
    Hook(H),
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
    // program: Arc<Program<W, I>>,
    state: State,
    heap: ProcessHeap<W>,
}

// impl<W: Word, I: Insn> Machine<W, I> {
//     fn fetch(&self) -> I {
//         self.program.code()[self.registers.pc]
//     }
// 
//     pub fn local(&self, scope: usize, index: usize) -> Result<&W, ExecError> {
//         let mut e = self.registers.env;
//         while scope > 0 {
//         }
//     }
// 
//     pub fn step(&mut self) -> Result<(), ExecError> {
//         use self::UnpackedInsn::*;
// 
//         match self.fetch().unpack() {
//             Halt => Err(ExecError::Halt),
//             _ => unimplemented!(),
//         }
//     }
// }
