/// Trait for types usable as instructions.
pub trait Insn: Copy + Eq + 'static {
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
pub enum UnpackedInsn {
    /// Stop. Drop. Roll.
    Halt,
    
    /// Load a constant from the current template into a location.
    Load(usize, Location),

    /// Move a value from one location into another.
    Move(Location, Location),
}
