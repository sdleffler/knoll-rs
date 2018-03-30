#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    Environment,
    Template,
    Local { scope: usize, index: usize },
    Accumulator(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Location {
    Register(Register),
    Address(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnpackedInsn {
    /// Load a constant from the current template into a location.
    Load(usize, Location),

    /// Move a value from one location into another.
    Move(Location, Location),
}
