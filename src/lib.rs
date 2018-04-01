#![feature(const_fn, plugin, trace_macros)]
#![plugin(interpolate_idents)]

#[macro_use]
extern crate failure;

mod heap;
// mod insn;
// mod machine;
// mod program;
mod word;
