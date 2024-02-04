// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run
use derive_debug::CustomDebug;
use std::fmt::Debug;

pub trait Trait {
    type Value;
}

#[derive(CustomDebug)]
#[debug(bound = "T::Value: Debug")]
pub struct Wrapper<T: Trait> {
    field: Field<T>,
}

#[derive(CustomDebug)]
struct Field<T: Trait> {
    values: Vec<T::Value>,
}

fn main() {}
