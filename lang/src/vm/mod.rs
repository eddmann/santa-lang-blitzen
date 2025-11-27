pub mod builtins;
pub mod bytecode;
pub mod compiler;
pub mod runtime;
pub mod value;

// Re-export commonly used types
pub use runtime::{RuntimeError, VM};
pub use value::Value;

#[cfg(test)]
mod tests;
