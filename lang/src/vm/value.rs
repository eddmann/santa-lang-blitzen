use im_rc::{HashMap, HashSet, Vector};
use ordered_float::OrderedFloat;
use std::cell::RefCell;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use super::bytecode::CompiledFunction;

/// Runtime value type for santa-lang
/// Per LANG.txt §3 Type System - 10 types total
#[derive(Clone, Debug)]
pub enum Value {
    /// §3.10 Nil - absence of a value
    Nil,

    /// §3.1 Integer - 64-bit signed
    Integer(i64),

    /// §3.2 Decimal - 64-bit floating point (IEEE 754)
    Decimal(OrderedFloat<f64>),

    /// §3.4 Boolean - true/false (note: §3.4 in LANG.txt refers to Range, Boolean is mentioned in §2.5)
    Boolean(bool),

    /// §3.3 String - UTF-8, grapheme-cluster indexed
    String(Rc<String>),

    /// §3.5 List - ordered, heterogeneous, persistent
    List(Vector<Value>),

    /// §3.6 Set - unordered, unique elements, persistent
    Set(HashSet<Value>),

    /// §3.7 Dictionary - unordered key-value mappings, persistent
    Dict(HashMap<Value, Value>),

    /// §3.9 Function - first-class functions
    Function(Rc<Closure>),

    /// §3.8 Lazy Sequence - infinite sequences with deferred computation
    LazySequence(Rc<RefCell<LazySeq>>),

    /// §3.4 Range - lazy sequence of integers
    Range {
        start: i64,
        end: Option<i64>,
        inclusive: bool,
    },

    /// External function (e.g., puts, read, env) - not part of LANG.txt spec
    /// but needed for CLI runtime (Phase 18)
    ExternalFunction(String),

    /// Partially applied function - stores original closure and captured arguments
    /// Used for auto-currying when a function is called with fewer args than its arity
    PartialApplication {
        closure: Rc<Closure>,
        args: Vec<Value>,
    },

    // TODO: Add MemoizedFunction variant for memoize builtin (Phase 14+)
    // See PLAN.md "Future Work: memoize Implementation" for details
    // Will require:
    //   MemoizedFunction(Rc<RefCell<MemoizedFunction>>),
    // Where MemoizedFunction contains: function + HashMap<Vec<Value>, Value> cache
}

/// Closure wraps a compiled function with its captured upvalues
#[derive(Debug)]
pub struct Closure {
    pub function: Rc<CompiledFunction>,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>,
}

/// Upvalue for capturing variables in closures
/// §8.3 Closures - capture variables from enclosing scopes
#[derive(Debug)]
pub enum Upvalue {
    /// Variable is still on the stack
    Open(usize),
    /// Variable has been closed over (moved to heap)
    Closed(Value),
}

/// §3.8 Lazy Sequence variants
#[derive(Debug)]
pub enum LazySeq {
    /// Unbounded range with current position
    Range {
        current: i64,
        end: Option<i64>,
        inclusive: bool,
    },
    /// repeat(value) - infinite repetition
    Repeat { value: Value },
    /// cycle(collection) - cycle through collection
    Cycle { source: Vector<Value>, index: usize },
    /// iterate(fn, initial) - apply fn repeatedly
    /// generator is stored as Value to support both Function and PartialApplication
    Iterate {
        generator: Value,
        current: Value,
    },
    /// map over lazy sequence
    Map {
        source: Rc<RefCell<LazySeq>>,
        mapper: Rc<Closure>,
    },
    /// filter lazy sequence
    Filter {
        source: Rc<RefCell<LazySeq>>,
        predicate: Rc<Closure>,
    },
    /// skip n elements
    Skip {
        source: Rc<RefCell<LazySeq>>,
        remaining: usize,
    },
    /// zip multiple sequences
    Zip { sources: Vec<Rc<RefCell<LazySeq>>> },
    /// combinations of elements
    Combinations {
        source: Vec<Value>,
        size: usize,
        indices: Vec<usize>,
    },
    /// range(from, to, step) - custom step range
    RangeStep {
        current: i64,
        end: i64,
        step: i64,
    },
    /// Exhausted sequence
    Empty,
}

impl Value {
    /// §14.1 Truthy and Falsy Values
    /// Returns whether the value is considered truthy
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            Value::Integer(n) => *n != 0,
            Value::Decimal(n) => n.0 != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::List(v) => !v.is_empty(),
            Value::Set(s) => !s.is_empty(),
            Value::Dict(d) => !d.is_empty(),
            // Functions, LazySequences, ExternalFunctions, and PartialApplications are always truthy
            Value::Function(_) => true,
            Value::LazySequence(_) => true,
            Value::ExternalFunction(_) => true,
            Value::PartialApplication { .. } => true,
            Value::Range { .. } => true,
        }
    }

    /// Returns the type name for error messages
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Nil => "Nil",
            Value::Integer(_) => "Integer",
            Value::Decimal(_) => "Decimal",
            Value::Boolean(_) => "Boolean",
            Value::String(_) => "String",
            Value::List(_) => "List",
            Value::Set(_) => "Set",
            Value::Dict(_) => "Dictionary",
            Value::Function(_) => "Function",
            Value::LazySequence(_) => "LazySequence",
            Value::Range { .. } => "Range",
            Value::ExternalFunction(_) => "ExternalFunction",
            Value::PartialApplication { .. } => "Function",
        }
    }

    /// §3.11 Hashability - check if a value can be used as Set element or Dict key
    pub fn is_hashable(&self) -> bool {
        match self {
            Value::Nil | Value::Integer(_) | Value::Decimal(_) | Value::Boolean(_) => true,
            Value::String(_) => true,
            Value::List(elements) => elements.iter().all(|e| e.is_hashable()),
            Value::Set(_) => true,
            // Dict, LazySequence, Function, ExternalFunction, PartialApplication are NOT hashable
            Value::Dict(_) | Value::LazySequence(_) | Value::Function(_)
            | Value::ExternalFunction(_) | Value::PartialApplication { .. } => false,
            // Ranges are hashable (they're just data)
            Value::Range { .. } => true,
        }
    }
}

/// Equality for Values - structural equality
/// §4.3 Equality Operators
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Decimal(a), Value::Decimal(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Set(a), Value::Set(b)) => a == b,
            (Value::Dict(a), Value::Dict(b)) => a == b,
            (
                Value::Range {
                    start: s1,
                    end: e1,
                    inclusive: i1,
                },
                Value::Range {
                    start: s2,
                    end: e2,
                    inclusive: i2,
                },
            ) => s1 == s2 && e1 == e2 && i1 == i2,
            // Functions compare by identity (Rc pointer equality)
            (Value::Function(a), Value::Function(b)) => Rc::ptr_eq(a, b),
            // LazySequences compare by identity
            (Value::LazySequence(a), Value::LazySequence(b)) => Rc::ptr_eq(a, b),
            // External functions compare by name
            (Value::ExternalFunction(a), Value::ExternalFunction(b)) => a == b,
            // Partial applications compare by closure identity and args equality
            (
                Value::PartialApplication { closure: c1, args: a1 },
                Value::PartialApplication { closure: c2, args: a2 },
            ) => Rc::ptr_eq(c1, c2) && a1 == a2,
            // Different types are never equal
            _ => false,
        }
    }
}

impl Eq for Value {}

/// Hash implementation for Values
/// §3.11 Hashability rules
impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Include type discriminant in hash
        std::mem::discriminant(self).hash(state);

        match self {
            Value::Nil => {}
            Value::Integer(n) => n.hash(state),
            Value::Decimal(n) => n.hash(state),
            Value::Boolean(b) => b.hash(state),
            Value::String(s) => s.hash(state),
            Value::List(elements) => {
                for elem in elements {
                    elem.hash(state);
                }
            }
            Value::Set(s) => {
                // Hash the length and sorted elements for deterministic hash
                s.len().hash(state);
                // Note: im_rc::HashSet iteration order may vary,
                // but for equality checking this is fine
                for elem in s {
                    elem.hash(state);
                }
            }
            Value::Range {
                start,
                end,
                inclusive,
            } => {
                start.hash(state);
                end.hash(state);
                inclusive.hash(state);
            }
            // Non-hashable types use pointer identity
            Value::Dict(d) => {
                // Hash by pointer for Dict (used in error cases)
                std::ptr::hash(d as *const _, state);
            }
            Value::Function(f) => {
                std::ptr::hash(Rc::as_ptr(f), state);
            }
            Value::LazySequence(s) => {
                std::ptr::hash(Rc::as_ptr(s), state);
            }
            Value::ExternalFunction(name) => {
                name.hash(state);
            }
            Value::PartialApplication { closure, args } => {
                std::ptr::hash(Rc::as_ptr(closure), state);
                for arg in args {
                    arg.hash(state);
                }
            }
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Integer(n) => write!(f, "{n}"),
            Value::Decimal(n) => {
                // Ensure we display at least one decimal place
                if n.0.fract() == 0.0 {
                    write!(f, "{}.0", n.0 as i64)
                } else {
                    write!(f, "{}", n.0)
                }
            }
            Value::Boolean(b) => write!(f, "{b}"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::List(elements) => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, "]")
            }
            Value::Set(elements) => {
                write!(f, "{{")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, "}}")
            }
            Value::Dict(entries) => {
                write!(f, "#{{")?;
                for (i, (key, value)) in entries.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key}: {value}")?;
                }
                write!(f, "}}")
            }
            Value::Function(_) => write!(f, "<function>"),
            Value::LazySequence(_) => write!(f, "<lazy-sequence>"),
            Value::ExternalFunction(name) => write!(f, "<external-function:{}>", name),
            Value::PartialApplication { .. } => write!(f, "<function>"),
            Value::Range {
                start,
                end,
                inclusive,
            } => match (end, inclusive) {
                (Some(e), false) => write!(f, "{start}..{e}"),
                (Some(e), true) => write!(f, "{start}..={e}"),
                (None, _) => write!(f, "{start}.."),
            },
        }
    }
}
