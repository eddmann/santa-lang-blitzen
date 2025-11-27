//! Built-in functions for santa-lang
//!
//! Phase 9: Type conversion, collection access, and collection modification
//! Per LANG.txt §11.1-11.3

use im_rc::{HashMap, HashSet, Vector};
use ordered_float::OrderedFloat;
use regex::Regex;
use std::rc::Rc;
use unicode_segmentation::UnicodeSegmentation;

use super::runtime::RuntimeError;
use super::value::Value;

/// Built-in function ID enum for compile-time registration
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum BuiltinId {
    // Type conversion (§11.1)
    Int = 0,
    Ints = 1,
    List = 2,
    Set = 3,
    Dict = 4,

    // Collection access (§11.2)
    Get = 10,
    Size = 11,
    First = 12,
    Second = 13,
    Rest = 14,
    Keys = 15,
    Values = 16,

    // Collection modification (§11.3)
    Push = 20,
    Assoc = 21,
    Update = 22,
    UpdateD = 23,

    // Transformation (§11.4)
    Map = 30,
    Filter = 31,
    FlatMap = 32,
    FilterMap = 33,
    FindMap = 34,

    // Reduction (§11.5)
    Reduce = 40,
    Fold = 41,
    FoldS = 42,
    Scan = 43,

    // Iteration (§11.6)
    Each = 50,

    // Search (§11.7)
    Find = 60,
    Count = 61,

    // Aggregation (§11.8)
    Sum = 70,
    Max = 71,
    Min = 72,

    // Sequence Manipulation (§11.9)
    Skip = 80,
    Take = 81,
    Sort = 82,
    Reverse = 83,
    Rotate = 84,
    Chunk = 85,

    // Set Operations (§11.10)
    Union = 90,
    Intersection = 91,

    // Predicates (§11.11)
    Includes = 100,
    Excludes = 101,
    Any = 102,
    All = 103,
}

impl BuiltinId {
    /// Get the built-in function name
    pub fn name(self) -> &'static str {
        match self {
            BuiltinId::Int => "int",
            BuiltinId::Ints => "ints",
            BuiltinId::List => "list",
            BuiltinId::Set => "set",
            BuiltinId::Dict => "dict",
            BuiltinId::Get => "get",
            BuiltinId::Size => "size",
            BuiltinId::First => "first",
            BuiltinId::Second => "second",
            BuiltinId::Rest => "rest",
            BuiltinId::Keys => "keys",
            BuiltinId::Values => "values",
            BuiltinId::Push => "push",
            BuiltinId::Assoc => "assoc",
            BuiltinId::Update => "update",
            BuiltinId::UpdateD => "update_d",
            BuiltinId::Map => "map",
            BuiltinId::Filter => "filter",
            BuiltinId::FlatMap => "flat_map",
            BuiltinId::FilterMap => "filter_map",
            BuiltinId::FindMap => "find_map",
            BuiltinId::Reduce => "reduce",
            BuiltinId::Fold => "fold",
            BuiltinId::FoldS => "fold_s",
            BuiltinId::Scan => "scan",
            BuiltinId::Each => "each",
            BuiltinId::Find => "find",
            BuiltinId::Count => "count",
            BuiltinId::Sum => "sum",
            BuiltinId::Max => "max",
            BuiltinId::Min => "min",
            BuiltinId::Skip => "skip",
            BuiltinId::Take => "take",
            BuiltinId::Sort => "sort",
            BuiltinId::Reverse => "reverse",
            BuiltinId::Rotate => "rotate",
            BuiltinId::Chunk => "chunk",
            BuiltinId::Union => "union",
            BuiltinId::Intersection => "intersection",
            BuiltinId::Includes => "includes?",
            BuiltinId::Excludes => "excludes?",
            BuiltinId::Any => "any?",
            BuiltinId::All => "all?",
        }
    }

    /// Try to get a built-in by name
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "int" => Some(BuiltinId::Int),
            "ints" => Some(BuiltinId::Ints),
            "list" => Some(BuiltinId::List),
            "set" => Some(BuiltinId::Set),
            "dict" => Some(BuiltinId::Dict),
            "get" => Some(BuiltinId::Get),
            "size" => Some(BuiltinId::Size),
            "first" => Some(BuiltinId::First),
            "second" => Some(BuiltinId::Second),
            "rest" => Some(BuiltinId::Rest),
            "keys" => Some(BuiltinId::Keys),
            "values" => Some(BuiltinId::Values),
            "push" => Some(BuiltinId::Push),
            "assoc" => Some(BuiltinId::Assoc),
            "update" => Some(BuiltinId::Update),
            "update_d" => Some(BuiltinId::UpdateD),
            "map" => Some(BuiltinId::Map),
            "filter" => Some(BuiltinId::Filter),
            "flat_map" => Some(BuiltinId::FlatMap),
            "filter_map" => Some(BuiltinId::FilterMap),
            "find_map" => Some(BuiltinId::FindMap),
            "reduce" => Some(BuiltinId::Reduce),
            "fold" => Some(BuiltinId::Fold),
            "fold_s" => Some(BuiltinId::FoldS),
            "scan" => Some(BuiltinId::Scan),
            "each" => Some(BuiltinId::Each),
            "find" => Some(BuiltinId::Find),
            "count" => Some(BuiltinId::Count),
            "sum" => Some(BuiltinId::Sum),
            "max" => Some(BuiltinId::Max),
            "min" => Some(BuiltinId::Min),
            "skip" => Some(BuiltinId::Skip),
            "take" => Some(BuiltinId::Take),
            "sort" => Some(BuiltinId::Sort),
            "reverse" => Some(BuiltinId::Reverse),
            "rotate" => Some(BuiltinId::Rotate),
            "chunk" => Some(BuiltinId::Chunk),
            "union" => Some(BuiltinId::Union),
            "intersection" => Some(BuiltinId::Intersection),
            "includes?" => Some(BuiltinId::Includes),
            "excludes?" => Some(BuiltinId::Excludes),
            "any?" => Some(BuiltinId::Any),
            "all?" => Some(BuiltinId::All),
            _ => None,
        }
    }

    /// Get the expected arity (number of arguments)
    /// Returns (min_arity, max_arity) for variadic functions
    pub fn arity(self) -> (u8, u8) {
        match self {
            // Single argument functions
            BuiltinId::Int
            | BuiltinId::Ints
            | BuiltinId::List
            | BuiltinId::Set
            | BuiltinId::Dict
            | BuiltinId::Size
            | BuiltinId::First
            | BuiltinId::Second
            | BuiltinId::Rest
            | BuiltinId::Keys
            | BuiltinId::Values
            | BuiltinId::Sum
            | BuiltinId::Reverse => (1, 1),

            // Two argument functions: (fn, collection) or (index, collection)
            BuiltinId::Get
            | BuiltinId::Push
            | BuiltinId::Map
            | BuiltinId::Filter
            | BuiltinId::FlatMap
            | BuiltinId::FilterMap
            | BuiltinId::FindMap
            | BuiltinId::Reduce
            | BuiltinId::Each
            | BuiltinId::Find
            | BuiltinId::Count
            | BuiltinId::Skip
            | BuiltinId::Take
            | BuiltinId::Sort
            | BuiltinId::Rotate
            | BuiltinId::Chunk
            | BuiltinId::Includes
            | BuiltinId::Excludes
            | BuiltinId::Any
            | BuiltinId::All => (2, 2),

            // Three argument functions
            BuiltinId::Assoc
            | BuiltinId::Update
            | BuiltinId::Fold
            | BuiltinId::FoldS
            | BuiltinId::Scan => (3, 3),

            // Four argument functions
            BuiltinId::UpdateD => (4, 4),

            // Variadic functions
            BuiltinId::Max | BuiltinId::Min | BuiltinId::Union | BuiltinId::Intersection => (1, 255),
        }
    }

    /// Returns true if this builtin requires callback invocation
    pub fn requires_callback(self) -> bool {
        matches!(
            self,
            BuiltinId::Map
                | BuiltinId::Filter
                | BuiltinId::FlatMap
                | BuiltinId::FilterMap
                | BuiltinId::FindMap
                | BuiltinId::Reduce
                | BuiltinId::Fold
                | BuiltinId::FoldS
                | BuiltinId::Scan
                | BuiltinId::Each
                | BuiltinId::Update
                | BuiltinId::UpdateD
                | BuiltinId::Find
                | BuiltinId::Count
                | BuiltinId::Sort
                | BuiltinId::Any
                | BuiltinId::All
        )
    }
}

impl TryFrom<u16> for BuiltinId {
    type Error = u16;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(BuiltinId::Int),
            1 => Ok(BuiltinId::Ints),
            2 => Ok(BuiltinId::List),
            3 => Ok(BuiltinId::Set),
            4 => Ok(BuiltinId::Dict),
            10 => Ok(BuiltinId::Get),
            11 => Ok(BuiltinId::Size),
            12 => Ok(BuiltinId::First),
            13 => Ok(BuiltinId::Second),
            14 => Ok(BuiltinId::Rest),
            15 => Ok(BuiltinId::Keys),
            16 => Ok(BuiltinId::Values),
            20 => Ok(BuiltinId::Push),
            21 => Ok(BuiltinId::Assoc),
            22 => Ok(BuiltinId::Update),
            23 => Ok(BuiltinId::UpdateD),
            30 => Ok(BuiltinId::Map),
            31 => Ok(BuiltinId::Filter),
            32 => Ok(BuiltinId::FlatMap),
            33 => Ok(BuiltinId::FilterMap),
            34 => Ok(BuiltinId::FindMap),
            40 => Ok(BuiltinId::Reduce),
            41 => Ok(BuiltinId::Fold),
            42 => Ok(BuiltinId::FoldS),
            43 => Ok(BuiltinId::Scan),
            50 => Ok(BuiltinId::Each),
            60 => Ok(BuiltinId::Find),
            61 => Ok(BuiltinId::Count),
            70 => Ok(BuiltinId::Sum),
            71 => Ok(BuiltinId::Max),
            72 => Ok(BuiltinId::Min),
            80 => Ok(BuiltinId::Skip),
            81 => Ok(BuiltinId::Take),
            82 => Ok(BuiltinId::Sort),
            83 => Ok(BuiltinId::Reverse),
            84 => Ok(BuiltinId::Rotate),
            85 => Ok(BuiltinId::Chunk),
            90 => Ok(BuiltinId::Union),
            91 => Ok(BuiltinId::Intersection),
            100 => Ok(BuiltinId::Includes),
            101 => Ok(BuiltinId::Excludes),
            102 => Ok(BuiltinId::Any),
            103 => Ok(BuiltinId::All),
            _ => Err(value),
        }
    }
}

/// Execute a built-in function
/// Returns the result value or an error
pub fn call_builtin(id: BuiltinId, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
    let (min_arity, max_arity) = id.arity();
    if args.len() < min_arity as usize || args.len() > max_arity as usize {
        return Err(RuntimeError::new(
            format!(
                "{} expects {} argument(s), got {}",
                id.name(),
                if min_arity == max_arity {
                    min_arity.to_string()
                } else {
                    format!("{}-{}", min_arity, max_arity)
                },
                args.len()
            ),
            line,
        ));
    }

    match id {
        BuiltinId::Int => builtin_int(&args[0], line),
        BuiltinId::Ints => builtin_ints(&args[0], line),
        BuiltinId::List => builtin_list(&args[0], line),
        BuiltinId::Set => builtin_set(&args[0], line),
        BuiltinId::Dict => builtin_dict(&args[0], line),
        BuiltinId::Get => builtin_get(&args[0], &args[1], line),
        BuiltinId::Size => builtin_size(&args[0], line),
        BuiltinId::First => builtin_first(&args[0], line),
        BuiltinId::Second => builtin_second(&args[0], line),
        BuiltinId::Rest => builtin_rest(&args[0], line),
        BuiltinId::Keys => builtin_keys(&args[0], line),
        BuiltinId::Values => builtin_values(&args[0], line),
        BuiltinId::Push => builtin_push(&args[0], &args[1], line),
        BuiltinId::Assoc => builtin_assoc(&args[0], &args[1], &args[2], line),
        // Phase 11 builtins
        BuiltinId::Sum => builtin_sum(&args[0], line),
        BuiltinId::Max => builtin_max(args, line),
        BuiltinId::Min => builtin_min(args, line),
        BuiltinId::Skip => builtin_skip(&args[0], &args[1], line),
        BuiltinId::Take => builtin_take(&args[0], &args[1], line),
        BuiltinId::Reverse => builtin_reverse(&args[0], line),
        BuiltinId::Rotate => builtin_rotate(&args[0], &args[1], line),
        BuiltinId::Chunk => builtin_chunk(&args[0], &args[1], line),
        BuiltinId::Union => builtin_union(args, line),
        BuiltinId::Intersection => builtin_intersection(args, line),
        BuiltinId::Includes => builtin_includes(&args[0], &args[1], line),
        BuiltinId::Excludes => builtin_excludes(&args[0], &args[1], line),
        // Callback-based builtins - handled specially in runtime
        BuiltinId::Update
        | BuiltinId::UpdateD
        | BuiltinId::Map
        | BuiltinId::Filter
        | BuiltinId::FlatMap
        | BuiltinId::FilterMap
        | BuiltinId::FindMap
        | BuiltinId::Reduce
        | BuiltinId::Fold
        | BuiltinId::FoldS
        | BuiltinId::Scan
        | BuiltinId::Each
        | BuiltinId::Find
        | BuiltinId::Count
        | BuiltinId::Sort
        | BuiltinId::Any
        | BuiltinId::All => Err(RuntimeError::new(
            format!(
                "{} requires callback support - should be handled by VM",
                id.name()
            ),
            line,
        )),
    }
}

// ============================================================================
// Type Conversion Functions (§11.1)
// ============================================================================

/// int(value) → Integer
/// Parse value to Integer. Returns 0 on failure.
/// Per LANG.txt §11.1
fn builtin_int(value: &Value, _line: u32) -> Result<Value, RuntimeError> {
    let result = match value {
        // Integer (identity)
        Value::Integer(n) => *n,

        // Decimal - rounds to nearest, half away from zero
        Value::Decimal(d) => {
            let n = d.0;
            if n >= 0.0 {
                (n + 0.5).floor() as i64
            } else {
                (n - 0.5).ceil() as i64
            }
        }

        // String - parse
        Value::String(s) => s.trim().parse::<i64>().unwrap_or(0),

        // Boolean
        Value::Boolean(true) => 1,
        Value::Boolean(false) => 0,

        // Other types return 0
        _ => 0,
    };

    Ok(Value::Integer(result))
}

/// ints(string) → List[Integer]
/// Extract all parseable integers from a string using regex (-?[0-9]+)
/// Per LANG.txt §11.1
fn builtin_ints(value: &Value, line: u32) -> Result<Value, RuntimeError> {
    match value {
        Value::String(s) => {
            let re = Regex::new(r"-?[0-9]+").expect("valid regex");
            let result: Vector<Value> = re
                .find_iter(s)
                .filter_map(|m| m.as_str().parse::<i64>().ok())
                .map(Value::Integer)
                .collect();
            Ok(Value::List(result))
        }
        _ => Err(RuntimeError::new(
            format!("ints expects String, got {}", value.type_name()),
            line,
        )),
    }
}

/// list(value) → List
/// Convert to List representation
/// Per LANG.txt §11.1
fn builtin_list(value: &Value, line: u32) -> Result<Value, RuntimeError> {
    match value {
        // List (identity)
        Value::List(_) => Ok(value.clone()),

        // Set - convert to list
        Value::Set(s) => Ok(Value::List(s.iter().cloned().collect())),

        // Dictionary - returns list of [key, value] tuples
        Value::Dict(d) => {
            let tuples: Vector<Value> = d
                .iter()
                .map(|(k, v)| {
                    let mut tuple = Vector::new();
                    tuple.push_back(k.clone());
                    tuple.push_back(v.clone());
                    Value::List(tuple)
                })
                .collect();
            Ok(Value::List(tuples))
        }

        // String - each grapheme cluster
        Value::String(s) => {
            let chars: Vector<Value> = s
                .graphemes(true)
                .map(|g| Value::String(Rc::new(g.to_string())))
                .collect();
            Ok(Value::List(chars))
        }

        // Range - materialize to list
        Value::Range {
            start,
            end,
            inclusive,
        } => match end {
            Some(e) => {
                let mut result = Vector::new();
                if start <= e {
                    let actual_end = if *inclusive { *e } else { e - 1 };
                    for i in *start..=actual_end {
                        result.push_back(Value::Integer(i));
                    }
                } else {
                    // Descending range
                    let actual_end = if *inclusive { *e } else { e + 1 };
                    let mut i = *start;
                    while i >= actual_end {
                        result.push_back(Value::Integer(i));
                        i -= 1;
                    }
                }
                Ok(Value::List(result))
            }
            None => Err(RuntimeError::new(
                "Cannot convert unbounded range to list",
                line,
            )),
        },

        // LazySequence - cannot materialize infinite sequences
        Value::LazySequence(_) => Err(RuntimeError::new(
            "Cannot convert infinite lazy sequence to list; use take() first",
            line,
        )),

        _ => Err(RuntimeError::new(
            format!("list expects collection type, got {}", value.type_name()),
            line,
        )),
    }
}

/// set(value) → Set
/// Convert to Set representation
/// Per LANG.txt §11.1
fn builtin_set(value: &Value, line: u32) -> Result<Value, RuntimeError> {
    match value {
        // Set (identity)
        Value::Set(_) => Ok(value.clone()),

        // List - convert to set
        Value::List(v) => {
            let mut result = HashSet::new();
            for elem in v.iter() {
                if !elem.is_hashable() {
                    return Err(RuntimeError::new(
                        format!("Cannot add {} to set (not hashable)", elem.type_name()),
                        line,
                    ));
                }
                result.insert(elem.clone());
            }
            Ok(Value::Set(result))
        }

        // String - each grapheme cluster
        Value::String(s) => {
            let chars: HashSet<Value> = s
                .graphemes(true)
                .map(|g| Value::String(Rc::new(g.to_string())))
                .collect();
            Ok(Value::Set(chars))
        }

        // Range - materialize to set
        Value::Range {
            start,
            end,
            inclusive,
        } => match end {
            Some(e) => {
                let mut result = HashSet::new();
                if start <= e {
                    let actual_end = if *inclusive { *e } else { e - 1 };
                    for i in *start..=actual_end {
                        result.insert(Value::Integer(i));
                    }
                } else {
                    let actual_end = if *inclusive { *e } else { e + 1 };
                    let mut i = *start;
                    while i >= actual_end {
                        result.insert(Value::Integer(i));
                        i -= 1;
                    }
                }
                Ok(Value::Set(result))
            }
            None => Err(RuntimeError::new(
                "Cannot convert unbounded range to set",
                line,
            )),
        },

        _ => Err(RuntimeError::new(
            format!(
                "set expects List, Set, String, or Range, got {}",
                value.type_name()
            ),
            line,
        )),
    }
}

/// dict(value) → Dictionary
/// Convert to Dictionary representation
/// Per LANG.txt §11.1
fn builtin_dict(value: &Value, line: u32) -> Result<Value, RuntimeError> {
    match value {
        // Dictionary (identity)
        Value::Dict(_) => Ok(value.clone()),

        // List of tuples
        Value::List(v) => {
            let mut result = HashMap::new();
            for elem in v.iter() {
                match elem {
                    Value::List(tuple) if tuple.len() == 2 => {
                        let key = &tuple[0];
                        let val = &tuple[1];
                        if !key.is_hashable() {
                            return Err(RuntimeError::new(
                                format!(
                                    "Cannot use {} as dictionary key (not hashable)",
                                    key.type_name()
                                ),
                                line,
                            ));
                        }
                        result.insert(key.clone(), val.clone());
                    }
                    _ => {
                        return Err(RuntimeError::new(
                            "dict expects list of [key, value] pairs",
                            line,
                        ));
                    }
                }
            }
            Ok(Value::Dict(result))
        }

        _ => Err(RuntimeError::new(
            format!("dict expects List or Dictionary, got {}", value.type_name()),
            line,
        )),
    }
}

// ============================================================================
// Collection Access Functions (§11.2)
// ============================================================================

/// get(index, collection) → Value | Nil
/// Get element at index. Returns nil if not found.
/// Per LANG.txt §11.2
fn builtin_get(index: &Value, collection: &Value, line: u32) -> Result<Value, RuntimeError> {
    match collection {
        // List
        Value::List(list) => match index {
            Value::Integer(idx) => {
                let len = list.len() as i64;
                let actual_idx = if *idx < 0 { len + idx } else { *idx };
                if actual_idx < 0 || actual_idx >= len {
                    Ok(Value::Nil)
                } else {
                    Ok(list[actual_idx as usize].clone())
                }
            }
            _ => Err(RuntimeError::new(
                format!("List index must be Integer, got {}", index.type_name()),
                line,
            )),
        },

        // Set (membership check - returns value or nil)
        Value::Set(set) => {
            if set.contains(index) {
                Ok(index.clone())
            } else {
                Ok(Value::Nil)
            }
        }

        // Dictionary
        Value::Dict(dict) => Ok(dict.get(index).cloned().unwrap_or(Value::Nil)),

        // String
        Value::String(s) => match index {
            Value::Integer(idx) => {
                let graphemes: Vec<&str> = s.graphemes(true).collect();
                let len = graphemes.len() as i64;
                let actual_idx = if *idx < 0 { len + idx } else { *idx };
                if actual_idx < 0 || actual_idx >= len {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::String(Rc::new(
                        graphemes[actual_idx as usize].to_string(),
                    )))
                }
            }
            _ => Err(RuntimeError::new(
                format!("String index must be Integer, got {}", index.type_name()),
                line,
            )),
        },

        // Range
        Value::Range {
            start,
            end,
            inclusive,
        } => match index {
            Value::Integer(idx) => {
                if *idx < 0 {
                    // Negative indices for ranges need the length
                    match end {
                        Some(e) => {
                            let len = if *inclusive {
                                (e - start).abs() + 1
                            } else {
                                (e - start).abs()
                            };
                            let actual_idx = len + idx;
                            if actual_idx < 0 {
                                Ok(Value::Nil)
                            } else {
                                let step = if start <= e { 1 } else { -1 };
                                Ok(Value::Integer(start + actual_idx * step))
                            }
                        }
                        None => Err(RuntimeError::new(
                            "Cannot use negative index on unbounded range",
                            line,
                        )),
                    }
                } else {
                    // Check bounds for bounded ranges
                    if let Some(e) = end {
                        let len = if *inclusive {
                            (e - start).abs() + 1
                        } else {
                            (e - start).abs()
                        };
                        if *idx >= len {
                            return Ok(Value::Nil);
                        }
                    }
                    let step = match end {
                        Some(e) if e < start => -1,
                        _ => 1,
                    };
                    Ok(Value::Integer(start + idx * step))
                }
            }
            _ => Err(RuntimeError::new(
                format!("Range index must be Integer, got {}", index.type_name()),
                line,
            )),
        },

        _ => Err(RuntimeError::new(
            format!("get not supported for {}", collection.type_name()),
            line,
        )),
    }
}

/// size(collection) → Integer
/// Get number of elements in a collection
/// Per LANG.txt §11.2
fn builtin_size(collection: &Value, line: u32) -> Result<Value, RuntimeError> {
    let size = match collection {
        Value::List(v) => v.len() as i64,
        Value::Set(s) => s.len() as i64,
        Value::Dict(d) => d.len() as i64,
        Value::String(s) => s.graphemes(true).count() as i64,
        Value::Range {
            start,
            end,
            inclusive,
        } => match end {
            Some(e) => {
                let diff = (e - start).abs();
                if *inclusive { diff + 1 } else { diff }
            }
            None => {
                return Err(RuntimeError::new(
                    "Cannot get size of unbounded range",
                    line,
                ));
            }
        },
        _ => {
            return Err(RuntimeError::new(
                format!("size not supported for {}", collection.type_name()),
                line,
            ));
        }
    };
    Ok(Value::Integer(size))
}

/// first(collection) → Value | Nil
/// Get first element. Returns nil if collection is empty.
/// Per LANG.txt §11.2
fn builtin_first(collection: &Value, line: u32) -> Result<Value, RuntimeError> {
    match collection {
        Value::List(v) => Ok(v.front().cloned().unwrap_or(Value::Nil)),
        Value::Set(s) => Ok(s.iter().next().cloned().unwrap_or(Value::Nil)),
        Value::String(s) => Ok(s
            .graphemes(true)
            .next()
            .map(|g| Value::String(Rc::new(g.to_string())))
            .unwrap_or(Value::Nil)),
        Value::Range { start, .. } => Ok(Value::Integer(*start)),
        // LazySequence will need special handling in runtime
        _ => Err(RuntimeError::new(
            format!("first not supported for {}", collection.type_name()),
            line,
        )),
    }
}

/// second(collection) → Value | Nil
/// Get second element. Returns nil if collection has fewer than 2 elements.
/// Per LANG.txt §11.2
fn builtin_second(collection: &Value, line: u32) -> Result<Value, RuntimeError> {
    match collection {
        Value::List(v) => Ok(v.get(1).cloned().unwrap_or(Value::Nil)),
        Value::Set(s) => Ok(s.iter().nth(1).cloned().unwrap_or(Value::Nil)),
        Value::String(s) => Ok(s
            .graphemes(true)
            .nth(1)
            .map(|g| Value::String(Rc::new(g.to_string())))
            .unwrap_or(Value::Nil)),
        Value::Range {
            start,
            end,
            inclusive,
        } => {
            let step = match end {
                Some(e) if *e < *start => -1,
                _ => 1,
            };
            // Check if there's a second element
            let second = start + step;
            if let Some(e) = end {
                let in_range = if *inclusive {
                    (step > 0 && second <= *e) || (step < 0 && second >= *e)
                } else {
                    (step > 0 && second < *e) || (step < 0 && second > *e)
                };
                if !in_range {
                    return Ok(Value::Nil);
                }
            }
            Ok(Value::Integer(second))
        }
        _ => Err(RuntimeError::new(
            format!("second not supported for {}", collection.type_name()),
            line,
        )),
    }
}

/// rest(collection) → Collection
/// Get all but first element. Returns empty collection if input has ≤1 element.
/// Per LANG.txt §11.2
fn builtin_rest(collection: &Value, line: u32) -> Result<Value, RuntimeError> {
    match collection {
        Value::List(v) => {
            if v.is_empty() {
                Ok(Value::List(Vector::new()))
            } else {
                Ok(Value::List(v.clone().slice(1..)))
            }
        }
        Value::Set(s) => {
            let mut iter = s.iter();
            iter.next(); // Skip first
            Ok(Value::Set(iter.cloned().collect()))
        }
        Value::String(s) => {
            let mut graphemes = s.graphemes(true);
            graphemes.next(); // Skip first
            Ok(Value::String(Rc::new(graphemes.collect())))
        }
        Value::Range {
            start,
            end,
            inclusive,
        } => {
            let step = match end {
                Some(e) if *e < *start => -1,
                _ => 1,
            };
            let new_start = start + step;

            // For bounded range, check if new_start is still valid
            if let Some(e) = end {
                let still_valid = if *inclusive {
                    (step > 0 && new_start <= *e) || (step < 0 && new_start >= *e)
                } else {
                    (step > 0 && new_start < *e) || (step < 0 && new_start > *e)
                };
                if !still_valid {
                    // Return empty list for exhausted range
                    return Ok(Value::List(Vector::new()));
                }
            }

            // For unbounded or continuing ranges, return a new range
            // (Will need LazySequence support for proper unbounded handling)
            match end {
                Some(e) => Ok(Value::Range {
                    start: new_start,
                    end: Some(*e),
                    inclusive: *inclusive,
                }),
                None => Ok(Value::Range {
                    start: new_start,
                    end: None,
                    inclusive: *inclusive,
                }),
            }
        }
        _ => Err(RuntimeError::new(
            format!("rest not supported for {}", collection.type_name()),
            line,
        )),
    }
}

/// keys(dictionary) → List
/// Get dictionary keys as a List
/// Per LANG.txt §11.2
fn builtin_keys(collection: &Value, line: u32) -> Result<Value, RuntimeError> {
    match collection {
        Value::Dict(d) => Ok(Value::List(d.keys().cloned().collect())),
        _ => Err(RuntimeError::new(
            format!("keys expects Dictionary, got {}", collection.type_name()),
            line,
        )),
    }
}

/// values(dictionary) → List
/// Get dictionary values as a List
/// Per LANG.txt §11.2
fn builtin_values(collection: &Value, line: u32) -> Result<Value, RuntimeError> {
    match collection {
        Value::Dict(d) => Ok(Value::List(d.values().cloned().collect())),
        _ => Err(RuntimeError::new(
            format!("values expects Dictionary, got {}", collection.type_name()),
            line,
        )),
    }
}

// ============================================================================
// Collection Modification Functions (§11.3)
// ============================================================================

/// push(value, collection) → Collection
/// Add a new value to a collection
/// Per LANG.txt §11.3
fn builtin_push(value: &Value, collection: &Value, line: u32) -> Result<Value, RuntimeError> {
    match collection {
        // List (appends to end)
        Value::List(list) => {
            let mut result = list.clone();
            result.push_back(value.clone());
            Ok(Value::List(result))
        }

        // Set
        Value::Set(set) => {
            if !value.is_hashable() {
                return Err(RuntimeError::new(
                    format!("Cannot add {} to set (not hashable)", value.type_name()),
                    line,
                ));
            }
            let mut result = set.clone();
            result.insert(value.clone());
            Ok(Value::Set(result))
        }

        _ => Err(RuntimeError::new(
            format!("push expects List or Set, got {}", collection.type_name()),
            line,
        )),
    }
}

/// assoc(key, value, collection) → Collection
/// Associate the provided key/index with the given value
/// Per LANG.txt §11.3
fn builtin_assoc(
    key: &Value,
    value: &Value,
    collection: &Value,
    line: u32,
) -> Result<Value, RuntimeError> {
    match collection {
        // List (replaces at index)
        Value::List(list) => match key {
            Value::Integer(idx) => {
                let len = list.len();
                let actual_idx = if *idx < 0 {
                    (len as i64 + idx) as usize
                } else {
                    *idx as usize
                };

                // Fill with nil if index beyond current size
                let mut result = list.clone();
                while result.len() <= actual_idx {
                    result.push_back(Value::Nil);
                }
                result.set(actual_idx, value.clone());
                Ok(Value::List(result))
            }
            _ => Err(RuntimeError::new(
                format!("List index must be Integer, got {}", key.type_name()),
                line,
            )),
        },

        // Dictionary
        Value::Dict(dict) => {
            if !key.is_hashable() {
                return Err(RuntimeError::new(
                    format!(
                        "Cannot use {} as dictionary key (not hashable)",
                        key.type_name()
                    ),
                    line,
                ));
            }
            let mut result = dict.clone();
            result.insert(key.clone(), value.clone());
            Ok(Value::Dict(result))
        }

        _ => Err(RuntimeError::new(
            format!(
                "assoc expects List or Dictionary, got {}",
                collection.type_name()
            ),
            line,
        )),
    }
}

// ============================================================================
// Aggregation Functions (§11.8)
// ============================================================================

/// sum(collection) → Integer | Decimal
/// Sum all numeric elements. Returns 0 for empty collections.
/// Per LANG.txt §11.8
fn builtin_sum(collection: &Value, line: u32) -> Result<Value, RuntimeError> {
    match collection {
        Value::List(list) => {
            if list.is_empty() {
                return Ok(Value::Integer(0));
            }
            let mut has_decimal = false;
            let mut int_sum: i64 = 0;
            let mut decimal_sum: f64 = 0.0;

            for elem in list.iter() {
                match elem {
                    Value::Integer(n) => {
                        if has_decimal {
                            decimal_sum += *n as f64;
                        } else {
                            int_sum += n;
                        }
                    }
                    Value::Decimal(d) => {
                        if !has_decimal {
                            has_decimal = true;
                            decimal_sum = int_sum as f64;
                        }
                        decimal_sum += d.0;
                    }
                    _ => {
                        return Err(RuntimeError::new(
                            format!("sum: non-numeric element {}", elem.type_name()),
                            line,
                        ));
                    }
                }
            }

            if has_decimal {
                Ok(Value::Decimal(OrderedFloat(decimal_sum)))
            } else {
                Ok(Value::Integer(int_sum))
            }
        }
        Value::Set(set) => {
            if set.is_empty() {
                return Ok(Value::Integer(0));
            }
            let mut has_decimal = false;
            let mut int_sum: i64 = 0;
            let mut decimal_sum: f64 = 0.0;

            for elem in set.iter() {
                match elem {
                    Value::Integer(n) => {
                        if has_decimal {
                            decimal_sum += *n as f64;
                        } else {
                            int_sum += n;
                        }
                    }
                    Value::Decimal(d) => {
                        if !has_decimal {
                            has_decimal = true;
                            decimal_sum = int_sum as f64;
                        }
                        decimal_sum += d.0;
                    }
                    _ => {
                        return Err(RuntimeError::new(
                            format!("sum: non-numeric element {}", elem.type_name()),
                            line,
                        ));
                    }
                }
            }

            if has_decimal {
                Ok(Value::Decimal(OrderedFloat(decimal_sum)))
            } else {
                Ok(Value::Integer(int_sum))
            }
        }
        Value::Dict(dict) => {
            if dict.is_empty() {
                return Ok(Value::Integer(0));
            }
            let mut has_decimal = false;
            let mut int_sum: i64 = 0;
            let mut decimal_sum: f64 = 0.0;

            for value in dict.values() {
                match value {
                    Value::Integer(n) => {
                        if has_decimal {
                            decimal_sum += *n as f64;
                        } else {
                            int_sum += n;
                        }
                    }
                    Value::Decimal(d) => {
                        if !has_decimal {
                            has_decimal = true;
                            decimal_sum = int_sum as f64;
                        }
                        decimal_sum += d.0;
                    }
                    _ => {
                        return Err(RuntimeError::new(
                            format!("sum: non-numeric value {}", value.type_name()),
                            line,
                        ));
                    }
                }
            }

            if has_decimal {
                Ok(Value::Decimal(OrderedFloat(decimal_sum)))
            } else {
                Ok(Value::Integer(int_sum))
            }
        }
        Value::Range {
            start,
            end,
            inclusive,
        } => match end {
            Some(e) => {
                let actual_end = if *inclusive { *e } else { e - 1 };
                if *start > actual_end {
                    return Ok(Value::Integer(0));
                }
                // Sum of arithmetic sequence: n * (first + last) / 2
                let n = actual_end - start + 1;
                let sum = n * (start + actual_end) / 2;
                Ok(Value::Integer(sum))
            }
            None => Err(RuntimeError::new(
                "Cannot sum unbounded range",
                line,
            )),
        },
        _ => Err(RuntimeError::new(
            format!("sum does not support {}", collection.type_name()),
            line,
        )),
    }
}

/// max(..values) → Value | Nil
/// Find the maximum value. Variadic or single collection.
/// Per LANG.txt §11.8
fn builtin_max(args: &[Value], line: u32) -> Result<Value, RuntimeError> {
    // If single argument is a collection, find max in collection
    if args.len() == 1 {
        return max_of_collection(&args[0], line);
    }

    // Otherwise find max among arguments
    let mut max_val: Option<&Value> = None;
    for arg in args {
        match max_val {
            None => max_val = Some(arg),
            Some(current) => {
                if compare_values(arg, current, line)? > 0 {
                    max_val = Some(arg);
                }
            }
        }
    }

    Ok(max_val.cloned().unwrap_or(Value::Nil))
}

fn max_of_collection(collection: &Value, line: u32) -> Result<Value, RuntimeError> {
    match collection {
        Value::List(list) => {
            if list.is_empty() {
                return Ok(Value::Nil);
            }
            let mut max_val = &list[0];
            for elem in list.iter().skip(1) {
                if compare_values(elem, max_val, line)? > 0 {
                    max_val = elem;
                }
            }
            Ok(max_val.clone())
        }
        Value::Set(set) => {
            if set.is_empty() {
                return Ok(Value::Nil);
            }
            let mut iter = set.iter();
            let mut max_val = iter.next().unwrap();
            for elem in iter {
                if compare_values(elem, max_val, line)? > 0 {
                    max_val = elem;
                }
            }
            Ok(max_val.clone())
        }
        Value::Dict(dict) => {
            if dict.is_empty() {
                return Ok(Value::Nil);
            }
            let mut iter = dict.values();
            let mut max_val = iter.next().unwrap();
            for value in iter {
                if compare_values(value, max_val, line)? > 0 {
                    max_val = value;
                }
            }
            Ok(max_val.clone())
        }
        Value::Range {
            start,
            end,
            inclusive,
        } => match end {
            Some(e) => {
                let actual_end = if *inclusive { *e } else { e - 1 };
                if *start > actual_end {
                    return Ok(Value::Nil);
                }
                Ok(Value::Integer(actual_end.max(*start)))
            }
            None => Err(RuntimeError::new(
                "Cannot find max of unbounded range",
                line,
            )),
        },
        _ => Err(RuntimeError::new(
            format!("max does not support {}", collection.type_name()),
            line,
        )),
    }
}

/// min(..values) → Value | Nil
/// Find the minimum value. Variadic or single collection.
/// Per LANG.txt §11.8
fn builtin_min(args: &[Value], line: u32) -> Result<Value, RuntimeError> {
    // If single argument is a collection, find min in collection
    if args.len() == 1 {
        return min_of_collection(&args[0], line);
    }

    // Otherwise find min among arguments
    let mut min_val: Option<&Value> = None;
    for arg in args {
        match min_val {
            None => min_val = Some(arg),
            Some(current) => {
                if compare_values(arg, current, line)? < 0 {
                    min_val = Some(arg);
                }
            }
        }
    }

    Ok(min_val.cloned().unwrap_or(Value::Nil))
}

fn min_of_collection(collection: &Value, line: u32) -> Result<Value, RuntimeError> {
    match collection {
        Value::List(list) => {
            if list.is_empty() {
                return Ok(Value::Nil);
            }
            let mut min_val = &list[0];
            for elem in list.iter().skip(1) {
                if compare_values(elem, min_val, line)? < 0 {
                    min_val = elem;
                }
            }
            Ok(min_val.clone())
        }
        Value::Set(set) => {
            if set.is_empty() {
                return Ok(Value::Nil);
            }
            let mut iter = set.iter();
            let mut min_val = iter.next().unwrap();
            for elem in iter {
                if compare_values(elem, min_val, line)? < 0 {
                    min_val = elem;
                }
            }
            Ok(min_val.clone())
        }
        Value::Dict(dict) => {
            if dict.is_empty() {
                return Ok(Value::Nil);
            }
            let mut iter = dict.values();
            let mut min_val = iter.next().unwrap();
            for value in iter {
                if compare_values(value, min_val, line)? < 0 {
                    min_val = value;
                }
            }
            Ok(min_val.clone())
        }
        Value::Range {
            start,
            end,
            inclusive,
        } => match end {
            Some(e) => {
                let actual_end = if *inclusive { *e } else { e - 1 };
                if *start > actual_end {
                    return Ok(Value::Nil);
                }
                Ok(Value::Integer(actual_end.min(*start)))
            }
            None => Ok(Value::Integer(*start)),
        },
        _ => Err(RuntimeError::new(
            format!("min does not support {}", collection.type_name()),
            line,
        )),
    }
}

/// Helper to compare two values, returning -1, 0, or 1
fn compare_values(a: &Value, b: &Value, line: u32) -> Result<i32, RuntimeError> {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => Ok(x.cmp(y) as i32),
        (Value::Integer(x), Value::Decimal(y)) => {
            let fx = *x as f64;
            if fx < y.0 {
                Ok(-1)
            } else if fx > y.0 {
                Ok(1)
            } else {
                Ok(0)
            }
        }
        (Value::Decimal(x), Value::Integer(y)) => {
            let fy = *y as f64;
            if x.0 < fy {
                Ok(-1)
            } else if x.0 > fy {
                Ok(1)
            } else {
                Ok(0)
            }
        }
        (Value::Decimal(x), Value::Decimal(y)) => {
            if x.0 < y.0 {
                Ok(-1)
            } else if x.0 > y.0 {
                Ok(1)
            } else {
                Ok(0)
            }
        }
        (Value::String(x), Value::String(y)) => Ok(x.cmp(y) as i32),
        _ => Err(RuntimeError::new(
            format!(
                "Cannot compare {} and {}",
                a.type_name(),
                b.type_name()
            ),
            line,
        )),
    }
}

// ============================================================================
// Sequence Manipulation Functions (§11.9)
// ============================================================================

/// skip(n, collection) → Collection
/// Skip n elements. Per LANG.txt §11.9
fn builtin_skip(total: &Value, collection: &Value, line: u32) -> Result<Value, RuntimeError> {
    let n = match total {
        Value::Integer(n) => *n as usize,
        _ => {
            return Err(RuntimeError::new(
                format!("skip expects Integer, got {}", total.type_name()),
                line,
            ))
        }
    };

    match collection {
        Value::List(list) => {
            if n >= list.len() {
                Ok(Value::List(Vector::new()))
            } else {
                Ok(Value::List(list.clone().slice(n..)))
            }
        }
        Value::Set(set) => {
            if n >= set.len() {
                Ok(Value::Set(HashSet::new()))
            } else {
                Ok(Value::Set(set.iter().skip(n).cloned().collect()))
            }
        }
        Value::Range {
            start,
            end,
            inclusive,
        } => match end {
            Some(e) => {
                let actual_end = if *inclusive { *e } else { e - 1 };
                let step = if start <= &actual_end { 1 } else { -1 };
                let new_start = start + (n as i64) * step;

                // Materialize to list for bounded ranges after skip
                let mut result = Vector::new();
                if step > 0 {
                    let mut i = new_start;
                    while i <= actual_end {
                        result.push_back(Value::Integer(i));
                        i += 1;
                    }
                } else {
                    let mut i = new_start;
                    while i >= actual_end {
                        result.push_back(Value::Integer(i));
                        i -= 1;
                    }
                }
                Ok(Value::List(result))
            }
            None => {
                // Unbounded range - return new range starting later
                let new_start = start + n as i64;
                Ok(Value::Range {
                    start: new_start,
                    end: None,
                    inclusive: *inclusive,
                })
            }
        },
        _ => Err(RuntimeError::new(
            format!("skip does not support {}", collection.type_name()),
            line,
        )),
    }
}

/// take(n, collection) → List
/// Take n elements. Per LANG.txt §11.9
fn builtin_take(total: &Value, collection: &Value, line: u32) -> Result<Value, RuntimeError> {
    let n = match total {
        Value::Integer(n) => *n as usize,
        _ => {
            return Err(RuntimeError::new(
                format!("take expects Integer, got {}", total.type_name()),
                line,
            ))
        }
    };

    match collection {
        Value::List(list) => {
            let count = n.min(list.len());
            Ok(Value::List(list.clone().slice(0..count)))
        }
        Value::Set(set) => Ok(Value::List(set.iter().take(n).cloned().collect())),
        Value::Range {
            start,
            end,
            inclusive,
        } => {
            let mut result = Vector::new();
            let step = match end {
                Some(e) if e < start => -1,
                _ => 1,
            };

            let actual_end = end.map(|e| if *inclusive { e } else { e - step });

            for i in 0..n {
                let val = start + (i as i64) * step;
                if let Some(e) = actual_end
                    && ((step > 0 && val > e) || (step < 0 && val < e))
                {
                    break;
                }
                result.push_back(Value::Integer(val));
            }
            Ok(Value::List(result))
        }
        _ => Err(RuntimeError::new(
            format!("take does not support {}", collection.type_name()),
            line,
        )),
    }
}

/// reverse(collection) → Collection
/// Reverse the order. Per LANG.txt §11.9
fn builtin_reverse(collection: &Value, line: u32) -> Result<Value, RuntimeError> {
    match collection {
        Value::List(list) => {
            let reversed: Vector<Value> = list.iter().rev().cloned().collect();
            Ok(Value::List(reversed))
        }
        Value::String(s) => {
            let reversed: String = s.graphemes(true).rev().collect();
            Ok(Value::String(Rc::new(reversed)))
        }
        Value::Range {
            start,
            end,
            inclusive,
        } => match end {
            Some(e) => {
                let actual_end = if *inclusive { *e } else { e - 1 };
                // Materialize and reverse
                let mut result = Vector::new();
                if start <= &actual_end {
                    for i in (*start..=actual_end).rev() {
                        result.push_back(Value::Integer(i));
                    }
                } else {
                    for i in (actual_end..=*start).rev() {
                        result.push_back(Value::Integer(i));
                    }
                }
                Ok(Value::List(result))
            }
            None => Err(RuntimeError::new(
                "Cannot reverse unbounded range",
                line,
            )),
        },
        _ => Err(RuntimeError::new(
            format!("reverse does not support {}", collection.type_name()),
            line,
        )),
    }
}

/// rotate(steps, collection) → List
/// Rotate list by n steps. Per LANG.txt §11.9
fn builtin_rotate(steps: &Value, collection: &Value, line: u32) -> Result<Value, RuntimeError> {
    let n = match steps {
        Value::Integer(n) => *n,
        _ => {
            return Err(RuntimeError::new(
                format!("rotate expects Integer, got {}", steps.type_name()),
                line,
            ))
        }
    };

    match collection {
        Value::List(list) => {
            if list.is_empty() {
                return Ok(Value::List(Vector::new()));
            }
            let len = list.len() as i64;
            // Normalize: positive n means forward rotation (last moves to start)
            let normalized = ((n % len) + len) % len;
            let split_point = len - normalized;

            let mut result = Vector::new();
            for i in split_point..len {
                result.push_back(list[i as usize].clone());
            }
            for i in 0..split_point {
                result.push_back(list[i as usize].clone());
            }
            Ok(Value::List(result))
        }
        _ => Err(RuntimeError::new(
            format!("rotate expects List, got {}", collection.type_name()),
            line,
        )),
    }
}

/// chunk(size, collection) → List[List]
/// Split into chunks. Per LANG.txt §11.9
fn builtin_chunk(size: &Value, collection: &Value, line: u32) -> Result<Value, RuntimeError> {
    let chunk_size = match size {
        Value::Integer(n) if *n > 0 => *n as usize,
        Value::Integer(_) => {
            return Err(RuntimeError::new("chunk size must be positive", line))
        }
        _ => {
            return Err(RuntimeError::new(
                format!("chunk expects Integer, got {}", size.type_name()),
                line,
            ))
        }
    };

    match collection {
        Value::List(list) => {
            let mut result = Vector::new();
            let mut current_chunk = Vector::new();

            for elem in list.iter() {
                current_chunk.push_back(elem.clone());
                if current_chunk.len() == chunk_size {
                    result.push_back(Value::List(current_chunk));
                    current_chunk = Vector::new();
                }
            }

            if !current_chunk.is_empty() {
                result.push_back(Value::List(current_chunk));
            }

            Ok(Value::List(result))
        }
        _ => Err(RuntimeError::new(
            format!("chunk expects List, got {}", collection.type_name()),
            line,
        )),
    }
}

// ============================================================================
// Set Operations (§11.10)
// ============================================================================

/// union(..values) → Set
/// Elements found in any collection. Per LANG.txt §11.10
fn builtin_union(args: &[Value], line: u32) -> Result<Value, RuntimeError> {
    let mut result = HashSet::new();

    // If single argument is a list of collections, use that
    if args.len() == 1
        && let Value::List(collections) = &args[0]
    {
        for collection in collections.iter() {
            add_to_set(&mut result, collection, line)?;
        }
        return Ok(Value::Set(result));
    }

    for arg in args {
        add_to_set(&mut result, arg, line)?;
    }

    Ok(Value::Set(result))
}

fn add_to_set(result: &mut HashSet<Value>, collection: &Value, line: u32) -> Result<(), RuntimeError> {
    match collection {
        Value::List(list) => {
            for elem in list.iter() {
                if !elem.is_hashable() {
                    return Err(RuntimeError::new(
                        format!("Cannot add {} to set (not hashable)", elem.type_name()),
                        line,
                    ));
                }
                result.insert(elem.clone());
            }
        }
        Value::Set(set) => {
            for elem in set.iter() {
                result.insert(elem.clone());
            }
        }
        Value::Range {
            start,
            end,
            inclusive,
        } => match end {
            Some(e) => {
                let actual_end = if *inclusive { *e } else { e - 1 };
                if start <= &actual_end {
                    for i in *start..=actual_end {
                        result.insert(Value::Integer(i));
                    }
                } else {
                    for i in actual_end..=*start {
                        result.insert(Value::Integer(i));
                    }
                }
            }
            None => {
                return Err(RuntimeError::new(
                    "Cannot convert unbounded range to set",
                    line,
                ));
            }
        },
        Value::String(s) => {
            for grapheme in s.graphemes(true) {
                result.insert(Value::String(Rc::new(grapheme.to_string())));
            }
        }
        _ => {
            return Err(RuntimeError::new(
                format!("union does not support {}", collection.type_name()),
                line,
            ));
        }
    }
    Ok(())
}

/// intersection(..values) → Set
/// Elements found in all collections. Per LANG.txt §11.10
fn builtin_intersection(args: &[Value], line: u32) -> Result<Value, RuntimeError> {
    // If single argument is a list of collections, use that
    let collections: Vec<&Value> = if args.len() == 1 {
        if let Value::List(list) = &args[0] {
            list.iter().collect()
        } else {
            args.iter().collect()
        }
    } else {
        args.iter().collect()
    };

    if collections.is_empty() {
        return Ok(Value::Set(HashSet::new()));
    }

    // Start with elements from first collection
    let mut result = HashSet::new();
    add_to_set(&mut result, collections[0], line)?;

    // Intersect with each subsequent collection
    for collection in collections.iter().skip(1) {
        let mut other = HashSet::new();
        add_to_set(&mut other, collection, line)?;
        result = result.intersection(other);
    }

    Ok(Value::Set(result))
}

// ============================================================================
// Predicates (§11.11)
// ============================================================================

/// includes?(collection, value) → Boolean
/// Check if value is in collection. Per LANG.txt §11.11
fn builtin_includes(collection: &Value, value: &Value, line: u32) -> Result<Value, RuntimeError> {
    let result = match collection {
        Value::List(list) => list.contains(value),
        Value::Set(set) => set.contains(value),
        Value::Dict(dict) => dict.contains_key(value),
        Value::String(s) => {
            if let Value::String(needle) = value {
                s.contains(needle.as_str())
            } else {
                false
            }
        }
        Value::Range {
            start,
            end,
            inclusive,
        } => {
            if let Value::Integer(n) = value {
                match end {
                    Some(e) => {
                        if *inclusive {
                            (*start <= *e && *n >= *start && *n <= *e)
                                || (*start > *e && *n <= *start && *n >= *e)
                        } else {
                            (*start <= *e && *n >= *start && *n < *e)
                                || (*start > *e && *n <= *start && *n > *e)
                        }
                    }
                    None => *n >= *start,
                }
            } else {
                false
            }
        }
        _ => {
            return Err(RuntimeError::new(
                format!("includes? does not support {}", collection.type_name()),
                line,
            ))
        }
    };

    Ok(Value::Boolean(result))
}

/// excludes?(collection, value) → Boolean
/// Check if value is NOT in collection. Per LANG.txt §11.11
fn builtin_excludes(collection: &Value, value: &Value, line: u32) -> Result<Value, RuntimeError> {
    let includes = builtin_includes(collection, value, line)?;
    match includes {
        Value::Boolean(b) => Ok(Value::Boolean(!b)),
        _ => Ok(Value::Boolean(true)), // Should not happen
    }
}
