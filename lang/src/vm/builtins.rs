//! Built-in functions for santa-lang
//!
//! Phase 9: Type conversion, collection access, and collection modification
//! Per LANG.txt §11.1-11.3

use im_rc::{HashMap, HashSet, Vector};
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
            | BuiltinId::Values => (1, 1),

            // Two argument functions
            BuiltinId::Get | BuiltinId::Push => (2, 2),

            // Three argument functions
            BuiltinId::Assoc | BuiltinId::Update => (3, 3),

            // Four argument functions
            BuiltinId::UpdateD => (4, 4),
        }
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
            _ => Err(value),
        }
    }
}

/// Execute a built-in function
/// Returns the result value or an error
pub fn call_builtin(
    id: BuiltinId,
    args: &[Value],
    line: u32,
) -> Result<Value, RuntimeError> {
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
        BuiltinId::Update | BuiltinId::UpdateD => {
            // These require function callbacks - will be handled specially in runtime
            Err(RuntimeError::new(
                format!("{} requires callback support", id.name()),
                line,
            ))
        }
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
                        format!(
                            "Cannot add {} to set (not hashable)",
                            elem.type_name()
                        ),
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
            format!("set expects List, Set, String, or Range, got {}", value.type_name()),
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
                        ))
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
                    Ok(Value::String(Rc::new(graphemes[actual_idx as usize].to_string())))
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
                if *inclusive {
                    diff + 1
                } else {
                    diff
                }
            }
            None => {
                return Err(RuntimeError::new("Cannot get size of unbounded range", line))
            }
        },
        _ => {
            return Err(RuntimeError::new(
                format!("size not supported for {}", collection.type_name()),
                line,
            ))
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
            format!("assoc expects List or Dictionary, got {}", collection.type_name()),
            line,
        )),
    }
}
