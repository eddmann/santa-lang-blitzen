//! Built-in functions for santa-lang
//!
//! Phase 9: Type conversion, collection access, and collection modification
//! Per LANG.txt §11.1-11.3

// RuntimeError is 128+ bytes but boxing would add overhead on successful paths.
// Error paths are not performance-critical for an interpreter.
#![allow(clippy::result_large_err)]

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
    Last = 17,
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

    // Lazy Sequence Generation (§11.12)
    Zip = 110,
    Repeat = 111,
    Cycle = 112,
    Iterate = 113,
    Combinations = 114,

    // Range Generation (§11.13)
    RangeFn = 120,

    // String Functions (§11.14)
    Lines = 130,
    Split = 131,
    RegexMatch = 132,
    RegexMatchAll = 133,
    Upper = 134,
    Lower = 135,
    Replace = 136,
    Md5 = 137,
    Join = 138,

    // Math Functions (§11.15)
    Abs = 140,
    Signum = 141,
    VecAdd = 142,

    // Bitwise Functions (§4.5)
    BitAnd = 150,
    BitOr = 151,
    BitXor = 152,
    BitNot = 153,
    BitShiftLeft = 154,
    BitShiftRight = 155,

    // Utility Functions (§11.16)
    Id = 160,
    Type = 161,
    Memoize = 162,
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
            BuiltinId::Last => "last",
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
            BuiltinId::Zip => "zip",
            BuiltinId::Repeat => "repeat",
            BuiltinId::Cycle => "cycle",
            BuiltinId::Iterate => "iterate",
            BuiltinId::Combinations => "combinations",
            BuiltinId::RangeFn => "range",
            BuiltinId::Lines => "lines",
            BuiltinId::Split => "split",
            BuiltinId::RegexMatch => "regex_match",
            BuiltinId::RegexMatchAll => "regex_match_all",
            BuiltinId::Upper => "upper",
            BuiltinId::Lower => "lower",
            BuiltinId::Replace => "replace",
            BuiltinId::Md5 => "md5",
            BuiltinId::Join => "join",
            BuiltinId::Abs => "abs",
            BuiltinId::Signum => "signum",
            BuiltinId::VecAdd => "vec_add",
            BuiltinId::BitAnd => "bit_and",
            BuiltinId::BitOr => "bit_or",
            BuiltinId::BitXor => "bit_xor",
            BuiltinId::BitNot => "bit_not",
            BuiltinId::BitShiftLeft => "bit_shift_left",
            BuiltinId::BitShiftRight => "bit_shift_right",
            BuiltinId::Id => "id",
            BuiltinId::Type => "type",
            BuiltinId::Memoize => "memoize",
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
            "last" => Some(BuiltinId::Last),
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
            "zip" => Some(BuiltinId::Zip),
            "repeat" => Some(BuiltinId::Repeat),
            "cycle" => Some(BuiltinId::Cycle),
            "iterate" => Some(BuiltinId::Iterate),
            "combinations" => Some(BuiltinId::Combinations),
            "range" => Some(BuiltinId::RangeFn),
            "lines" => Some(BuiltinId::Lines),
            "split" => Some(BuiltinId::Split),
            "regex_match" => Some(BuiltinId::RegexMatch),
            "regex_match_all" => Some(BuiltinId::RegexMatchAll),
            "upper" => Some(BuiltinId::Upper),
            "lower" => Some(BuiltinId::Lower),
            "replace" => Some(BuiltinId::Replace),
            "md5" => Some(BuiltinId::Md5),
            "join" => Some(BuiltinId::Join),
            "abs" => Some(BuiltinId::Abs),
            "signum" => Some(BuiltinId::Signum),
            "vec_add" => Some(BuiltinId::VecAdd),
            "bit_and" => Some(BuiltinId::BitAnd),
            "bit_or" => Some(BuiltinId::BitOr),
            "bit_xor" => Some(BuiltinId::BitXor),
            "bit_not" => Some(BuiltinId::BitNot),
            "bit_shift_left" => Some(BuiltinId::BitShiftLeft),
            "bit_shift_right" => Some(BuiltinId::BitShiftRight),
            "id" => Some(BuiltinId::Id),
            "type" => Some(BuiltinId::Type),
            "memoize" => Some(BuiltinId::Memoize),
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
            | BuiltinId::Last
            | BuiltinId::Rest
            | BuiltinId::Keys
            | BuiltinId::Values
            | BuiltinId::Sum
            | BuiltinId::Reverse
            | BuiltinId::Repeat
            | BuiltinId::Cycle
            | BuiltinId::Lines
            | BuiltinId::Upper
            | BuiltinId::Lower
            | BuiltinId::Md5
            | BuiltinId::Abs
            | BuiltinId::Signum
            | BuiltinId::BitNot
            | BuiltinId::Id
            | BuiltinId::Type
            | BuiltinId::Memoize => (1, 1),

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
            | BuiltinId::All
            | BuiltinId::Iterate
            | BuiltinId::Combinations
            | BuiltinId::Split
            | BuiltinId::RegexMatch
            | BuiltinId::RegexMatchAll
            | BuiltinId::Join
            | BuiltinId::VecAdd
            | BuiltinId::BitAnd
            | BuiltinId::BitOr
            | BuiltinId::BitXor
            | BuiltinId::BitShiftLeft
            | BuiltinId::BitShiftRight => (2, 2),

            // Three argument functions
            BuiltinId::Assoc
            | BuiltinId::Update
            | BuiltinId::Fold
            | BuiltinId::FoldS
            | BuiltinId::Scan
            | BuiltinId::RangeFn
            | BuiltinId::Replace => (3, 3),

            // Four argument functions
            BuiltinId::UpdateD => (4, 4),

            // Variadic functions
            BuiltinId::Max
            | BuiltinId::Min
            | BuiltinId::Union
            | BuiltinId::Intersection
            | BuiltinId::Zip => (1, 255),
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
                | BuiltinId::Iterate
                | BuiltinId::Take
                | BuiltinId::Second
                | BuiltinId::Last
                | BuiltinId::Rest
                | BuiltinId::Get
                | BuiltinId::First
                | BuiltinId::Memoize
                | BuiltinId::List
                | BuiltinId::Set
                | BuiltinId::Sum
                | BuiltinId::Size
                | BuiltinId::Max
                | BuiltinId::Min
                | BuiltinId::Includes
                | BuiltinId::Excludes
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
            17 => Ok(BuiltinId::Last),
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
            110 => Ok(BuiltinId::Zip),
            111 => Ok(BuiltinId::Repeat),
            112 => Ok(BuiltinId::Cycle),
            113 => Ok(BuiltinId::Iterate),
            114 => Ok(BuiltinId::Combinations),
            120 => Ok(BuiltinId::RangeFn),
            130 => Ok(BuiltinId::Lines),
            131 => Ok(BuiltinId::Split),
            132 => Ok(BuiltinId::RegexMatch),
            133 => Ok(BuiltinId::RegexMatchAll),
            134 => Ok(BuiltinId::Upper),
            135 => Ok(BuiltinId::Lower),
            136 => Ok(BuiltinId::Replace),
            137 => Ok(BuiltinId::Md5),
            138 => Ok(BuiltinId::Join),
            140 => Ok(BuiltinId::Abs),
            141 => Ok(BuiltinId::Signum),
            142 => Ok(BuiltinId::VecAdd),
            150 => Ok(BuiltinId::BitAnd),
            151 => Ok(BuiltinId::BitOr),
            152 => Ok(BuiltinId::BitXor),
            153 => Ok(BuiltinId::BitNot),
            154 => Ok(BuiltinId::BitShiftLeft),
            155 => Ok(BuiltinId::BitShiftRight),
            160 => Ok(BuiltinId::Id),
            161 => Ok(BuiltinId::Type),
            162 => Ok(BuiltinId::Memoize),
            _ => Err(value),
        }
    }
}

/// Execute a built-in function
/// Returns the result value or an error
pub fn call_builtin(id: BuiltinId, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
    let (min_arity, max_arity) = id.arity();
    // max_arity == 255 means unlimited (variadic)
    let exceeds_max = max_arity != 255 && args.len() > max_arity as usize;
    if args.len() < min_arity as usize || exceeds_max {
        return Err(RuntimeError::new(
            format!(
                "{} expects {} argument(s), got {}",
                id.name(),
                if min_arity == max_arity {
                    min_arity.to_string()
                } else if max_arity == 255 {
                    format!("{}+", min_arity)
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
        // List and Set need callback support for LazySequence - handled by VM
        BuiltinId::List | BuiltinId::Set => Err(RuntimeError::new(
            format!("{} is not a callback builtin", id.name()),
            line,
        )),
        BuiltinId::Dict => builtin_dict(&args[0], line),
        // Size is now handled in runtime.rs for LazySequence support
        // Get, First, Second, Rest are callback builtins for LazySequence support
        BuiltinId::Keys => builtin_keys(&args[0], line),
        BuiltinId::Values => builtin_values(&args[0], line),
        BuiltinId::Push => builtin_push(&args[0], &args[1], line),
        BuiltinId::Assoc => builtin_assoc(&args[0], &args[1], &args[2], line),
        // Max, Min, Sum, Size are callback builtins for LazySequence support
        BuiltinId::Skip => builtin_skip(&args[0], &args[1], line),
        BuiltinId::Reverse => builtin_reverse(&args[0], line),
        BuiltinId::Rotate => builtin_rotate(&args[0], &args[1], line),
        BuiltinId::Chunk => builtin_chunk(&args[0], &args[1], line),
        BuiltinId::Union => builtin_union(args, line),
        BuiltinId::Intersection => builtin_intersection(args, line),
        // Includes/Excludes are callback builtins for LazySequence support
        // BuiltinId::Includes and BuiltinId::Excludes handled in runtime.rs
        // Phase 12: Lazy sequence generators
        BuiltinId::Zip => builtin_zip(args, line),
        BuiltinId::Repeat => builtin_repeat(&args[0], line),
        BuiltinId::Cycle => builtin_cycle(&args[0], line),
        BuiltinId::Combinations => builtin_combinations(&args[0], &args[1], line),
        BuiltinId::RangeFn => builtin_range_fn(&args[0], &args[1], &args[2], line),
        // Phase 13: String functions
        BuiltinId::Lines => builtin_lines(&args[0], line),
        BuiltinId::Split => builtin_split(&args[0], &args[1], line),
        BuiltinId::RegexMatch => builtin_regex_match(&args[0], &args[1], line),
        BuiltinId::RegexMatchAll => builtin_regex_match_all(&args[0], &args[1], line),
        BuiltinId::Upper => builtin_upper(&args[0], line),
        BuiltinId::Lower => builtin_lower(&args[0], line),
        BuiltinId::Md5 => builtin_md5(&args[0], line),
        BuiltinId::Replace => builtin_replace(&args[0], &args[1], &args[2], line),
        BuiltinId::Join => builtin_join(&args[0], &args[1], line),
        // Phase 13: Math functions
        BuiltinId::Abs => builtin_abs(&args[0], line),
        BuiltinId::Signum => builtin_signum(&args[0], line),
        BuiltinId::VecAdd => builtin_vec_add(&args[0], &args[1], line),
        // Phase 13: Bitwise functions
        BuiltinId::BitAnd => builtin_bit_and(&args[0], &args[1], line),
        BuiltinId::BitOr => builtin_bit_or(&args[0], &args[1], line),
        BuiltinId::BitXor => builtin_bit_xor(&args[0], &args[1], line),
        BuiltinId::BitNot => builtin_bit_not(&args[0], line),
        BuiltinId::BitShiftLeft => builtin_bit_shift_left(&args[0], &args[1], line),
        BuiltinId::BitShiftRight => builtin_bit_shift_right(&args[0], &args[1], line),
        // Phase 13: Utility functions
        BuiltinId::Id => builtin_id(&args[0], line),
        BuiltinId::Type => builtin_type(&args[0], line),
        // Callback-based builtins - handled specially in runtime
        BuiltinId::Get
        | BuiltinId::First
        | BuiltinId::Last
        | BuiltinId::Update
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
        | BuiltinId::All
        | BuiltinId::Iterate
        | BuiltinId::Take
        | BuiltinId::Second
        | BuiltinId::Rest
        | BuiltinId::Memoize
        | BuiltinId::Sum
        | BuiltinId::Size
        | BuiltinId::Max
        | BuiltinId::Min
        | BuiltinId::Includes
        | BuiltinId::Excludes => {
            // These builtins require callback support and are handled directly by the VM
            Err(RuntimeError::new(
                format!(
                    "{} requires callback support - should be handled by VM",
                    id.name()
                ),
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

// get(), first(), size() are now callback builtins in runtime.rs
// to support LazySequence with callbacks (iterate, map, filter)

// first(), second() and rest() are now callback builtins in runtime.rs
// to support LazySequence with callbacks (iterate, map, filter)

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

// sum(), max(), min() are now callback builtins in runtime.rs
// to support LazySequence with callbacks (iterate, map, filter)

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
            ));
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
                let step: i64 = if start <= &actual_end { 1 } else { -1 };
                let new_start = start + (n as i64) * step;

                // Return a new Range with adjusted start (stays lazy)
                // Check if we've skipped past the end
                if (step > 0 && new_start > actual_end) || (step < 0 && new_start < actual_end) {
                    // Empty range - return empty list
                    Ok(Value::List(Vector::new()))
                } else {
                    // Return new bounded range
                    Ok(Value::Range {
                        start: new_start,
                        end: Some(*e),
                        inclusive: *inclusive,
                    })
                }
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
        Value::LazySequence(lazy_seq) => {
            // Wrap in LazySeq::Skip for lazy composition
            Ok(Value::LazySequence(Rc::new(RefCell::new(LazySeq::Skip {
                source: lazy_seq.clone(),
                remaining: n,
            }))))
        }
        _ => Err(RuntimeError::new(
            format!("skip does not support {}", collection.type_name()),
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
            None => Err(RuntimeError::new("Cannot reverse unbounded range", line)),
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
            ));
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
        Value::Integer(_) => return Err(RuntimeError::new("chunk size must be positive", line)),
        _ => {
            return Err(RuntimeError::new(
                format!("chunk expects Integer, got {}", size.type_name()),
                line,
            ));
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
        Value::String(s) => {
            let graphemes: Vec<&str> = s.graphemes(true).collect();
            let mut result = Vector::new();

            for chunk in graphemes.chunks(chunk_size) {
                let chunk_str: String = chunk.concat();
                result.push_back(Value::String(Rc::new(chunk_str)));
            }

            Ok(Value::List(result))
        }
        _ => Err(RuntimeError::new(
            format!(
                "chunk expects List or String, got {}",
                collection.type_name()
            ),
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

fn add_to_set(
    result: &mut HashSet<Value>,
    collection: &Value,
    line: u32,
) -> Result<(), RuntimeError> {
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
// NOTE: includes? and excludes? are now callback builtins in runtime.rs
// to support LazySequence iteration.

// ============================================================================
// Lazy Sequence Generation (§11.12)
// ============================================================================

use super::value::{FlatMapInner, LazySeq};
use std::cell::RefCell;

/// repeat(value) → LazySequence
/// Generate a lazy sequence that repeats value indefinitely.
/// Per LANG.txt §11.12
fn builtin_repeat(value: &Value, _line: u32) -> Result<Value, RuntimeError> {
    Ok(Value::LazySequence(Rc::new(RefCell::new(
        LazySeq::Repeat {
            value: value.clone(),
        },
    ))))
}

/// cycle(collection) → LazySequence
/// Generate a lazy sequence that cycles through elements indefinitely.
/// Per LANG.txt §11.12
fn builtin_cycle(collection: &Value, line: u32) -> Result<Value, RuntimeError> {
    let elements: Vector<Value> = match collection {
        Value::List(list) => {
            if list.is_empty() {
                return Err(RuntimeError::new("cycle on empty list", line));
            }
            list.clone()
        }
        Value::String(s) => {
            use unicode_segmentation::UnicodeSegmentation;
            let graphemes: Vec<&str> = s.graphemes(true).collect();
            if graphemes.is_empty() {
                return Err(RuntimeError::new("cycle on empty string", line));
            }
            graphemes
                .into_iter()
                .map(|g| Value::String(Rc::new(g.to_string())))
                .collect()
        }
        _ => {
            return Err(RuntimeError::new(
                format!(
                    "cycle expects List or String, got {}",
                    collection.type_name()
                ),
                line,
            ));
        }
    };

    Ok(Value::LazySequence(Rc::new(RefCell::new(LazySeq::Cycle {
        source: elements,
        index: 0,
    }))))
}

/// zip(collection, ..collections) → List | LazySequence
/// Aggregate multiple collections into tuples.
/// Per LANG.txt §11.12
fn builtin_zip(args: &[Value], line: u32) -> Result<Value, RuntimeError> {
    if args.is_empty() {
        return Err(RuntimeError::new(
            "zip requires at least one argument",
            line,
        ));
    }

    // Per LANG.txt §11.12:
    // - If ANY collection has finite size → returns List
    // - If ALL collections are infinite → returns LazySequence
    let all_infinite = args
        .iter()
        .all(|arg| matches!(arg, Value::Range { end: None, .. } | Value::LazySequence(_)));

    if all_infinite {
        // Create lazy sequence only when ALL are infinite
        let mut sources: Vec<Rc<RefCell<LazySeq>>> = Vec::new();
        for arg in args {
            let lazy = value_to_lazy_seq_for_zip(arg, line)?;
            sources.push(Rc::new(RefCell::new(lazy)));
        }
        Ok(Value::LazySequence(Rc::new(RefCell::new(LazySeq::Zip {
            sources,
        }))))
    } else {
        // At least one collection is finite - materialize to List
        // First, find the minimum length from finite collections
        let min_len = args
            .iter()
            .filter_map(collection_finite_len)
            .min()
            .unwrap_or(0);

        // Now collect elements up to min_len from each collection
        let mut collections: Vec<Vec<Value>> = Vec::new();
        for arg in args {
            let elements = collection_take_n(arg, min_len, line)?;
            collections.push(elements);
        }

        let mut result = Vector::new();
        for i in 0..min_len {
            let mut tuple = Vector::new();
            for collection in &collections {
                tuple.push_back(collection[i].clone());
            }
            result.push_back(Value::List(tuple));
        }

        Ok(Value::List(result))
    }
}
/// Get the finite length of a collection, or None if infinite
fn collection_finite_len(value: &Value) -> Option<usize> {
    match value {
        Value::List(list) => Some(list.len()),
        Value::String(s) => {
            use unicode_segmentation::UnicodeSegmentation;
            Some(s.graphemes(true).count())
        }
        Value::Set(set) => Some(set.len()),
        Value::Range {
            start,
            end,
            inclusive,
        } => match end {
            Some(e) => {
                // For exclusive ranges where start >= end, the range is empty
                if !*inclusive && *start >= *e {
                    return Some(0);
                }
                let actual_end = if *inclusive { *e } else { e - 1 };
                if start <= &actual_end {
                    Some((actual_end - start + 1) as usize)
                } else {
                    // Descending range (inclusive only reaches here)
                    Some((start - actual_end + 1) as usize)
                }
            }
            None => None, // Unbounded range is infinite
        },
        Value::LazySequence(_) => None, // Lazy sequences are considered infinite
        _ => Some(0),
    }
}

/// Take n elements from a collection (works for both finite and infinite)
fn collection_take_n(value: &Value, n: usize, line: u32) -> Result<Vec<Value>, RuntimeError> {
    match value {
        Value::List(list) => Ok(list.iter().take(n).cloned().collect()),
        Value::String(s) => Ok(s
            .graphemes(true)
            .take(n)
            .map(|g| Value::String(Rc::new(g.to_string())))
            .collect()),
        Value::Set(set) => Ok(set.iter().take(n).cloned().collect()),
        Value::Range {
            start,
            end,
            inclusive,
        } => {
            let mut result = Vec::new();
            match end {
                Some(e) => {
                    let actual_end = if *inclusive { *e } else { e - 1 };
                    if start <= &actual_end {
                        for i in (*start..=actual_end).take(n) {
                            result.push(Value::Integer(i));
                        }
                    } else {
                        let mut i = *start;
                        let mut count = 0;
                        while i >= actual_end && count < n {
                            result.push(Value::Integer(i));
                            i -= 1;
                            count += 1;
                        }
                    }
                }
                None => {
                    // Unbounded range - take n from start
                    for i in 0..n {
                        result.push(Value::Integer(start + i as i64));
                    }
                }
            }
            Ok(result)
        }
        Value::LazySequence(lazy_seq) => {
            // For lazy sequences, we need to consume n elements
            // Note: This only works for non-callback sequences (Repeat, Cycle, Range)
            let mut result = Vec::new();
            let mut seq = lazy_seq.borrow_mut();
            for _ in 0..n {
                match lazy_seq_next_simple(&mut seq)? {
                    Some(val) => result.push(val),
                    None => break,
                }
            }
            Ok(result)
        }
        _ => Err(RuntimeError::new(
            format!("Cannot take from {}", value.type_name()),
            line,
        )),
    }
}

/// Convert a value to LazySeq for zip (handles the infinite cases)
fn value_to_lazy_seq_for_zip(value: &Value, line: u32) -> Result<LazySeq, RuntimeError> {
    match value {
        Value::Range {
            start,
            end,
            inclusive,
        } => {
            let step = match end {
                Some(e) if *e < *start => -1,
                _ => 1,
            };
            Ok(LazySeq::Range {
                current: *start,
                end: *end,
                inclusive: *inclusive,
                step,
            })
        }
        Value::LazySequence(seq) => Ok(seq.borrow().clone()),
        _ => Err(RuntimeError::new(
            format!("Unexpected value type for lazy zip: {}", value.type_name()),
            line,
        )),
    }
}

/// combinations(size, collection) → LazySequence
/// Generate all combinations of given size from collection.
/// Per LANG.txt §11.12
fn builtin_combinations(
    size_val: &Value,
    collection: &Value,
    line: u32,
) -> Result<Value, RuntimeError> {
    let size = match size_val {
        Value::Integer(n) if *n >= 0 => *n as usize,
        _ => {
            return Err(RuntimeError::new(
                "combinations expects non-negative Integer as first argument",
                line,
            ));
        }
    };

    let elements: Vec<Value> = match collection {
        Value::List(list) => list.iter().cloned().collect(),
        _ => {
            return Err(RuntimeError::new(
                format!("combinations expects List, got {}", collection.type_name()),
                line,
            ));
        }
    };

    if size > elements.len() {
        // No combinations possible - return empty lazy sequence
        return Ok(Value::LazySequence(Rc::new(RefCell::new(LazySeq::Empty))));
    }

    if size == 0 {
        // Special case: single empty combination
        let mut result = Vector::new();
        result.push_back(Value::List(Vector::new()));
        return Ok(Value::List(result));
    }

    // Initialize indices: [0, 1, 2, ..., size-1]
    let indices: Vec<usize> = (0..size).collect();

    Ok(Value::LazySequence(Rc::new(RefCell::new(
        LazySeq::Combinations {
            source: elements,
            size,
            indices,
        },
    ))))
}

// ============================================================================
// Range Generation (§11.13)
// ============================================================================

/// range(from, to, step) → LazySequence
/// Generate a range with custom step value.
/// Per LANG.txt §11.13
fn builtin_range_fn(
    from: &Value,
    to: &Value,
    step: &Value,
    line: u32,
) -> Result<Value, RuntimeError> {
    let from_val = match from {
        Value::Integer(n) => *n,
        _ => {
            return Err(RuntimeError::new(
                format!(
                    "range expects Integer as first argument, got {}",
                    from.type_name()
                ),
                line,
            ));
        }
    };

    let to_val = match to {
        Value::Integer(n) => *n,
        _ => {
            return Err(RuntimeError::new(
                format!(
                    "range expects Integer as second argument, got {}",
                    to.type_name()
                ),
                line,
            ));
        }
    };

    let step_val = match step {
        Value::Integer(n) => *n,
        _ => {
            return Err(RuntimeError::new(
                format!(
                    "range expects Integer as third argument, got {}",
                    step.type_name()
                ),
                line,
            ));
        }
    };

    // Validate step
    if step_val == 0 {
        return Err(RuntimeError::new("range: step cannot be zero", line));
    }

    // Validate step direction matches from->to direction
    if (from_val < to_val && step_val < 0) || (from_val > to_val && step_val > 0) {
        return Err(RuntimeError::new("range: step direction mismatch", line));
    }

    Ok(Value::LazySequence(Rc::new(RefCell::new(
        LazySeq::RangeStep {
            current: from_val,
            end: to_val,
            step: step_val,
        },
    ))))
}

// ============================================================================
// Clone implementation for LazySeq
// ============================================================================

impl Clone for LazySeq {
    fn clone(&self) -> Self {
        match self {
            LazySeq::Range {
                current,
                end,
                inclusive,
                step,
            } => LazySeq::Range {
                current: *current,
                end: *end,
                inclusive: *inclusive,
                step: *step,
            },
            LazySeq::RangeStep { current, end, step } => LazySeq::RangeStep {
                current: *current,
                end: *end,
                step: *step,
            },
            LazySeq::Repeat { value } => LazySeq::Repeat {
                value: value.clone(),
            },
            LazySeq::Cycle { source, index } => LazySeq::Cycle {
                source: source.clone(),
                index: *index,
            },
            LazySeq::Iterate { generator, current } => LazySeq::Iterate {
                generator: generator.clone(),
                current: current.clone(),
            },
            LazySeq::Map { source, mapper } => LazySeq::Map {
                source: Rc::new(RefCell::new(source.borrow().clone())),
                mapper: mapper.clone(),
            },
            LazySeq::Filter { source, predicate } => LazySeq::Filter {
                source: Rc::new(RefCell::new(source.borrow().clone())),
                predicate: predicate.clone(),
            },
            LazySeq::FilterMap { source, mapper } => LazySeq::FilterMap {
                source: Rc::new(RefCell::new(source.borrow().clone())),
                mapper: mapper.clone(),
            },
            LazySeq::FlatMap {
                source,
                mapper,
                current_inner,
            } => LazySeq::FlatMap {
                source: Rc::new(RefCell::new(source.borrow().clone())),
                mapper: mapper.clone(),
                current_inner: current_inner.as_ref().map(|inner| {
                    Box::new(match inner.as_ref() {
                        FlatMapInner::List { items, index } => FlatMapInner::List {
                            items: items.clone(),
                            index: *index,
                        },
                        FlatMapInner::LazySeq(seq) => {
                            FlatMapInner::LazySeq(Rc::new(RefCell::new(seq.borrow().clone())))
                        }
                    })
                }),
            },
            LazySeq::Skip { source, remaining } => LazySeq::Skip {
                source: Rc::new(RefCell::new(source.borrow().clone())),
                remaining: *remaining,
            },
            LazySeq::Scan {
                source,
                folder,
                accumulator,
                initial,
                emitted_initial,
            } => LazySeq::Scan {
                source: Rc::new(RefCell::new(source.borrow().clone())),
                folder: folder.clone(),
                accumulator: accumulator.clone(),
                initial: initial.clone(),
                emitted_initial: *emitted_initial,
            },
            LazySeq::Zip { sources } => LazySeq::Zip {
                sources: sources
                    .iter()
                    .map(|s| Rc::new(RefCell::new(s.borrow().clone())))
                    .collect(),
            },
            LazySeq::Combinations {
                source,
                size,
                indices,
            } => LazySeq::Combinations {
                source: source.clone(),
                size: *size,
                indices: indices.clone(),
            },
            LazySeq::Empty => LazySeq::Empty,
        }
    }
}

/// Simple lazy sequence iteration for non-callback sequences
/// Returns Ok(Some(value)) for next element, Ok(None) if exhausted,
/// Err if sequence requires callbacks (Iterate, Map, Filter)
fn lazy_seq_next_simple(seq: &mut LazySeq) -> Result<Option<Value>, RuntimeError> {
    match seq {
        LazySeq::Range {
            current,
            end,
            inclusive,
            step,
        } => {
            if let Some(e) = end {
                let actual_end = if *step > 0 {
                    if *inclusive { *e } else { *e - 1 }
                } else if *inclusive {
                    *e
                } else {
                    *e + 1
                };

                if *step > 0 {
                    // Ascending
                    if *current > actual_end {
                        return Ok(None);
                    }
                } else {
                    // Descending
                    if *current < actual_end {
                        return Ok(None);
                    }
                }

                let val = *current;
                *current += *step;
                Ok(Some(Value::Integer(val)))
            } else {
                // Unbounded range (always ascending)
                let val = *current;
                *current += *step;
                Ok(Some(Value::Integer(val)))
            }
        }
        LazySeq::RangeStep { current, end, step } => {
            if *step > 0 {
                if current < end {
                    let val = *current;
                    *current += *step;
                    Ok(Some(Value::Integer(val)))
                } else {
                    Ok(None)
                }
            } else if *step < 0 {
                if current > end {
                    let val = *current;
                    *current += *step;
                    Ok(Some(Value::Integer(val)))
                } else {
                    Ok(None)
                }
            } else {
                Ok(None) // step == 0, shouldn't happen
            }
        }
        LazySeq::Repeat { value } => Ok(Some(value.clone())),
        LazySeq::Cycle { source, index } => {
            if source.is_empty() {
                Ok(None)
            } else {
                let val = source[*index % source.len()].clone();
                *index += 1;
                Ok(Some(val))
            }
        }
        LazySeq::Combinations {
            source,
            size,
            indices,
        } => {
            if *size == 0 || *size > source.len() {
                return Ok(None);
            }
            if indices.is_empty() {
                // Initialize indices for first combination
                *indices = (0..*size).collect();
            } else {
                // Find next combination
                let n = source.len();
                let mut i = *size;
                while i > 0 {
                    i -= 1;
                    if indices[i] < n - *size + i {
                        indices[i] += 1;
                        for j in (i + 1)..*size {
                            indices[j] = indices[j - 1] + 1;
                        }
                        break;
                    }
                    if i == 0 {
                        *seq = LazySeq::Empty;
                        return Ok(None);
                    }
                }
            }
            let combo: Vector<Value> = indices.iter().map(|&i| source[i].clone()).collect();
            Ok(Some(Value::List(combo)))
        }
        LazySeq::Empty => Ok(None),
        // Callback-requiring sequences cannot be iterated without VM
        LazySeq::Iterate { .. }
        | LazySeq::Map { .. }
        | LazySeq::Filter { .. }
        | LazySeq::FilterMap { .. }
        | LazySeq::FlatMap { .. }
        | LazySeq::Scan { .. } => Err(RuntimeError::new(
            "Cannot iterate callback-based lazy sequence in this context",
            0,
        )),
        LazySeq::Skip { source, remaining } => {
            // Skip remaining elements first
            while *remaining > 0 {
                let inner_result = {
                    let mut inner = source.borrow_mut();
                    lazy_seq_next_simple(&mut inner)?
                };
                if inner_result.is_none() {
                    *seq = LazySeq::Empty;
                    return Ok(None);
                }
                *remaining -= 1;
            }
            // Then return from source
            let mut inner = source.borrow_mut();
            lazy_seq_next_simple(&mut inner)
        }
        LazySeq::Zip { sources } => {
            let mut tuple = Vector::new();
            for src in sources.iter() {
                let result = {
                    let mut inner = src.borrow_mut();
                    lazy_seq_next_simple(&mut inner)?
                };
                match result {
                    Some(val) => tuple.push_back(val),
                    None => return Ok(None),
                }
            }
            Ok(Some(Value::List(tuple)))
        }
    }
}

// ============================================================================
// String Functions (§11.14)
// ============================================================================

/// lines(string) → List[String]
/// Split string on newline characters. Per LANG.txt §11.14
fn builtin_lines(value: &Value, line: u32) -> Result<Value, RuntimeError> {
    match value {
        Value::String(s) => {
            let lines: Vector<Value> = s
                .split('\n')
                .map(|line| Value::String(Rc::new(line.to_string())))
                .collect();
            Ok(Value::List(lines))
        }
        _ => Err(RuntimeError::new(
            format!("lines expects String, got {}", value.type_name()),
            line,
        )),
    }
}

/// upper(string) → String
/// Convert string to uppercase. Per LANG.txt §11.14
fn builtin_upper(value: &Value, line: u32) -> Result<Value, RuntimeError> {
    match value {
        Value::String(s) => Ok(Value::String(Rc::new(s.to_uppercase()))),
        _ => Err(RuntimeError::new(
            format!("upper expects String, got {}", value.type_name()),
            line,
        )),
    }
}

/// lower(string) → String
/// Convert string to lowercase. Per LANG.txt §11.14
fn builtin_lower(value: &Value, line: u32) -> Result<Value, RuntimeError> {
    match value {
        Value::String(s) => Ok(Value::String(Rc::new(s.to_lowercase()))),
        _ => Err(RuntimeError::new(
            format!("lower expects String, got {}", value.type_name()),
            line,
        )),
    }
}

/// md5(string) → String
/// Compute MD5 hash of string and return as lowercase hexadecimal.
fn builtin_md5(value: &Value, line: u32) -> Result<Value, RuntimeError> {
    use md5::{Digest, Md5};
    match value {
        Value::String(s) => {
            let mut hasher = Md5::new();
            hasher.update(s.as_bytes());
            let digest = hasher.finalize();
            Ok(Value::String(Rc::new(format!("{:x}", digest))))
        }
        _ => Err(RuntimeError::new(
            format!("md5 expects String, got {}", value.type_name()),
            line,
        )),
    }
}

/// replace(pattern, replacement, string) → String
/// Replace all occurrences of pattern with replacement. Per LANG.txt §11.14
fn builtin_replace(
    pattern: &Value,
    replacement: &Value,
    string: &Value,
    line: u32,
) -> Result<Value, RuntimeError> {
    match (pattern, replacement, string) {
        (Value::String(pat), Value::String(repl), Value::String(s)) => Ok(Value::String(Rc::new(
            s.replace(pat.as_str(), repl.as_str()),
        ))),
        (Value::String(_), Value::String(_), _) => Err(RuntimeError::new(
            format!(
                "replace expects String as third argument, got {}",
                string.type_name()
            ),
            line,
        )),
        (Value::String(_), _, _) => Err(RuntimeError::new(
            format!(
                "replace expects String as second argument, got {}",
                replacement.type_name()
            ),
            line,
        )),
        _ => Err(RuntimeError::new(
            format!(
                "replace expects String as first argument, got {}",
                pattern.type_name()
            ),
            line,
        )),
    }
}

/// split(separator, string) → List[String]
/// Split string by separator. Per LANG.txt §11.14
fn builtin_split(separator: &Value, string: &Value, line: u32) -> Result<Value, RuntimeError> {
    match (separator, string) {
        (Value::String(sep), Value::String(s)) => {
            let parts: Vector<Value> = if sep.is_empty() {
                // Split into grapheme clusters
                s.graphemes(true)
                    .map(|g| Value::String(Rc::new(g.to_string())))
                    .collect()
            } else {
                s.split(sep.as_str())
                    .map(|part| Value::String(Rc::new(part.to_string())))
                    .collect()
            };
            Ok(Value::List(parts))
        }
        (Value::String(_), _) => Err(RuntimeError::new(
            format!(
                "split expects String as second argument, got {}",
                string.type_name()
            ),
            line,
        )),
        _ => Err(RuntimeError::new(
            format!(
                "split expects String as first argument, got {}",
                separator.type_name()
            ),
            line,
        )),
    }
}

/// regex_match(pattern, string) → List[String]
/// Match and return capture groups only. Per LANG.txt §11.14
fn builtin_regex_match(pattern: &Value, string: &Value, line: u32) -> Result<Value, RuntimeError> {
    match (pattern, string) {
        (Value::String(pat), Value::String(s)) => {
            let re = Regex::new(pat)
                .map_err(|e| RuntimeError::new(format!("Invalid regex pattern: {}", e), line))?;

            let captures: Vector<Value> = match re.captures(s) {
                Some(caps) => caps
                    .iter()
                    .skip(1) // Skip the full match (group 0)
                    .filter_map(|m| m.map(|m| Value::String(Rc::new(m.as_str().to_string()))))
                    .collect(),
                None => Vector::new(),
            };

            Ok(Value::List(captures))
        }
        (Value::String(_), _) => Err(RuntimeError::new(
            format!(
                "regex_match expects String as second argument, got {}",
                string.type_name()
            ),
            line,
        )),
        _ => Err(RuntimeError::new(
            format!(
                "regex_match expects String as first argument, got {}",
                pattern.type_name()
            ),
            line,
        )),
    }
}

/// regex_match_all(pattern, string) → List[String]
/// Match all occurrences. Per LANG.txt §11.14
fn builtin_regex_match_all(
    pattern: &Value,
    string: &Value,
    line: u32,
) -> Result<Value, RuntimeError> {
    match (pattern, string) {
        (Value::String(pat), Value::String(s)) => {
            let re = Regex::new(pat)
                .map_err(|e| RuntimeError::new(format!("Invalid regex pattern: {}", e), line))?;

            let matches: Vector<Value> = re
                .find_iter(s)
                .map(|m| Value::String(Rc::new(m.as_str().to_string())))
                .collect();

            Ok(Value::List(matches))
        }
        (Value::String(_), _) => Err(RuntimeError::new(
            format!(
                "regex_match_all expects String as second argument, got {}",
                string.type_name()
            ),
            line,
        )),
        _ => Err(RuntimeError::new(
            format!(
                "regex_match_all expects String as first argument, got {}",
                pattern.type_name()
            ),
            line,
        )),
    }
}

/// Helper to convert a Value to an unquoted string representation
fn value_to_unquoted_string(value: &Value) -> String {
    match value {
        Value::String(s) => s.to_string(),
        other => other.to_string(),
    }
}

/// join(separator, collection) → String
/// Join collection elements into a string with separator. Per LANG.txt §11.14
fn builtin_join(separator: &Value, collection: &Value, line: u32) -> Result<Value, RuntimeError> {
    match (separator, collection) {
        (Value::String(sep), Value::List(list)) => {
            let strings: Vec<String> = list.iter().map(value_to_unquoted_string).collect();
            Ok(Value::String(Rc::new(strings.join(sep.as_str()))))
        }
        (Value::String(sep), Value::Set(set)) => {
            let strings: Vec<String> = set.iter().map(value_to_unquoted_string).collect();
            Ok(Value::String(Rc::new(strings.join(sep.as_str()))))
        }
        (Value::String(_), _) => Err(RuntimeError::new(
            format!(
                "join expects List or Set as second argument, got {}",
                collection.type_name()
            ),
            line,
        )),
        _ => Err(RuntimeError::new(
            format!(
                "join expects String as first argument, got {}",
                separator.type_name()
            ),
            line,
        )),
    }
}

// ============================================================================
// Math Functions (§11.15)
// ============================================================================

/// abs(value) → Integer | Decimal
/// Return absolute value. Per LANG.txt §11.15
fn builtin_abs(value: &Value, line: u32) -> Result<Value, RuntimeError> {
    match value {
        Value::Integer(n) => Ok(Value::Integer(n.abs())),
        Value::Decimal(d) => Ok(Value::Decimal(OrderedFloat(d.0.abs()))),
        _ => Err(RuntimeError::new(
            format!("abs expects Integer or Decimal, got {}", value.type_name()),
            line,
        )),
    }
}

/// signum(value) → Integer
/// Return sign of number: -1, 0, or 1. Per LANG.txt §11.15
fn builtin_signum(value: &Value, line: u32) -> Result<Value, RuntimeError> {
    match value {
        Value::Integer(n) => Ok(Value::Integer(n.signum())),
        Value::Decimal(d) => {
            let sign = if d.0 > 0.0 {
                1
            } else if d.0 < 0.0 {
                -1
            } else {
                0
            };
            Ok(Value::Integer(sign))
        }
        _ => Err(RuntimeError::new(
            format!(
                "signum expects Integer or Decimal, got {}",
                value.type_name()
            ),
            line,
        )),
    }
}

/// vec_add(a, b) → List
/// Element-wise vector addition. Per LANG.txt §11.15
fn builtin_vec_add(a: &Value, b: &Value, line: u32) -> Result<Value, RuntimeError> {
    match (a, b) {
        (Value::List(list_a), Value::List(list_b)) => {
            let result: Vector<Value> = list_a
                .iter()
                .zip(list_b.iter())
                .map(|(va, vb)| match (va, vb) {
                    (Value::Integer(na), Value::Integer(nb)) => Value::Integer(na + nb),
                    (Value::Integer(na), Value::Decimal(db)) => {
                        Value::Decimal(OrderedFloat(*na as f64 + db.0))
                    }
                    (Value::Decimal(da), Value::Integer(nb)) => {
                        Value::Decimal(OrderedFloat(da.0 + *nb as f64))
                    }
                    (Value::Decimal(da), Value::Decimal(db)) => {
                        Value::Decimal(OrderedFloat(da.0 + db.0))
                    }
                    _ => Value::Nil, // Non-numeric values become nil
                })
                .collect();
            Ok(Value::List(result))
        }
        (Value::List(_), _) => Err(RuntimeError::new(
            format!(
                "vec_add expects List as second argument, got {}",
                b.type_name()
            ),
            line,
        )),
        _ => Err(RuntimeError::new(
            format!(
                "vec_add expects List as first argument, got {}",
                a.type_name()
            ),
            line,
        )),
    }
}

// ============================================================================
// Bitwise Functions (§4.5)
// ============================================================================

/// bit_and(a, b) → Integer
/// Bitwise AND. Per LANG.txt §4.5
fn builtin_bit_and(a: &Value, b: &Value, line: u32) -> Result<Value, RuntimeError> {
    match (a, b) {
        (Value::Integer(na), Value::Integer(nb)) => Ok(Value::Integer(na & nb)),
        (Value::Integer(_), _) => Err(RuntimeError::new(
            format!(
                "bit_and expects Integer as second argument, got {}",
                b.type_name()
            ),
            line,
        )),
        _ => Err(RuntimeError::new(
            format!(
                "bit_and expects Integer as first argument, got {}",
                a.type_name()
            ),
            line,
        )),
    }
}

/// bit_or(a, b) → Integer
/// Bitwise OR. Per LANG.txt §4.5
fn builtin_bit_or(a: &Value, b: &Value, line: u32) -> Result<Value, RuntimeError> {
    match (a, b) {
        (Value::Integer(na), Value::Integer(nb)) => Ok(Value::Integer(na | nb)),
        (Value::Integer(_), _) => Err(RuntimeError::new(
            format!(
                "bit_or expects Integer as second argument, got {}",
                b.type_name()
            ),
            line,
        )),
        _ => Err(RuntimeError::new(
            format!(
                "bit_or expects Integer as first argument, got {}",
                a.type_name()
            ),
            line,
        )),
    }
}

/// bit_xor(a, b) → Integer
/// Bitwise XOR. Per LANG.txt §4.5
fn builtin_bit_xor(a: &Value, b: &Value, line: u32) -> Result<Value, RuntimeError> {
    match (a, b) {
        (Value::Integer(na), Value::Integer(nb)) => Ok(Value::Integer(na ^ nb)),
        (Value::Integer(_), _) => Err(RuntimeError::new(
            format!(
                "bit_xor expects Integer as second argument, got {}",
                b.type_name()
            ),
            line,
        )),
        _ => Err(RuntimeError::new(
            format!(
                "bit_xor expects Integer as first argument, got {}",
                a.type_name()
            ),
            line,
        )),
    }
}

/// bit_not(value) → Integer
/// Bitwise NOT (complement). Per LANG.txt §4.5
fn builtin_bit_not(value: &Value, line: u32) -> Result<Value, RuntimeError> {
    match value {
        Value::Integer(n) => Ok(Value::Integer(!n)),
        _ => Err(RuntimeError::new(
            format!("bit_not expects Integer, got {}", value.type_name()),
            line,
        )),
    }
}

/// bit_shift_left(value, shift) → Integer
/// Left shift. Per LANG.txt §4.5
fn builtin_bit_shift_left(value: &Value, shift: &Value, line: u32) -> Result<Value, RuntimeError> {
    match (value, shift) {
        (Value::Integer(n), Value::Integer(s)) => {
            if *s < 0 {
                Err(RuntimeError::new(
                    "bit_shift_left shift amount must be non-negative".to_string(),
                    line,
                ))
            } else if *s >= 64 {
                Ok(Value::Integer(0)) // Shift more than width gives 0
            } else {
                Ok(Value::Integer(n << s))
            }
        }
        (Value::Integer(_), _) => Err(RuntimeError::new(
            format!(
                "bit_shift_left expects Integer as second argument, got {}",
                shift.type_name()
            ),
            line,
        )),
        _ => Err(RuntimeError::new(
            format!(
                "bit_shift_left expects Integer as first argument, got {}",
                value.type_name()
            ),
            line,
        )),
    }
}

/// bit_shift_right(value, shift) → Integer
/// Right shift (arithmetic). Per LANG.txt §4.5
fn builtin_bit_shift_right(value: &Value, shift: &Value, line: u32) -> Result<Value, RuntimeError> {
    match (value, shift) {
        (Value::Integer(n), Value::Integer(s)) => {
            if *s < 0 {
                Err(RuntimeError::new(
                    "bit_shift_right shift amount must be non-negative".to_string(),
                    line,
                ))
            } else if *s >= 64 {
                // Arithmetic shift: preserve sign
                Ok(Value::Integer(if *n < 0 { -1 } else { 0 }))
            } else {
                Ok(Value::Integer(n >> s))
            }
        }
        (Value::Integer(_), _) => Err(RuntimeError::new(
            format!(
                "bit_shift_right expects Integer as second argument, got {}",
                shift.type_name()
            ),
            line,
        )),
        _ => Err(RuntimeError::new(
            format!(
                "bit_shift_right expects Integer as first argument, got {}",
                value.type_name()
            ),
            line,
        )),
    }
}

// ============================================================================
// Utility Functions (§11.16)
// ============================================================================

/// id(value) → Value
/// Identity function. Per LANG.txt §11.16
fn builtin_id(value: &Value, _line: u32) -> Result<Value, RuntimeError> {
    Ok(value.clone())
}

/// type(value) → String
/// Get type name. Per LANG.txt §11.16
fn builtin_type(value: &Value, _line: u32) -> Result<Value, RuntimeError> {
    Ok(Value::String(Rc::new(value.type_name().to_string())))
}
