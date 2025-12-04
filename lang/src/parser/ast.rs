use crate::lexer::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
}

pub type SpannedExpr = Spanned<Expr>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Literals
    Integer(i64),
    Decimal(f64),
    String(String),
    Boolean(bool),
    Nil,

    // Collections
    List(Vec<SpannedExpr>),
    Set(Vec<SpannedExpr>),
    Dict(Vec<(SpannedExpr, SpannedExpr)>),

    // Identifiers & Placeholders
    Identifier(String),
    Placeholder,

    // Operations
    Prefix {
        op: PrefixOp,
        right: Box<SpannedExpr>,
    },
    Infix {
        left: Box<SpannedExpr>,
        op: InfixOp,
        right: Box<SpannedExpr>,
    },
    Index {
        collection: Box<SpannedExpr>,
        index: Box<SpannedExpr>,
    },

    // Functions
    Function {
        params: Vec<Param>,
        body: Box<SpannedExpr>,
    },
    Call {
        function: Box<SpannedExpr>,
        args: Vec<SpannedExpr>,
    },
    InfixCall {
        function: String,
        left: Box<SpannedExpr>,
        right: Box<SpannedExpr>,
    },

    // Control Flow
    If {
        condition: Box<SpannedExpr>,
        then_branch: Box<SpannedExpr>,
        else_branch: Option<Box<SpannedExpr>>,
    },
    IfLet {
        pattern: Pattern,
        value: Box<SpannedExpr>,
        then_branch: Box<SpannedExpr>,
        else_branch: Option<Box<SpannedExpr>>,
    },
    Match {
        subject: Box<SpannedExpr>,
        arms: Vec<MatchArm>,
    },
    Block(Vec<SpannedStmt>),

    // Assignment
    Assignment {
        name: String,
        value: Box<SpannedExpr>,
    },

    // Spread (for list/dict/call arguments)
    Spread(Box<SpannedExpr>),

    // Range
    Range {
        start: Box<SpannedExpr>,
        end: Option<Box<SpannedExpr>>,
        inclusive: bool,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefixOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InfixOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,

    // Logical
    And,
    Or,

    // Pipeline/Composition
    Pipeline,
    Compose,
}

impl std::fmt::Display for PrefixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrefixOp::Neg => write!(f, "-"),
            PrefixOp::Not => write!(f, "!"),
        }
    }
}

impl std::fmt::Display for InfixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixOp::Add => write!(f, "+"),
            InfixOp::Sub => write!(f, "-"),
            InfixOp::Mul => write!(f, "*"),
            InfixOp::Div => write!(f, "/"),
            InfixOp::Mod => write!(f, "%"),
            InfixOp::Eq => write!(f, "=="),
            InfixOp::Ne => write!(f, "!="),
            InfixOp::Lt => write!(f, "<"),
            InfixOp::Le => write!(f, "<="),
            InfixOp::Gt => write!(f, ">"),
            InfixOp::Ge => write!(f, ">="),
            InfixOp::And => write!(f, "&&"),
            InfixOp::Or => write!(f, "||"),
            InfixOp::Pipeline => write!(f, "|>"),
            InfixOp::Compose => write!(f, ">>"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: ParamKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParamKind {
    Identifier(String),
    Placeholder,
    Rest(String),
    Pattern(Pattern),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<SpannedExpr>,
    pub body: SpannedExpr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard,
    Identifier(String),
    RestIdentifier(String),
    Literal(LiteralPattern),
    List(Vec<Pattern>),
    Range {
        start: i64,
        end: Option<i64>,
        inclusive: bool,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralPattern {
    Integer(i64),
    Decimal(f64),
    String(String),
    Boolean(bool),
    Nil,
}

pub type SpannedStmt = Spanned<Stmt>;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let {
        mutable: bool,
        pattern: Pattern,
        value: SpannedExpr,
    },
    Return(SpannedExpr),
    Break(SpannedExpr),
    Expr(SpannedExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Section {
    Input(SpannedExpr),
    PartOne(SpannedExpr),
    PartTwo(SpannedExpr),
    Test {
        attributes: Vec<Attribute>,
        input: SpannedExpr,
        part_one: Option<SpannedExpr>,
        part_two: Option<SpannedExpr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<SpannedStmt>,
    pub sections: Vec<Section>,
}
