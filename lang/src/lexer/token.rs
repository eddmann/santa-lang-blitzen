#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords
    Let,
    Mut,
    If,
    Else,
    Match,
    Return,
    Break,
    Nil,
    True,
    False,

    // Literals
    Integer(i64),
    Decimal(f64),
    String(String),

    // Identifiers
    Identifier(String),

    // Operators
    Plus,           // +
    Minus,          // -
    Star,           // *
    Slash,          // /
    Percent,        // %
    Bang,           // !
    Equal,          // =
    EqualEqual,     // ==
    BangEqual,      // !=
    Less,           // <
    LessEqual,      // <=
    Greater,        // >
    GreaterEqual,   // >=
    AmpAmp,         // &&
    PipePipe,       // ||
    PipeGreater,    // |>
    GreaterGreater, // >>
    DotDot,         // ..
    DotDotEqual,    // ..=

    // Delimiters
    LeftParen,    // (
    RightParen,   // )
    LeftBracket,  // [
    RightBracket, // ]
    LeftBrace,    // {
    RightBrace,   // }
    HashBrace,    // #{
    Comma,        // ,
    Colon,        // :
    Semicolon,    // ;
    Pipe,         // |
    Backtick,     // `

    // Special
    Underscore, // _ (placeholder/wildcard)
    At,         // @ (attribute prefix)

    // End of file
    Eof,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Let => write!(f, "let"),
            TokenKind::Mut => write!(f, "mut"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::Match => write!(f, "match"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::Break => write!(f, "break"),
            TokenKind::Nil => write!(f, "nil"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Integer(n) => write!(f, "{n}"),
            TokenKind::Decimal(n) => write!(f, "{n}"),
            TokenKind::String(s) => write!(f, "\"{s}\""),
            TokenKind::Identifier(s) => write!(f, "{s}"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Equal => write!(f, "="),
            TokenKind::EqualEqual => write!(f, "=="),
            TokenKind::BangEqual => write!(f, "!="),
            TokenKind::Less => write!(f, "<"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::AmpAmp => write!(f, "&&"),
            TokenKind::PipePipe => write!(f, "||"),
            TokenKind::PipeGreater => write!(f, "|>"),
            TokenKind::GreaterGreater => write!(f, ">>"),
            TokenKind::DotDot => write!(f, ".."),
            TokenKind::DotDotEqual => write!(f, "..="),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::LeftBracket => write!(f, "["),
            TokenKind::RightBracket => write!(f, "]"),
            TokenKind::LeftBrace => write!(f, "{{"),
            TokenKind::RightBrace => write!(f, "}}"),
            TokenKind::HashBrace => write!(f, "#{{"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Pipe => write!(f, "|"),
            TokenKind::Backtick => write!(f, "`"),
            TokenKind::Underscore => write!(f, "_"),
            TokenKind::At => write!(f, "@"),
            TokenKind::Eof => write!(f, "EOF"),
        }
    }
}
