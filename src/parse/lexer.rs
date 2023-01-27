use logos::Logos;

#[derive(Logos, Debug, PartialEq, Eq, Clone, Copy)]
pub enum Token {
    // Type keywords
    #[token("integer")]
    Integer,
    #[token("float")]
    Float,
    #[token("bool")]
    Bool,
    #[token("string")]
    String,

    // Declaration keywords
    #[token("variable")]
    Variable,
    #[token("function")]
    Function,
    #[token("type")]
    Type,

    // Comments
    #[regex("//[^\n]*")]
    Comment,

    // Misc
    #[token("\n")]
    Newline,
    #[token(":")]
    Colon,
    #[token(".")]
    Period,
    #[token(",")]
    Comma,
    #[token("[")]
    OpenSq,
    #[token("]")]
    ClosedSq,
    #[token("(")]
    OpenP,
    #[token(")")]
    ClosedP,
    #[token("{")]
    OpenBr,
    #[token("}")]
    ClosedBr,
    #[token("->")]
    Arrow,

    // Control flow keywords
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("end")]
    End,
    #[token("return")]
    Return,
    #[token("do")]
    Do,
    #[token("in")]
    In,
    #[token("then")]
    Then,
    #[token("output")]
    Output,
    #[token("min")]
    Min,
    #[token("max")]
    Max,
    #[token("repeat")]
    Repeat,
    #[token("has_string")]
    HasString,
    #[token("next_string")]
    NextString,
    #[token("has_int")]
    HasInt,
    #[token("next_int")]
    NextInt,

    // Operators
    #[token("<-")]
    Assign,
    #[token(">")]
    Gt,
    #[token(">=")]
    Ge,
    #[token("<")]
    Lt,
    #[token("<=")]
    Le,
    #[token("==")]
    Eq,
    #[token("!=")]
    Ne,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("+")]
    Sum,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("mod")]
    Mod,
    #[token("...")]
    Range,
    #[token("not")]
    Not,

    // Literals
    #[regex("[0-9]+")]
    IntegerLit,
    #[regex("[0-9]+\\.[0-9]+")]
    FloatLit,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[regex("\"([^\"]|\\.)*\"")]
    StringLit,

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,

    // End of file.
    Eos,

    #[error]
    #[regex(r"[ \t\f]+", logos::skip)]
    Error,
}
