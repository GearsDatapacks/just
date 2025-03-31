pub type Token {
  // Comments and whitespace
  SingleLineComment(String)
  MultiLineComment(String)
  HashBangComment(String)
  Whitespace(String)
  LineTerminator(String)
  EndOfFile

  // Literals
  Identifier(String)
  PrivateIdentifier(String)
  Number(String)
  BigInt(String)
  String(quote: String, contents: String)
  RegularExpression(contents: String, flags: String)
  TemplateHead(String)
  TemplateMiddle(String)
  TemplateTail(String)

  // Keywords
  Break
  Case
  Catch
  Class
  Const
  Continue
  Debugger
  Default
  Delete
  Do
  Else
  Export
  Extends
  False
  Finally
  For
  Function
  If
  Import
  In
  Instanceof
  New
  Null
  Return
  Super
  Switch
  This
  Throw
  True
  Try
  Typeof
  Var
  Void
  While
  With

  // Keywords in strict mode
  Let
  Static
  Yield

  // Future reserved words
  Enum

  // Future reserved words in strict mode
  Implements
  Interface
  Package
  Private
  Protected

  // Contextual keywords
  ContextualKeyword(ContextualKeyword)

  // Grouping
  LeftBrace
  RightBrace
  LeftParen
  RightParen
  LeftSquare
  RightSquare

  // Separators
  Dot
  TripleDot
  Semicolon
  Comma
  Colon
  Arrow

  // Comparison
  Less
  Greater
  LessEqual
  GreaterEqual
  DoubleEqual
  BangEqual
  TripleEqual
  BangDoubleEqual

  // Arithmetic
  Plus
  Minus
  Star
  Slash
  Percent
  DoubleStar
  DoublePlus
  DoubleMinus
  DoubleLess
  DoubleGreater
  TripleGreater
  Ampersand
  Pipe
  Caret
  Tilde

  // Logic
  Bang
  DoubleAmpersand
  DoublePipe
  Question
  DoubleQuestion
  QuestionDot

  // Assignment
  Equal
  PlusEqual
  MinusEqual
  StarEqual
  SlashEqual
  PercentEqual
  DoubleStarEqual
  DoubleLessEqual
  DoubleGreaterEqual
  TripleGreaterEqual
  AmpersandEqual
  PipeEqual
  CaratEqual
  DoubleAmpersandEqual
  DoublePipeEqual
  DoubleQuestionEqual

  // Invalid tokens
  Unknown(String)
  UnterminatedString(quote: String, contents: String)
  UnterminatedTemplate(String)
  UnterminatedRegularExpression(String)
  UnterminatedComment(String)
}

pub type ContextualKeyword {
  As
  Async
  Await
  From
  Get
  ContextualLet
  Of
  Set
  ContextualStatic
  ContextualYield
}

pub fn contextual_keyword_name(keyword: ContextualKeyword) -> String {
  case keyword {
    As -> "as"
    Async -> "async"
    Await -> "await"
    From -> "from"
    Get -> "get"
    ContextualLet -> "let"
    Of -> "of"
    ContextualStatic -> "static"
    Set -> "set"
    ContextualYield -> "yield"
  }
}

pub fn to_source(token: Token) -> String {
  case token {
    // Comments and whitespace
    SingleLineComment(value) -> "//" <> value
    MultiLineComment(value) -> "/*" <> value <> "*/"
    HashBangComment(value) -> "#!" <> value
    Whitespace(value) -> value
    LineTerminator(value) -> value
    EndOfFile -> ""

    // Literals
    Identifier(value) -> value
    PrivateIdentifier(value) -> "#" <> value
    Number(value) -> value
    BigInt(value) -> value <> "n"
    String(quote:, contents:) -> quote <> contents <> quote
    RegularExpression(contents:, flags:) -> "/" <> contents <> "/" <> flags
    TemplateHead(value) -> "`" <> value <> "${"
    TemplateMiddle(value) -> "}" <> value <> "${"
    TemplateTail(value) -> "}" <> value <> "`"

    // Keywords
    Break -> "break"
    Case -> "case"
    Catch -> "catch"
    Class -> "class"
    Const -> "const"
    Continue -> "continue"
    Debugger -> "debugger"
    Default -> "default"
    Delete -> "delete"
    Do -> "do"
    Else -> "else"
    Export -> "export"
    Extends -> "extends"
    False -> "false"
    Finally -> "finally"
    For -> "for"
    Function -> "function"
    If -> "if"
    Import -> "import"
    In -> "in"
    Instanceof -> "instanceof"
    New -> "new"
    Null -> "null"
    Return -> "return"
    Super -> "super"
    Switch -> "switch"
    This -> "this"
    Throw -> "throw"
    True -> "true"
    Try -> "try"
    Typeof -> "typeof"
    Var -> "var"
    Void -> "void"
    While -> "while"
    With -> "with"

    // Keywords in strict mode
    Let -> "let"
    Static -> "static"
    Yield -> "yield"

    // Future reserved words
    Enum -> "enum"

    // Future reserved words in strict mode
    Implements -> "implements"
    Interface -> "interface"
    Package -> "package"
    Private -> "private"
    Protected -> "protected"

    // Contextual keywords
    ContextualKeyword(keyword) -> contextual_keyword_name(keyword)

    // Grouping
    LeftBrace -> "{"
    RightBrace -> "}"
    LeftParen -> "("
    RightParen -> ")"
    LeftSquare -> "["
    RightSquare -> "]"

    // Separators
    Dot -> "."
    TripleDot -> "..."
    Semicolon -> ";"
    Comma -> ","
    Colon -> ":"
    Arrow -> "=>"

    // Comparison
    Less -> "<"
    Greater -> ">"
    LessEqual -> "<="
    GreaterEqual -> ">="
    DoubleEqual -> "=="
    BangEqual -> "!="
    TripleEqual -> "==="
    BangDoubleEqual -> "!=="

    // Arithmetic
    Plus -> "+"
    Minus -> "-"
    Star -> "*"
    Slash -> "/"
    Percent -> "%"
    DoubleStar -> "**"
    DoublePlus -> "++"
    DoubleMinus -> "--"
    DoubleLess -> "<<"
    DoubleGreater -> ">>"
    TripleGreater -> ">>>"
    Ampersand -> "&"
    Pipe -> "|"
    Caret -> "^"
    Tilde -> "~"

    // Logic
    Bang -> "!"
    DoubleAmpersand -> "&&"
    DoublePipe -> "||"
    Question -> "?"
    DoubleQuestion -> "??"
    QuestionDot -> "?."

    // Assignment
    Equal -> "="
    PlusEqual -> "+="
    MinusEqual -> "-="
    StarEqual -> "*="
    SlashEqual -> "/="
    PercentEqual -> "%="
    DoubleStarEqual -> "**="
    DoubleLessEqual -> "<<="
    DoubleGreaterEqual -> ">>="
    TripleGreaterEqual -> ">>>="
    AmpersandEqual -> "&="
    PipeEqual -> "|="
    CaratEqual -> "^="
    DoubleAmpersandEqual -> "&&="
    DoublePipeEqual -> "||="
    DoubleQuestionEqual -> "??="

    Unknown(value) -> value
    UnterminatedComment(value) -> "/*" <> value
    UnterminatedRegularExpression(value) -> "/" <> value
    UnterminatedString(quote:, contents:) -> quote <> contents
    UnterminatedTemplate(value) -> value
  }
}
