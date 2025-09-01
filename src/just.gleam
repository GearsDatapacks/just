import gleam
import gleam/list
import gleam/string
import gleam_community/ansi
import houdini
import splitter.{type Splitter}

pub opaque type Lexer {
  Lexer(
    source: String,
    ignore_comments: Bool,
    ignore_whitespace: Bool,
    strict_mode: Bool,
    mode: LexerMode,
    errors: List(Error),
    splitters: Splitters,
  )
}

type Splitters {
  Splitters(
    string: Splitter,
    multiline_comment: Splitter,
    template: Splitter,
    until_end_of_line: Splitter,
    regex_in_group: Splitter,
    regex_regular: Splitter,
  )
}

type LexerMode {
  TreatSlashAsRegex
  TreatSlashAsDivision
}

pub type Error {
  UnknownCharacter(character: String)
  UnterminatedStringLiteral
  UnterminatedMultilineComment
  UnterminatedRegExpLiteral
  UnterminatedTemplateLiteral
  LetterAfterNumber
  NumericSeparatorNotAllowed
  ExpectedExponent
  InvalidPrivateIdentifier(identifier: String)
  ZeroPrefixedNumberInStrictMode
}

pub fn stringify_error(error: Error) -> String {
  case error {
    InvalidPrivateIdentifier(identifier) ->
      "Invalid private identifier: `" <> identifier <> "`"
    LetterAfterNumber ->
      "Identifier characters cannot appear directly after numbers"
    ExpectedExponent -> "Expected an exponent"
    NumericSeparatorNotAllowed -> "Numeric separator is not allowed here"
    UnknownCharacter(character) -> "Unexpected character: `" <> character <> "`"
    UnterminatedMultilineComment -> "Unterminated comment"
    UnterminatedRegExpLiteral -> "Unterminated regular expression literal"
    UnterminatedStringLiteral -> "Unterminated string literal"
    UnterminatedTemplateLiteral -> "Unterminated template literal"
    ZeroPrefixedNumberInStrictMode ->
      "Zero-prefixed numbers are not allowed in strict mode"
  }
}

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

pub fn token_to_source(token: Token) -> String {
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

pub fn new(source: String) -> Lexer {
  Lexer(
    source:,
    ignore_comments: gleam.False,
    ignore_whitespace: gleam.False,
    strict_mode: gleam.False,
    mode: TreatSlashAsRegex,
    errors: [],
    splitters: make_splitters(),
  )
}

fn make_splitters() -> Splitters {
  Splitters(
    string: splitter.new(["\"", "'", "\\", "\n", "\r"]),
    multiline_comment: splitter.new(["*/"]),
    template: splitter.new(["`", "${", "\\"]),
    until_end_of_line: splitter.new(["\n", "\r", "\u{2028}", "\u{2029}"]),
    regex_regular: splitter.new([
      "/", "[", "]", "\n", "\r", "\u{2028}", "\u{2029}", "\\",
    ]),
    regex_in_group: splitter.new(["]", "\n", "\r", "\u{2028}", "\u{2029}", "\\"]),
  )
}

pub fn ignore_comments(lexer: Lexer) -> Lexer {
  Lexer(..lexer, ignore_comments: gleam.True)
}

pub fn ignore_whitespace(lexer: Lexer) -> Lexer {
  Lexer(..lexer, ignore_whitespace: gleam.True)
}

pub fn strict_mode(lexer: Lexer) -> Lexer {
  Lexer(..lexer, strict_mode: gleam.True)
}

pub fn tokenise(lexer: Lexer) -> #(List(Token), List(Error)) {
  let #(lexer, tokens) = maybe_lex_hashbang_comment(lexer)
  do_tokenise(lexer, tokens)
}

pub fn to_source(tokens: List(Token)) -> String {
  list.fold(tokens, "", fn(code, token) { code <> token_to_source(token) })
}

fn maybe_lex_hashbang_comment(lexer: Lexer) -> #(Lexer, List(Token)) {
  case lexer.source {
    "#!" <> source -> {
      let #(lexer, contents) =
        lexer
        |> advance(source)
        |> lex_until_end_of_line

      #(lexer, case lexer.ignore_comments {
        gleam.True -> []
        gleam.False -> [HashBangComment(contents)]
      })
    }
    _ -> #(lexer, [])
  }
}

fn do_tokenise(lexer: Lexer, tokens: List(Token)) -> #(List(Token), List(Error)) {
  case next(lexer) {
    #(lexer, EndOfFile) -> #(
      list.reverse([EndOfFile, ..tokens]),
      list.reverse(lexer.errors),
    )
    #(lexer, TemplateHead(_) as token) -> {
      let #(lexer, tokens) =
        lex_template_parts(
          update_mode_with_token(lexer, token),
          [token, ..tokens],
          LexTokens(bracket_level: 0),
        )
      do_tokenise(lexer, tokens)
    }
    #(lexer, token) ->
      do_tokenise(update_mode_with_token(lexer, token), [token, ..tokens])
  }
}

fn maybe_token(lexer: Lexer, token: Token, condition: Bool) -> #(Lexer, Token) {
  case condition {
    gleam.True -> #(lexer, token)
    gleam.False -> next(lexer)
  }
}

fn next(lexer: Lexer) -> #(Lexer, Token) {
  case lexer.source {
    "" -> #(lexer, EndOfFile)

    "\t" as space <> source
    | "\u{000B}" as space <> source
    | "\f" as space <> source
    | " " as space <> source
    | "\u{1680}" as space <> source
    | "\u{2000}" as space <> source
    | "\u{2001}" as space <> source
    | "\u{2002}" as space <> source
    | "\u{2003}" as space <> source
    | "\u{2004}" as space <> source
    | "\u{2005}" as space <> source
    | "\u{2006}" as space <> source
    | "\u{2007}" as space <> source
    | "\u{2008}" as space <> source
    | "\u{2009}" as space <> source
    | "\u{200A}" as space <> source
    | "\u{202F}" as space <> source
    | "\u{205F}" as space <> source
    | "\u{3000}" as space <> source
    | "\u{FEFF}" as space <> source -> {
      whitespace(advance(lexer, source), space)
    }

    "\u{000A}" as space <> source
    | "\u{000D}" as space <> source
    | "\u{2028}" as space <> source
    | "\u{2029}" as space <> source ->
      maybe_token(
        advance(lexer, source),
        LineTerminator(space),
        !lexer.ignore_whitespace,
      )

    "//" <> source -> {
      let #(lexer, contents) = lex_until_end_of_line(advance(lexer, source))
      maybe_token(lexer, SingleLineComment(contents), !lexer.ignore_comments)
    }
    "/*" <> source -> lex_multiline_comment(advance(lexer, source))

    "0b" as prefix <> source | "0B" as prefix <> source ->
      lex_radix_number(advance(lexer, source), 2, prefix, gleam.False)

    "0o" as prefix <> source | "0O" as prefix <> source ->
      lex_radix_number(advance(lexer, source), 8, prefix, gleam.False)

    "0x" as prefix <> source | "0X" as prefix <> source ->
      lex_radix_number(advance(lexer, source), 16, prefix, gleam.False)

    "00" as digit <> source
    | "01" as digit <> source
    | "02" as digit <> source
    | "03" as digit <> source
    | "04" as digit <> source
    | "05" as digit <> source
    | "06" as digit <> source
    | "07" as digit <> source ->
      lex_leading_zero_number(advance(lexer, source), digit, gleam.False)

    "08" as digit <> source | "09" as digit <> source ->
      lex_leading_zero_number(advance(lexer, source), digit, gleam.True)

    "1" as digit <> source
    | "2" as digit <> source
    | "3" as digit <> source
    | "4" as digit <> source
    | "5" as digit <> source
    | "6" as digit <> source
    | "7" as digit <> source
    | "8" as digit <> source
    | "9" as digit <> source
    | "0" as digit <> source ->
      lex_number(advance(lexer, source), digit, Initial, AfterNumber)

    ".1" as digit <> source
    | ".2" as digit <> source
    | ".3" as digit <> source
    | ".4" as digit <> source
    | ".5" as digit <> source
    | ".6" as digit <> source
    | ".7" as digit <> source
    | ".8" as digit <> source
    | ".9" as digit <> source
    | ".0" as digit <> source ->
      lex_number(advance(lexer, source), digit, Decimal, AfterNumber)

    "/" <> source if lexer.mode == TreatSlashAsRegex ->
      lex_regex(advance(lexer, source), "", gleam.False)

    "{" <> source -> #(advance(lexer, source), LeftBrace)
    "}" <> source -> #(advance(lexer, source), RightBrace)
    "(" <> source -> #(advance(lexer, source), LeftParen)
    ")" <> source -> #(advance(lexer, source), RightParen)
    "[" <> source -> #(advance(lexer, source), LeftSquare)
    "]" <> source -> #(advance(lexer, source), RightSquare)

    "..." <> source -> #(advance(lexer, source), TripleDot)
    "." <> source -> #(advance(lexer, source), Dot)
    ";" <> source -> #(advance(lexer, source), Semicolon)
    "," <> source -> #(advance(lexer, source), Comma)
    ":" <> source -> #(advance(lexer, source), Colon)
    "=>" <> source -> #(advance(lexer, source), Arrow)

    "<=" <> source -> #(advance(lexer, source), LessEqual)
    ">=" <> source -> #(advance(lexer, source), GreaterEqual)
    "===" <> source -> #(advance(lexer, source), TripleEqual)
    "!==" <> source -> #(advance(lexer, source), BangDoubleEqual)
    "==" <> source -> #(advance(lexer, source), DoubleEqual)
    "!=" <> source -> #(advance(lexer, source), BangEqual)

    "=" <> source -> #(advance(lexer, source), Equal)
    "+=" <> source -> #(advance(lexer, source), PlusEqual)
    "-=" <> source -> #(advance(lexer, source), MinusEqual)
    "*=" <> source -> #(advance(lexer, source), StarEqual)
    "/=" <> source -> #(advance(lexer, source), SlashEqual)
    "%=" <> source -> #(advance(lexer, source), PercentEqual)
    "**=" <> source -> #(advance(lexer, source), DoubleStarEqual)
    "<<=" <> source -> #(advance(lexer, source), DoubleLessEqual)
    ">>=" <> source -> #(advance(lexer, source), DoubleGreaterEqual)
    ">>>=" <> source -> #(advance(lexer, source), TripleGreaterEqual)
    "&=" <> source -> #(advance(lexer, source), AmpersandEqual)
    "|=" <> source -> #(advance(lexer, source), PipeEqual)
    "^=" <> source -> #(advance(lexer, source), CaratEqual)
    "&&=" <> source -> #(advance(lexer, source), DoubleAmpersandEqual)
    "||=" <> source -> #(advance(lexer, source), DoublePipeEqual)
    "??=" <> source -> #(advance(lexer, source), DoubleQuestionEqual)

    "<<" <> source -> #(advance(lexer, source), DoubleLess)
    ">>>" <> source -> #(advance(lexer, source), TripleGreater)
    ">>" <> source -> #(advance(lexer, source), DoubleGreater)

    "!" <> source -> #(advance(lexer, source), Bang)
    "&&" <> source -> #(advance(lexer, source), DoubleAmpersand)
    "||" <> source -> #(advance(lexer, source), DoublePipe)
    "??" <> source -> #(advance(lexer, source), DoubleQuestion)
    "?." <> source -> #(advance(lexer, source), QuestionDot)
    "?" <> source -> #(advance(lexer, source), Question)

    "<" <> source -> #(advance(lexer, source), Less)
    ">" <> source -> #(advance(lexer, source), Greater)

    "**" <> source -> #(advance(lexer, source), DoubleStar)
    "++" <> source -> #(advance(lexer, source), DoublePlus)
    "--" <> source -> #(advance(lexer, source), DoubleMinus)
    "+" <> source -> #(advance(lexer, source), Plus)
    "-" <> source -> #(advance(lexer, source), Minus)
    "*" <> source -> #(advance(lexer, source), Star)
    "/" <> source -> #(advance(lexer, source), Slash)
    "%" <> source -> #(advance(lexer, source), Percent)
    "&" <> source -> #(advance(lexer, source), Ampersand)
    "|" <> source -> #(advance(lexer, source), Pipe)
    "^" <> source -> #(advance(lexer, source), Caret)
    "~" <> source -> #(advance(lexer, source), Tilde)

    "#" <> source -> {
      let #(lexer, name) = lex_identifier(advance(lexer, source), "")

      let lexer = case name {
        ""
        | "0" <> _
        | "1" <> _
        | "2" <> _
        | "3" <> _
        | "4" <> _
        | "5" <> _
        | "6" <> _
        | "7" <> _
        | "8" <> _
        | "9" <> _ -> error(lexer, InvalidPrivateIdentifier("#" <> name))
        _ -> lexer
      }

      #(lexer, PrivateIdentifier(name))
    }

    "_" as character <> source
    | "$" as character <> source
    | "a" as character <> source
    | "b" as character <> source
    | "c" as character <> source
    | "d" as character <> source
    | "e" as character <> source
    | "f" as character <> source
    | "g" as character <> source
    | "h" as character <> source
    | "i" as character <> source
    | "j" as character <> source
    | "k" as character <> source
    | "l" as character <> source
    | "m" as character <> source
    | "n" as character <> source
    | "o" as character <> source
    | "p" as character <> source
    | "q" as character <> source
    | "r" as character <> source
    | "s" as character <> source
    | "t" as character <> source
    | "u" as character <> source
    | "v" as character <> source
    | "w" as character <> source
    | "x" as character <> source
    | "y" as character <> source
    | "z" as character <> source
    | "A" as character <> source
    | "B" as character <> source
    | "C" as character <> source
    | "D" as character <> source
    | "E" as character <> source
    | "F" as character <> source
    | "G" as character <> source
    | "H" as character <> source
    | "I" as character <> source
    | "J" as character <> source
    | "K" as character <> source
    | "L" as character <> source
    | "M" as character <> source
    | "N" as character <> source
    | "O" as character <> source
    | "P" as character <> source
    | "Q" as character <> source
    | "R" as character <> source
    | "S" as character <> source
    | "T" as character <> source
    | "U" as character <> source
    | "V" as character <> source
    | "W" as character <> source
    | "X" as character <> source
    | "Y" as character <> source
    | "Z" as character <> source -> {
      let #(lexer, name) = lex_identifier(advance(lexer, source), character)

      let token = identifier_token(name, lexer.strict_mode)
      #(lexer, token)
    }

    "'" as quote <> source | "\"" as quote <> source ->
      lex_string(advance(lexer, source), quote, "")

    "`" <> source -> lex_template_head(advance(lexer, source), "")

    _ ->
      case string.pop_grapheme(lexer.source) {
        Error(_) -> #(lexer, EndOfFile)
        Ok(#(character, source)) -> #(
          lexer |> advance(source) |> error(UnknownCharacter(character)),
          Unknown(character),
        )
      }
  }
}

fn lex_multiline_comment(lexer: Lexer) -> #(Lexer, Token) {
  case splitter.split(lexer.splitters.multiline_comment, lexer.source) {
    #(before, "", "") ->
      maybe_token(
        error(advance(lexer, ""), UnterminatedMultilineComment),
        UnterminatedComment(before),
        !lexer.ignore_comments,
      )
    #(before, _, after) ->
      maybe_token(
        advance(lexer, after),
        MultiLineComment(before),
        !lexer.ignore_comments,
      )
  }
}

fn identifier_token(name: String, strict_mode: Bool) -> Token {
  case name {
    // Keywords
    "break" -> Break
    "case" -> Case
    "catch" -> Catch
    "class" -> Class
    "const" -> Const
    "continue" -> Continue
    "debugger" -> Debugger
    "default" -> Default
    "delete" -> Delete
    "do" -> Do
    "else" -> Else
    "export" -> Export
    "extends" -> Extends
    "false" -> False
    "finally" -> Finally
    "for" -> For
    "function" -> Function
    "if" -> If
    "import" -> Import
    "in" -> In
    "instanceof" -> Instanceof
    "new" -> New
    "null" -> Null
    "return" -> Return
    "super" -> Super
    "switch" -> Switch
    "this" -> This
    "throw" -> Throw
    "true" -> True
    "try" -> Try
    "typeof" -> Typeof
    "var" -> Var
    "void" -> Void
    "while" -> While
    "with" -> With

    // Keywords in strict mode
    "let" if strict_mode -> Let
    "static" if strict_mode -> Static
    "yield" if strict_mode -> Yield

    // Future reserved words
    "enum" -> Enum

    // Future reserved words in strict mode
    "implements" if strict_mode -> Implements
    "interface" if strict_mode -> Interface
    "package" if strict_mode -> Package
    "private" if strict_mode -> Private
    "protected" if strict_mode -> Protected

    // Contextual keywords
    "as" -> ContextualKeyword(As)
    "async" -> ContextualKeyword(Async)
    "await" -> ContextualKeyword(Await)
    "from" -> ContextualKeyword(From)
    "get" -> ContextualKeyword(Get)
    "let" -> ContextualKeyword(ContextualLet)
    "of" -> ContextualKeyword(Of)
    "set" -> ContextualKeyword(Set)
    "static" -> ContextualKeyword(ContextualStatic)
    "yield" -> ContextualKeyword(ContextualYield)

    _ -> Identifier(name)
  }
}

type LexNumberMode {
  Initial
  Decimal
  Exponent
}

type DelimitedPosition {
  AfterDecimal
  AfterNumber
  AfterSeparator
  AfterExponent
}

fn lex_number(
  lexer: Lexer,
  lexed: String,
  mode: LexNumberMode,
  position: DelimitedPosition,
) -> #(Lexer, Token) {
  case lexer.source, mode, position {
    "0" as digit <> source, _, _
    | "1" as digit <> source, _, _
    | "2" as digit <> source, _, _
    | "3" as digit <> source, _, _
    | "4" as digit <> source, _, _
    | "5" as digit <> source, _, _
    | "6" as digit <> source, _, _
    | "7" as digit <> source, _, _
    | "8" as digit <> source, _, _
    | "9" as digit <> source, _, _
    -> lex_number(advance(lexer, source), lexed <> digit, mode, AfterNumber)

    "." <> source, Initial, AfterNumber ->
      lex_number(advance(lexer, source), lexed <> ".", Decimal, AfterDecimal)

    "e-" as prefix <> source, Initial, AfterNumber
    | "e-" as prefix <> source, Decimal, AfterNumber
    | "e" as prefix <> source, Initial, AfterNumber
    | "e" as prefix <> source, Decimal, AfterNumber
    | "E-" as prefix <> source, Initial, AfterNumber
    | "E-" as prefix <> source, Decimal, AfterNumber
    | "E" as prefix <> source, Initial, AfterNumber
    | "E" as prefix <> source, Decimal, AfterNumber
    ->
      lex_number(
        advance(lexer, source),
        lexed <> prefix,
        Exponent,
        AfterExponent,
      )

    "_" <> source, _, AfterNumber ->
      lex_number(advance(lexer, source), lexed <> "_", mode, AfterSeparator)

    "_" <> _, _, _ -> #(error(lexer, NumericSeparatorNotAllowed), Number(lexed))

    "n" <> source, Initial, AfterNumber -> #(
      ensure_no_letters_after_numbers(advance(lexer, source)),
      BigInt(lexed),
    )

    _, _, AfterExponent -> #(error(lexer, ExpectedExponent), Number(lexed))

    _, _, AfterSeparator -> #(
      error(lexer, NumericSeparatorNotAllowed),
      Number(lexed),
    )

    _, _, _ -> #(ensure_no_letters_after_numbers(lexer), Number(lexed))
  }
}

fn lex_leading_zero_number(
  lexer: Lexer,
  lexed: String,
  allow_decimal_digits: Bool,
) -> #(Lexer, Token) {
  case lexer.source {
    "0" as digit <> source
    | "1" as digit <> source
    | "2" as digit <> source
    | "3" as digit <> source
    | "4" as digit <> source
    | "5" as digit <> source
    | "6" as digit <> source
    | "7" as digit <> source ->
      lex_leading_zero_number(
        advance(lexer, source),
        lexed <> digit,
        allow_decimal_digits,
      )

    "8" as digit <> source | "9" as digit <> source if allow_decimal_digits ->
      lex_leading_zero_number(
        advance(lexer, source),
        lexed <> digit,
        allow_decimal_digits,
      )

    _ -> {
      let lexer = ensure_no_letters_after_numbers(lexer)
      let lexer = case lexer.strict_mode {
        gleam.False -> lexer
        gleam.True -> error(lexer, ZeroPrefixedNumberInStrictMode)
      }
      #(lexer, Number(lexed))
    }
  }
}

fn lex_radix_number(
  lexer: Lexer,
  radix: Int,
  lexed: String,
  valid_delimiter: Bool,
) -> #(Lexer, Token) {
  case lexer.source {
    "0" as digit <> source | "1" as digit <> source ->
      lex_radix_number(
        advance(lexer, source),
        radix,
        lexed <> digit,
        gleam.True,
      )

    "2" as digit <> source
      | "3" as digit <> source
      | "4" as digit <> source
      | "5" as digit <> source
      | "6" as digit <> source
      | "7" as digit <> source
      if radix >= 8
    ->
      lex_radix_number(
        advance(lexer, source),
        radix,
        lexed <> digit,
        gleam.True,
      )

    "8" as digit <> source
      | "9" as digit <> source
      | "A" as digit <> source
      | "B" as digit <> source
      | "C" as digit <> source
      | "D" as digit <> source
      | "E" as digit <> source
      | "F" as digit <> source
      | "a" as digit <> source
      | "b" as digit <> source
      | "c" as digit <> source
      | "d" as digit <> source
      | "e" as digit <> source
      | "f" as digit <> source
      if radix == 16
    ->
      lex_radix_number(
        advance(lexer, source),
        radix,
        lexed <> digit,
        gleam.True,
      )

    "n" <> source if valid_delimiter -> #(
      ensure_no_letters_after_numbers(advance(lexer, source)),
      BigInt(lexed),
    )

    "_" <> source if valid_delimiter ->
      lex_radix_number(advance(lexer, source), radix, lexed <> "_", gleam.False)

    _ if !valid_delimiter -> #(
      error(lexer, NumericSeparatorNotAllowed),
      Number(lexed),
    )
    _ -> #(ensure_no_letters_after_numbers(lexer), Number(lexed))
  }
}

fn ensure_no_letters_after_numbers(lexer: Lexer) -> Lexer {
  case lexer.source {
    "_" <> _source
    | "$" <> _source
    | "a" <> _source
    | "b" <> _source
    | "c" <> _source
    | "d" <> _source
    | "e" <> _source
    | "f" <> _source
    | "g" <> _source
    | "h" <> _source
    | "i" <> _source
    | "j" <> _source
    | "k" <> _source
    | "l" <> _source
    | "m" <> _source
    | "n" <> _source
    | "o" <> _source
    | "p" <> _source
    | "q" <> _source
    | "r" <> _source
    | "s" <> _source
    | "t" <> _source
    | "u" <> _source
    | "v" <> _source
    | "w" <> _source
    | "x" <> _source
    | "y" <> _source
    | "z" <> _source
    | "A" <> _source
    | "B" <> _source
    | "C" <> _source
    | "D" <> _source
    | "E" <> _source
    | "F" <> _source
    | "G" <> _source
    | "H" <> _source
    | "I" <> _source
    | "J" <> _source
    | "K" <> _source
    | "L" <> _source
    | "M" <> _source
    | "N" <> _source
    | "O" <> _source
    | "P" <> _source
    | "Q" <> _source
    | "R" <> _source
    | "S" <> _source
    | "T" <> _source
    | "U" <> _source
    | "V" <> _source
    | "W" <> _source
    | "X" <> _source
    | "Y" <> _source
    | "Z" <> _source -> error(lexer, LetterAfterNumber)
    _ -> lexer
  }
}

fn lex_string(lexer: Lexer, quote: String, contents: String) -> #(Lexer, Token) {
  let #(before, after) =
    splitter.split_before(lexer.splitters.string, lexer.source)
  case after {
    "\r" <> _rest | "\n" <> _rest -> #(
      error(advance(lexer, after), UnterminatedStringLiteral),
      UnterminatedString(quote:, contents: contents <> before),
    )

    "\\" <> after ->
      case string.pop_grapheme(after) {
        Error(_) -> #(
          error(advance(lexer, after), UnterminatedStringLiteral),
          UnterminatedString(quote:, contents:),
        )
        Ok(#(character, source)) ->
          lex_string(
            advance(lexer, source),
            quote,
            contents <> before <> "\\" <> character,
          )
      }

    "\"" as split <> after | "'" as split <> after if split == quote -> #(
      advance(lexer, after),
      String(quote:, contents: contents <> before),
    )

    // Here, we've split on a quote which doesn't match the current string.
    // In this case, we must continue lexing until we find a quote of the
    // correct kind.
    "\"" as split <> after | "'" as split <> after ->
      lex_string(advance(lexer, after), quote, contents <> before <> split)

    _ -> #(
      error(advance(lexer, after), UnterminatedStringLiteral),
      UnterminatedString(quote:, contents: contents <> before),
    )
  }
}

fn lex_template_head(lexer: Lexer, lexed: String) -> #(Lexer, Token) {
  let #(before, split, after) =
    splitter.split(lexer.splitters.template, lexer.source)

  case split {
    "${" -> #(advance(lexer, after), TemplateHead(lexed <> before))
    "\\" ->
      case string.pop_grapheme(after) {
        Error(_) -> #(
          error(advance(lexer, after), UnterminatedStringLiteral),
          UnterminatedString("`", lexed <> before),
        )
        Ok(#(character, source)) ->
          lex_template_head(
            advance(lexer, source),
            lexed <> before <> "\\" <> character,
          )
      }
    "" -> #(
      error(advance(lexer, after), UnterminatedStringLiteral),
      UnterminatedString("`", lexed <> before),
    )
    _ -> #(advance(lexer, after), String("`", lexed <> before))
  }
}

type LexTemplateMode {
  LexTokens(bracket_level: Int)
  LexTemplate(lexed: String)
}

fn lex_template_parts(
  lexer: Lexer,
  tokens: List(Token),
  mode: LexTemplateMode,
) -> #(Lexer, List(Token)) {
  case mode {
    LexTemplate(lexed) -> {
      let #(before, split, after) =
        splitter.split(lexer.splitters.template, lexer.source)

      case split {
        "${" -> {
          let lexer = advance(lexer, after)
          let token = TemplateMiddle(lexed <> before)
          lex_template_parts(
            update_mode_with_token(lexer, token),
            [token, ..tokens],
            LexTokens(0),
          )
        }
        "\\" ->
          case string.pop_grapheme(after) {
            Error(_) -> #(error(lexer, UnterminatedTemplateLiteral), [
              UnterminatedTemplate(lexed <> before),
              ..tokens
            ])
            Ok(#(character, source)) ->
              lex_template_parts(
                advance(lexer, source),
                tokens,
                LexTemplate(lexed <> before <> "\\" <> character),
              )
          }
        "" -> #(error(advance(lexer, after), UnterminatedTemplateLiteral), [
          UnterminatedTemplate(lexed <> before),
          ..tokens
        ])
        _ -> #(advance(lexer, after), [TemplateTail(lexed <> before), ..tokens])
      }
    }

    LexTokens(bracket_level) ->
      case next(lexer) {
        #(lexer, EndOfFile) -> #(lexer, tokens)
        #(lexer, RightBrace) if bracket_level == 0 ->
          lex_template_parts(lexer, tokens, LexTemplate(""))
        #(lexer, RightBrace as token) ->
          lex_template_parts(
            update_mode_with_token(lexer, token),
            [token, ..tokens],
            LexTokens(bracket_level - 1),
          )
        #(lexer, LeftBrace as token) ->
          lex_template_parts(
            update_mode_with_token(lexer, token),
            [token, ..tokens],
            LexTokens(bracket_level + 1),
          )
        #(lexer, token) ->
          lex_template_parts(
            update_mode_with_token(lexer, token),
            [token, ..tokens],
            mode,
          )
      }
  }
}

fn lex_regex(lexer: Lexer, lexed: String, in_group: Bool) -> #(Lexer, Token) {
  let splitter = case in_group {
    gleam.False -> lexer.splitters.regex_regular
    gleam.True -> lexer.splitters.regex_in_group
  }
  let #(before, after) = splitter.split_before(splitter, lexer.source)
  case after {
    "/" <> after -> {
      let lexer = advance(lexer, after)
      let #(lexer, flags) = lex_identifier(lexer, "")
      #(lexer, RegularExpression(contents: lexed <> before, flags:))
    }
    "[" <> after ->
      lex_regex(advance(lexer, after), lexed <> before <> "[", gleam.True)
    "]" <> after ->
      lex_regex(advance(lexer, after), lexed <> before <> "]", gleam.False)

    "\\" <> after ->
      case string.pop_grapheme(after) {
        Error(_) -> #(
          error(advance(lexer, after), UnterminatedRegExpLiteral),
          UnterminatedRegularExpression(lexed <> before),
        )
        Ok(#(character, source)) ->
          lex_regex(
            advance(lexer, source),
            lexed <> before <> "\\" <> character,
            in_group,
          )
      }

    _ -> #(
      error(advance(lexer, after), UnterminatedRegExpLiteral),
      UnterminatedRegularExpression(lexed <> before),
    )
  }
}

fn lex_identifier(lexer: Lexer, lexed: String) -> #(Lexer, String) {
  case lexer.source {
    "_" as character <> source
    | "$" as character <> source
    | "a" as character <> source
    | "b" as character <> source
    | "c" as character <> source
    | "d" as character <> source
    | "e" as character <> source
    | "f" as character <> source
    | "g" as character <> source
    | "h" as character <> source
    | "i" as character <> source
    | "j" as character <> source
    | "k" as character <> source
    | "l" as character <> source
    | "m" as character <> source
    | "n" as character <> source
    | "o" as character <> source
    | "p" as character <> source
    | "q" as character <> source
    | "r" as character <> source
    | "s" as character <> source
    | "t" as character <> source
    | "u" as character <> source
    | "v" as character <> source
    | "w" as character <> source
    | "x" as character <> source
    | "y" as character <> source
    | "z" as character <> source
    | "A" as character <> source
    | "B" as character <> source
    | "C" as character <> source
    | "D" as character <> source
    | "E" as character <> source
    | "F" as character <> source
    | "G" as character <> source
    | "H" as character <> source
    | "I" as character <> source
    | "J" as character <> source
    | "K" as character <> source
    | "L" as character <> source
    | "M" as character <> source
    | "N" as character <> source
    | "O" as character <> source
    | "P" as character <> source
    | "Q" as character <> source
    | "R" as character <> source
    | "S" as character <> source
    | "T" as character <> source
    | "U" as character <> source
    | "V" as character <> source
    | "W" as character <> source
    | "X" as character <> source
    | "Y" as character <> source
    | "Z" as character <> source
    | "0" as character <> source
    | "1" as character <> source
    | "2" as character <> source
    | "3" as character <> source
    | "4" as character <> source
    | "5" as character <> source
    | "6" as character <> source
    | "7" as character <> source
    | "8" as character <> source
    | "9" as character <> source ->
      lex_identifier(advance(lexer, source), lexed <> character)

    _ -> #(lexer, lexed)
  }
}

fn whitespace(lexer: Lexer, lexed: String) -> #(Lexer, Token) {
  case lexer.source {
    "\t" as space <> source
    | "\u{000B}" as space <> source
    | "\f" as space <> source
    | " " as space <> source
    | "\u{1680}" as space <> source
    | "\u{2000}" as space <> source
    | "\u{2001}" as space <> source
    | "\u{2002}" as space <> source
    | "\u{2003}" as space <> source
    | "\u{2004}" as space <> source
    | "\u{2005}" as space <> source
    | "\u{2006}" as space <> source
    | "\u{2007}" as space <> source
    | "\u{2008}" as space <> source
    | "\u{2009}" as space <> source
    | "\u{200A}" as space <> source
    | "\u{202F}" as space <> source
    | "\u{205F}" as space <> source
    | "\u{3000}" as space <> source
    | "\u{FEFF}" as space <> source ->
      whitespace(advance(lexer, source), lexed <> space)

    _ ->
      case lexer.ignore_whitespace {
        gleam.True -> next(lexer)
        gleam.False -> #(lexer, Whitespace(lexed))
      }
  }
}

fn lex_until_end_of_line(lexer: Lexer) -> #(Lexer, String) {
  let #(before, after) =
    splitter.split_before(lexer.splitters.until_end_of_line, lexer.source)
  #(advance(lexer, after), before)
}

fn advance(lexer: Lexer, source: String) -> Lexer {
  Lexer(..lexer, source:)
}

fn update_mode_with_token(lexer: Lexer, token: Token) -> Lexer {
  let mode = case token {
    // Comments and whitespace don't affect lexing mode
    SingleLineComment(_)
    | MultiLineComment(_)
    | HashBangComment(_)
    | Whitespace(_)
    | LineTerminator(_)
    | EndOfFile -> lexer.mode

    // Values make us look for division
    Identifier(_)
    | PrivateIdentifier(_)
    | Number(_)
    | BigInt(_)
    | String(..)
    | RegularExpression(..)
    | TemplateTail(_) -> TreatSlashAsDivision

    // These keywords act as values, so we look for division after them
    False | Null | This | True -> TreatSlashAsDivision

    // After a grouping we look for division
    RightParen | RightSquare -> TreatSlashAsDivision

    // These can be either postfix or prefix. Either way, we keep the lexing mode the same.
    DoublePlus | DoubleMinus -> lexer.mode

    // In any other case, we look for a regular expression next.
    _ -> TreatSlashAsRegex
  }

  Lexer(..lexer, mode:)
}

fn error(lexer: Lexer, error: Error) -> Lexer {
  Lexer(..lexer, errors: [error, ..lexer.errors])
}

/// A highlighting token, containing information about the kind of syntax
/// being used. Many similar tokens (e.g. all keywords) are grouped together
/// to simplify them.
/// 
/// For syntax tokens, see [`Token`](#Token).
/// 
pub type HighlightToken {
  HighlightWhitespace(String)
  HighlightKeyword(String)
  HighlightVariable(String)
  HighlightClass(String)
  HighlightString(String)
  HighlightRegexp(String)
  HighlightNumber(String)
  HighlightFunction(String)
  HighlightOperator(String)
  HighlightComment(String)
  HighlightPunctuation(String)
  HighlightOther(String)
}

/// Convert a string of JavaScript source code into ansi highlighting.
/// 
/// Colours taken from [`contour`](https://hexdocs.pm/contour):
/// | Token                  | Colour      |
/// | ---------------------- | ----------- |
/// | Keyword                | Yellow      |
/// | Class                  | Cyan        |
/// | Function               | Blue        |
/// | Operator               | Magenta     |
/// | Comment                | Italic grey |
/// | String, Number, Regexp | Green       |
/// | Whitespace, Variable   | No colour   |
///
/// If you wish to use other colours or another format, use `to_tokens`.
/// 
pub fn highlight_ansi(code: String) -> String {
  highlight_tokens(code)
  |> list.fold("", fn(code, token) {
    code
    <> case token {
      HighlightWhitespace(s) -> ansi.reset(s)
      HighlightKeyword(s) -> ansi.yellow(s)
      HighlightVariable(s) -> ansi.reset(s)
      HighlightClass(s) -> ansi.cyan(s)
      HighlightString(s) -> ansi.green(s)
      HighlightRegexp(s) -> ansi.green(s)
      HighlightNumber(s) -> ansi.green(s)
      HighlightFunction(s) -> ansi.blue(s)
      HighlightOperator(s) -> ansi.magenta(s)
      HighlightComment(s) -> ansi.italic(ansi.gray(s))
      HighlightPunctuation(s) -> ansi.reset(s)
      HighlightOther(s) -> ansi.reset(s)
    }
  })
}

/// Convert a string of JavaScript source code into an HTML string.
/// Each token is wrapped in a `<span>` with a class indicating the type of 
/// 
/// Class names taken from [`contour`](https://hexdocs.pm/contour):
/// | Token       | CSS class      |
/// | ----------- | -------------- |
/// | Keyword     | hl-keyword     |
/// | Variable    | hl-variable    |
/// | Class       | hl-class       |
/// | Function    | hl-function    |
/// | Operator    | hl-operator    |
/// | Punctuation | hl-punctuation |
/// | Comment     | hl-comment     |
/// | String      | hl-string      |
/// | Regexp      | hl-regexp      |
/// | Number      | hl-number      |
/// | Whitespace  | no class       |
///
/// Place the output within a `<pre><code>...</code></pre>` and add styling for
/// these CSS classes to get highlighting on your website. Here's some CSS you
/// could use:
///
/// ```css
/// pre code .hl-comment  { color: #d4d4d4; font-style: italic }
/// pre code .hl-function { color: #9ce7ff }
/// pre code .hl-keyword  { color: #ffd596 }
/// pre code .hl-operator { color: #ffaff3 }
/// pre code .hl-string   { color: #c8ffa7 }
/// pre code .hl-number   { color: #c8ffa7 }
/// pre code .hl-regexp   { color: #c8ffa7 }
/// pre code .hl-class    { color: #ffddfa }
/// ```
///
/// If you wish to use another format see `to_ansi` or `to_tokens`.
///
pub fn highlight_html(code: String) -> String {
  highlight_tokens(code)
  |> list.fold("", fn(acc, token) {
    case token {
      HighlightWhitespace(s) -> acc <> s
      HighlightKeyword(s) ->
        acc <> "<span class=hl-keyword>" <> houdini.escape(s) <> "</span>"
      HighlightVariable(s) ->
        acc <> "<span class=hl-variable>" <> houdini.escape(s) <> "</span>"
      HighlightClass(s) ->
        acc <> "<span class=hl-class>" <> houdini.escape(s) <> "</span>"
      HighlightString(s) ->
        acc <> "<span class=hl-string>" <> houdini.escape(s) <> "</span>"
      HighlightRegexp(s) ->
        acc <> "<span class=hl-regexp>" <> houdini.escape(s) <> "</span>"
      HighlightNumber(s) ->
        acc <> "<span class=hl-number>" <> houdini.escape(s) <> "</span>"
      HighlightFunction(s) ->
        acc <> "<span class=hl-function>" <> houdini.escape(s) <> "</span>"
      HighlightOperator(s) ->
        acc <> "<span class=hl-operator>" <> houdini.escape(s) <> "</span>"
      HighlightComment(s) ->
        acc <> "<span class=hl-comment>" <> houdini.escape(s) <> "</span>"
      HighlightPunctuation(s) ->
        acc <> "<span class=hl-punctuation>" <> houdini.escape(s) <> "</span>"
      HighlightOther(s) -> acc <> s
    }
  })
}

/// Convert a string of JavaScript source code into highlighting tokens.
/// Highlighting tokens only contain information about the kind of syntax
/// being used, grouping similar tokens (e.g. all keywords) into one category.
/// 
/// To convert code into syntax tokens, see [`tokenise`](#tokenise).
/// 
pub fn highlight_tokens(code: String) -> List(HighlightToken) {
  let #(tokens, _errors) = tokenise(new(code))
  do_highlight_tokens(tokens, [])
}

fn do_highlight_tokens(
  in: List(Token),
  out: List(HighlightToken),
) -> List(HighlightToken) {
  case in {
    [] -> list.reverse(out)

    // Identifiers and specific constructs
    [Identifier(value), LeftParen, ..in] ->
      do_highlight_tokens(in, [
        HighlightPunctuation("("),
        HighlightFunction(value),
        ..out
      ])
    [ContextualKeyword(keyword), LeftParen, ..in] ->
      do_highlight_tokens(in, [
        HighlightPunctuation("("),
        HighlightFunction(contextual_keyword_name(keyword)),
        ..out
      ])

    [Let, Whitespace(space), ContextualKeyword(keyword), ..in] ->
      do_highlight_tokens(in, [
        HighlightVariable(contextual_keyword_name(keyword)),
        HighlightWhitespace(space),
        HighlightKeyword("let"),
        ..out
      ])
    [Const, Whitespace(space), ContextualKeyword(keyword), ..in] ->
      do_highlight_tokens(in, [
        HighlightVariable(contextual_keyword_name(keyword)),
        HighlightWhitespace(space),
        HighlightKeyword("const"),
        ..out
      ])

    [New, Whitespace(space), Identifier(name), ..in] ->
      do_highlight_tokens(in, [
        HighlightClass(name),
        HighlightWhitespace(space),
        HighlightKeyword("new"),
        ..out
      ])
    [Class, Whitespace(space), Identifier(name), ..in] ->
      do_highlight_tokens(in, [
        HighlightClass(name),
        HighlightWhitespace(space),
        HighlightKeyword("class"),
        ..out
      ])
    [Extends, Whitespace(space), Identifier(name), ..in] ->
      do_highlight_tokens(in, [
        HighlightClass(name),
        HighlightWhitespace(space),
        HighlightKeyword("extends"),
        ..out
      ])
    [Instanceof, Whitespace(space), Identifier(name), ..in] ->
      do_highlight_tokens(in, [
        HighlightClass(name),
        HighlightWhitespace(space),
        HighlightKeyword("instanceof"),
        ..out
      ])

    [Identifier(name), ..in] ->
      do_highlight_tokens(in, [HighlightVariable(name), ..out])
    [PrivateIdentifier(name), ..in] ->
      do_highlight_tokens(in, [HighlightVariable("#" <> name), ..out])

    [ContextualKeyword(keyword), ..in] ->
      do_highlight_tokens(in, [
        HighlightKeyword(contextual_keyword_name(keyword)),
        ..out
      ])

    // Comments and whitespace
    [SingleLineComment(value), ..in] ->
      do_highlight_tokens(in, [HighlightComment("//" <> value), ..out])
    [MultiLineComment(value), ..in] ->
      do_highlight_tokens(in, [HighlightComment("/*" <> value <> "*/"), ..out])
    [HashBangComment(value), ..in] ->
      do_highlight_tokens(in, [HighlightComment("#!" <> value), ..out])
    [Whitespace(value), ..in] ->
      do_highlight_tokens(in, [HighlightWhitespace(value), ..out])
    [LineTerminator(value), ..in] ->
      do_highlight_tokens(in, [HighlightWhitespace(value), ..out])
    [EndOfFile, ..in] -> do_highlight_tokens(in, out)

    // Literals
    [Number(value), ..in] ->
      do_highlight_tokens(in, [HighlightNumber(value), ..out])
    [BigInt(value), ..in] ->
      do_highlight_tokens(in, [HighlightNumber(value <> "n"), ..out])
    [String(quote:, contents:), ..in] ->
      do_highlight_tokens(in, [
        HighlightString(quote <> contents <> quote),
        ..out
      ])
    [RegularExpression(contents:, flags:), ..in] ->
      do_highlight_tokens(in, [
        HighlightRegexp("/" <> contents <> "/" <> flags),
        ..out
      ])
    [TemplateHead(value), ..in] ->
      do_highlight_tokens(in, [HighlightString("`" <> value <> "${"), ..out])
    [TemplateMiddle(value), ..in] ->
      do_highlight_tokens(in, [HighlightString("}" <> value <> "${"), ..out])
    [TemplateTail(value), ..in] ->
      do_highlight_tokens(in, [HighlightString("}" <> value <> "`"), ..out])

    // Keywords
    [Break, ..in] -> do_highlight_tokens(in, [HighlightKeyword("break"), ..out])
    [Case, ..in] -> do_highlight_tokens(in, [HighlightKeyword("case"), ..out])
    [Catch, ..in] -> do_highlight_tokens(in, [HighlightKeyword("catch"), ..out])
    [Class, ..in] -> do_highlight_tokens(in, [HighlightKeyword("class"), ..out])
    [Const, ..in] -> do_highlight_tokens(in, [HighlightKeyword("const"), ..out])
    [Continue, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("continue"), ..out])
    [Debugger, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("debugger"), ..out])
    [Default, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("default"), ..out])
    [Delete, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("delete"), ..out])
    [Do, ..in] -> do_highlight_tokens(in, [HighlightKeyword("do"), ..out])
    [Else, ..in] -> do_highlight_tokens(in, [HighlightKeyword("else"), ..out])
    [Export, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("export"), ..out])
    [Extends, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("extends"), ..out])
    [False, ..in] -> do_highlight_tokens(in, [HighlightKeyword("false"), ..out])
    [Finally, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("finally"), ..out])
    [For, ..in] -> do_highlight_tokens(in, [HighlightKeyword("for"), ..out])
    [Function, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("function"), ..out])
    [If, ..in] -> do_highlight_tokens(in, [HighlightKeyword("if"), ..out])
    [Import, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("import"), ..out])
    [In, ..in] -> do_highlight_tokens(in, [HighlightKeyword("in"), ..out])
    [Instanceof, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("instanceof"), ..out])
    [New, ..in] -> do_highlight_tokens(in, [HighlightKeyword("new"), ..out])
    [Null, ..in] -> do_highlight_tokens(in, [HighlightKeyword("null"), ..out])
    [Return, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("return"), ..out])
    [Super, ..in] -> do_highlight_tokens(in, [HighlightKeyword("super"), ..out])
    [Switch, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("switch"), ..out])
    [This, ..in] -> do_highlight_tokens(in, [HighlightKeyword("this"), ..out])
    [Throw, ..in] -> do_highlight_tokens(in, [HighlightKeyword("throw"), ..out])
    [True, ..in] -> do_highlight_tokens(in, [HighlightKeyword("true"), ..out])
    [Try, ..in] -> do_highlight_tokens(in, [HighlightKeyword("try"), ..out])
    [Typeof, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("typeof"), ..out])
    [Var, ..in] -> do_highlight_tokens(in, [HighlightKeyword("var"), ..out])
    [Void, ..in] -> do_highlight_tokens(in, [HighlightKeyword("void"), ..out])
    [While, ..in] -> do_highlight_tokens(in, [HighlightKeyword("while"), ..out])
    [With, ..in] -> do_highlight_tokens(in, [HighlightKeyword("with"), ..out])

    // Keywords in strict mode
    [Let, ..in] -> do_highlight_tokens(in, [HighlightKeyword("let"), ..out])
    [Static, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("static"), ..out])
    [Yield, ..in] -> do_highlight_tokens(in, [HighlightKeyword("yield"), ..out])

    // Future reserved words
    [Enum, ..in] -> do_highlight_tokens(in, [HighlightKeyword("enum"), ..out])

    // Future reserved words in strict mode
    [Implements, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("implements"), ..out])
    [Interface, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("interface"), ..out])
    [Package, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("package"), ..out])
    [Private, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("private"), ..out])
    [Protected, ..in] ->
      do_highlight_tokens(in, [HighlightKeyword("protected"), ..out])

    // Grouping
    [LeftBrace, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation("{"), ..out])
    [RightBrace, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation("}"), ..out])
    [LeftParen, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation("("), ..out])
    [RightParen, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation(")"), ..out])
    [LeftSquare, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation("["), ..out])
    [RightSquare, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation("]"), ..out])

    // Separators
    [Dot, ..in] -> do_highlight_tokens(in, [HighlightPunctuation("."), ..out])
    [TripleDot, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation("..."), ..out])
    [Semicolon, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation(";"), ..out])
    [Comma, ..in] -> do_highlight_tokens(in, [HighlightPunctuation(","), ..out])
    [Colon, ..in] -> do_highlight_tokens(in, [HighlightPunctuation(":"), ..out])
    [Arrow, ..in] ->
      do_highlight_tokens(in, [HighlightPunctuation("=>"), ..out])

    // Comparison
    [Less, ..in] -> do_highlight_tokens(in, [HighlightOperator("<"), ..out])
    [Greater, ..in] -> do_highlight_tokens(in, [HighlightOperator(">"), ..out])
    [LessEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("<="), ..out])
    [GreaterEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator(">="), ..out])
    [DoubleEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("=="), ..out])
    [BangEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("!="), ..out])
    [TripleEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("==="), ..out])
    [BangDoubleEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("!=="), ..out])

    // Arithmetic
    [Plus, ..in] -> do_highlight_tokens(in, [HighlightOperator("+"), ..out])
    [Minus, ..in] -> do_highlight_tokens(in, [HighlightOperator("-"), ..out])
    [Star, ..in] -> do_highlight_tokens(in, [HighlightOperator("*"), ..out])
    [Slash, ..in] -> do_highlight_tokens(in, [HighlightOperator("/"), ..out])
    [Percent, ..in] -> do_highlight_tokens(in, [HighlightOperator("%"), ..out])
    [DoubleStar, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("**"), ..out])
    [DoublePlus, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("++"), ..out])
    [DoubleMinus, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("--"), ..out])
    [DoubleLess, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("<<"), ..out])
    [DoubleGreater, ..in] ->
      do_highlight_tokens(in, [HighlightOperator(">>"), ..out])
    [TripleGreater, ..in] ->
      do_highlight_tokens(in, [HighlightOperator(">>>"), ..out])
    [Ampersand, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("&"), ..out])
    [Pipe, ..in] -> do_highlight_tokens(in, [HighlightOperator("|"), ..out])
    [Caret, ..in] -> do_highlight_tokens(in, [HighlightOperator("^"), ..out])
    [Tilde, ..in] -> do_highlight_tokens(in, [HighlightOperator("~"), ..out])

    // Logic
    [Bang, ..in] -> do_highlight_tokens(in, [HighlightOperator("!"), ..out])
    [DoubleAmpersand, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("&&"), ..out])
    [DoublePipe, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("||"), ..out])
    [Question, ..in] -> do_highlight_tokens(in, [HighlightOperator("?"), ..out])
    [DoubleQuestion, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("??"), ..out])
    [QuestionDot, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("?."), ..out])

    // Assignment
    [Equal, ..in] -> do_highlight_tokens(in, [HighlightOperator("="), ..out])
    [PlusEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("+="), ..out])
    [MinusEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("-="), ..out])
    [StarEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("*="), ..out])
    [SlashEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("/="), ..out])
    [PercentEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("%="), ..out])
    [DoubleStarEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("**="), ..out])
    [DoubleLessEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("<<="), ..out])
    [DoubleGreaterEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator(">>="), ..out])
    [TripleGreaterEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator(">>>="), ..out])
    [AmpersandEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("&="), ..out])
    [PipeEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("|="), ..out])
    [CaratEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("^="), ..out])
    [DoubleAmpersandEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("&&="), ..out])
    [DoublePipeEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("||="), ..out])
    [DoubleQuestionEqual, ..in] ->
      do_highlight_tokens(in, [HighlightOperator("??="), ..out])

    [Unknown(value), ..in] ->
      do_highlight_tokens(in, [HighlightOther(value), ..out])
    [UnterminatedComment(value), ..in] ->
      do_highlight_tokens(in, [HighlightComment("/*" <> value), ..out])
    [UnterminatedRegularExpression(value), ..in] ->
      do_highlight_tokens(in, [HighlightRegexp("/" <> value), ..out])
    [UnterminatedString(quote:, contents:), ..in] ->
      do_highlight_tokens(in, [HighlightString(quote <> contents), ..out])
    [UnterminatedTemplate(contents), ..in] ->
      do_highlight_tokens(in, [HighlightString(contents), ..out])
  }
}
