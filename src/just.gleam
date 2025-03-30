import gleam/list
import gleam/string
import just/token.{type Token}

pub opaque type Lexer {
  Lexer(
    source: String,
    ignore_comments: Bool,
    ignore_whitespace: Bool,
    strict_mode: Bool,
  )
}

pub fn new(source: String) -> Lexer {
  Lexer(
    source:,
    ignore_comments: False,
    ignore_whitespace: False,
    strict_mode: False,
  )
}

pub fn ignore_comments(lexer: Lexer) -> Lexer {
  Lexer(..lexer, ignore_comments: True)
}

pub fn ignore_whitespace(lexer: Lexer) -> Lexer {
  Lexer(..lexer, ignore_whitespace: True)
}

pub fn strict_mode(lexer: Lexer) -> Lexer {
  Lexer(..lexer, strict_mode: True)
}

pub fn tokenise(lexer: Lexer) -> List(Token) {
  let #(lexer, tokens) = maybe_lex_hashbang_comment(lexer)
  do_tokenise(lexer, tokens)
}

fn maybe_lex_hashbang_comment(lexer: Lexer) -> #(Lexer, List(Token)) {
  case lexer.source {
    "#!" <> source -> {
      let #(lexer, contents) =
        lexer
        |> advance(source)
        |> lex_until_end_of_line("")

      #(lexer, case lexer.ignore_comments {
        True -> []
        False -> [token.HashBangComment(contents)]
      })
    }
    _ -> #(lexer, [])
  }
}

fn do_tokenise(lexer: Lexer, tokens: List(Token)) -> List(Token) {
  case next(lexer) {
    #(_, token.EndOfFile) -> list.reverse([token.EndOfFile, ..tokens])
    #(lexer, token) -> do_tokenise(lexer, [token, ..tokens])
  }
}

fn next(lexer: Lexer) -> #(Lexer, Token) {
  case lexer.source {
    "" -> #(lexer, token.EndOfFile)

    "\u{0009}" as space <> source
    | "\u{000B}" as space <> source
    | "\u{000C}" as space <> source
    | "\u{0020}" as space <> source
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
    | "\u{2029}" as space <> source -> {
      let lexer = advance(lexer, source)
      case lexer.ignore_whitespace {
        True -> next(lexer)
        False -> #(lexer, token.LineTerminator(space))
      }
    }

    "0b" as prefix <> source ->
      lex_radix_number(advance(lexer, source), 2, prefix, False)

    "0o" as prefix <> source ->
      lex_radix_number(advance(lexer, source), 8, prefix, False)

    "0x" as prefix <> source ->
      lex_radix_number(advance(lexer, source), 16, prefix, False)

    "00" as digit <> source
    | "01" as digit <> source
    | "02" as digit <> source
    | "03" as digit <> source
    | "04" as digit <> source
    | "05" as digit <> source
    | "06" as digit <> source
    | "07" as digit <> source ->
      lex_leading_zero_number(advance(lexer, source), digit)

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

    "{" <> source -> #(advance(lexer, source), token.LeftBrace)
    "}" <> source -> #(advance(lexer, source), token.RightBrace)
    "(" <> source -> #(advance(lexer, source), token.LeftParen)
    ")" <> source -> #(advance(lexer, source), token.RightParen)
    "[" <> source -> #(advance(lexer, source), token.LeftSquare)
    "]" <> source -> #(advance(lexer, source), token.RightSquare)

    "..." <> source -> #(advance(lexer, source), token.TripleDot)
    "." <> source -> #(advance(lexer, source), token.Dot)
    ";" <> source -> #(advance(lexer, source), token.Semicolon)
    "," <> source -> #(advance(lexer, source), token.Comma)
    ":" <> source -> #(advance(lexer, source), token.Colon)
    "=>" <> source -> #(advance(lexer, source), token.Arrow)

    "<=" <> source -> #(advance(lexer, source), token.LessEqual)
    ">=" <> source -> #(advance(lexer, source), token.GreaterEqual)
    "===" <> source -> #(advance(lexer, source), token.TripleEqual)
    "!==" <> source -> #(advance(lexer, source), token.BangDoubleEqual)
    "==" <> source -> #(advance(lexer, source), token.DoubleEqual)
    "!=" <> source -> #(advance(lexer, source), token.BangEqual)

    "=" <> source -> #(advance(lexer, source), token.Equal)
    "+=" <> source -> #(advance(lexer, source), token.PlusEqual)
    "-=" <> source -> #(advance(lexer, source), token.MinusEqual)
    "*=" <> source -> #(advance(lexer, source), token.StarEqual)
    "/=" <> source -> #(advance(lexer, source), token.SlashEqual)
    "%=" <> source -> #(advance(lexer, source), token.PercentEqual)
    "**=" <> source -> #(advance(lexer, source), token.DoubleStarEqual)
    "<<=" <> source -> #(advance(lexer, source), token.DoubleLessEqual)
    ">>=" <> source -> #(advance(lexer, source), token.DoubleGreaterEqual)
    ">>>=" <> source -> #(advance(lexer, source), token.TripleGreaterEqual)
    "&=" <> source -> #(advance(lexer, source), token.AmpersandEqual)
    "|=" <> source -> #(advance(lexer, source), token.PipeEqual)
    "^=" <> source -> #(advance(lexer, source), token.CaratEqual)
    "&&=" <> source -> #(advance(lexer, source), token.DoubleAmpersandEqual)
    "||=" <> source -> #(advance(lexer, source), token.DoublePipeEqual)
    "??=" <> source -> #(advance(lexer, source), token.DoubleQuestionEqual)

    "<<" <> source -> #(advance(lexer, source), token.DoubleLess)
    ">>>" <> source -> #(advance(lexer, source), token.TripleGreater)
    ">>" <> source -> #(advance(lexer, source), token.DoubleGreater)

    "!" <> source -> #(advance(lexer, source), token.Bang)
    "&&" <> source -> #(advance(lexer, source), token.DoubleAmpersand)
    "||" <> source -> #(advance(lexer, source), token.DoublePipe)
    "??" <> source -> #(advance(lexer, source), token.DoubleQuestion)
    "?." <> source -> #(advance(lexer, source), token.QuestionDot)
    "?" <> source -> #(advance(lexer, source), token.Question)

    "<" <> source -> #(advance(lexer, source), token.Less)
    ">" <> source -> #(advance(lexer, source), token.Greater)

    "**" <> source -> #(advance(lexer, source), token.DoubleStar)
    "++" <> source -> #(advance(lexer, source), token.DoublePlus)
    "--" <> source -> #(advance(lexer, source), token.DoubleMinus)
    "+" <> source -> #(advance(lexer, source), token.Plus)
    "-" <> source -> #(advance(lexer, source), token.Minus)
    "*" <> source -> #(advance(lexer, source), token.Star)
    "/" <> source -> #(advance(lexer, source), token.Slash)
    "%" <> source -> #(advance(lexer, source), token.Percent)
    "&" <> source -> #(advance(lexer, source), token.Ampersand)
    "|" <> source -> #(advance(lexer, source), token.Pipe)
    "^" <> source -> #(advance(lexer, source), token.Caret)
    "~" <> source -> #(advance(lexer, source), token.Tilde)

    "#" <> source -> {
      let #(lexer, name) = lex_identifier(advance(lexer, source), "")
      #(lexer, token.PrivateIdentifier(name))
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

    "'" as quote <> source | "\"" as quote <> source -> {
      let #(lexer, string) = lex_string(advance(lexer, source), quote, "")
      #(lexer, token.String(quote, string))
    }

    _ -> #(lexer, token.EndOfFile)
  }
}

fn identifier_token(name: String, strict_mode: Bool) -> Token {
  case name {
    // Keywords
    "break" -> token.Break
    "case" -> token.Case
    "catch" -> token.Catch
    "class" -> token.Class
    "const" -> token.Const
    "continue" -> token.Continue
    "debugger" -> token.Debugger
    "default" -> token.Default
    "delete" -> token.Delete
    "do" -> token.Do
    "else" -> token.Else
    "export" -> token.Export
    "extends" -> token.Extends
    "false" -> token.False
    "finally" -> token.Finally
    "for" -> token.For
    "function" -> token.Function
    "if" -> token.If
    "import" -> token.Import
    "in" -> token.In
    "instanceof" -> token.Instanceof
    "new" -> token.New
    "null" -> token.Null
    "return" -> token.Return
    "super" -> token.Super
    "switch" -> token.Switch
    "this" -> token.This
    "throw" -> token.Throw
    "true" -> token.True
    "try" -> token.Try
    "typeof" -> token.Typeof
    "var" -> token.Var
    "void" -> token.Void
    "while" -> token.While
    "with" -> token.With

    // Keywords in strict mode
    "let" if strict_mode -> token.Let
    "static" if strict_mode -> token.Static
    "yield" if strict_mode -> token.Yield

    // Future reserved words
    "enum" -> token.Enum

    // Future reserved words in strict mode
    "implements" if strict_mode -> token.Implements
    "interface" if strict_mode -> token.Interface
    "package" if strict_mode -> token.Package
    "private" if strict_mode -> token.Private
    "protected" if strict_mode -> token.Protected

    // Contextual keywords
    "as" -> token.ContextualKeyword(token.As)
    "async" -> token.ContextualKeyword(token.Async)
    "await" -> token.ContextualKeyword(token.Await)
    "from" -> token.ContextualKeyword(token.From)
    "get" -> token.ContextualKeyword(token.Get)
    "let" -> token.ContextualKeyword(token.ContextualLet)
    "of" -> token.ContextualKeyword(token.Of)
    "set" -> token.ContextualKeyword(token.Set)
    "static" -> token.ContextualKeyword(token.ContextualStatic)
    "yield" -> token.ContextualKeyword(token.ContextualYield)

    _ -> token.Identifier(name)
  }
}

type LexNumberMode {
  Initial
  Decimal
  ExponentInitial
  ExponentAfterMinus
  ExponentBody
}

type DelimitedPosition {
  AfterDecimal
  AfterNumber
  AfterDelimiter
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
    ->
      lex_number(
        advance(lexer, source),
        lexed <> digit,
        case mode {
          ExponentInitial | ExponentAfterMinus -> ExponentBody
          _ -> mode
        },
        AfterNumber,
      )

    "." <> source, Initial, AfterNumber ->
      lex_number(advance(lexer, source), lexed <> ".", Decimal, AfterDecimal)

    "e" <> source, Initial, AfterNumber | "e" <> source, Decimal, AfterNumber ->
      lex_number(
        advance(lexer, source),
        lexed <> "e",
        ExponentInitial,
        AfterDelimiter,
      )

    "-" <> source, ExponentInitial, _ ->
      lex_number(
        advance(lexer, source),
        lexed <> "-",
        ExponentAfterMinus,
        AfterDelimiter,
      )

    "_" <> source, _, AfterNumber ->
      lex_number(advance(lexer, source), lexed <> "_", mode, AfterDelimiter)

    "n" <> source, Initial, AfterNumber -> #(
      advance(lexer, source),
      token.BigInt(lexed),
    )

    _, _, _ -> #(lexer, token.Number(lexed))
  }
}

fn lex_leading_zero_number(lexer: Lexer, lexed: String) -> #(Lexer, Token) {
  case lexer.source {
    "0" as digit <> source
    | "1" as digit <> source
    | "2" as digit <> source
    | "3" as digit <> source
    | "4" as digit <> source
    | "5" as digit <> source
    | "6" as digit <> source
    | "7" as digit <> source ->
      lex_leading_zero_number(advance(lexer, source), lexed <> digit)

    _ -> #(lexer, token.Number(lexed))
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
      lex_radix_number(advance(lexer, source), radix, lexed <> digit, True)

    "2" as digit <> source
      | "3" as digit <> source
      | "4" as digit <> source
      | "5" as digit <> source
      | "6" as digit <> source
      | "7" as digit <> source
      if radix >= 8
    -> lex_radix_number(advance(lexer, source), radix, lexed <> digit, True)

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
    -> lex_radix_number(advance(lexer, source), radix, lexed <> digit, True)

    "n" <> source if valid_delimiter -> #(
      advance(lexer, source),
      token.BigInt(lexed),
    )

    "_" <> source if valid_delimiter ->
      lex_radix_number(advance(lexer, source), radix, lexed <> "_", False)

    _ -> #(lexer, token.Number(lexed))
  }
}

fn lex_string(lexer: Lexer, quote: String, contents: String) -> #(Lexer, String) {
  case string.pop_grapheme(lexer.source) {
    Error(_) -> #(lexer, contents)
    Ok(#(character, source)) if character == quote -> #(
      advance(lexer, source),
      contents,
    )
    Ok(#("\\", source)) ->
      case string.pop_grapheme(source) {
        Error(_) -> #(lexer, contents)
        Ok(#(character, source)) ->
          lex_string(
            advance(lexer, source),
            quote,
            contents <> "\\" <> character,
          )
      }
    Ok(#(character, source)) ->
      lex_string(advance(lexer, source), quote, contents <> character)
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
        True -> next(lexer)
        False -> #(lexer, token.Whitespace(lexed))
      }
  }
}

fn lex_until_end_of_line(lexer: Lexer, lexed: String) -> #(Lexer, String) {
  case lexer.source {
    "\n" <> source
    | "\r" <> source
    | "\u{2028}" <> source
    | "\u{2029}" <> source -> #(advance(lexer, source), lexed)
    _ ->
      case string.pop_grapheme(lexer.source) {
        Error(_) -> #(lexer, lexed)
        Ok(#(char, source)) ->
          lex_until_end_of_line(advance(lexer, source), lexed <> char)
      }
  }
}

fn advance(lexer: Lexer, source: String) -> Lexer {
  Lexer(..lexer, source:)
}
