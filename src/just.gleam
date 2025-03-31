import gleam/list
import gleam/string
import just/token.{type Token}

pub opaque type Lexer {
  Lexer(
    source: String,
    ignore_comments: Bool,
    ignore_whitespace: Bool,
    strict_mode: Bool,
    mode: LexerMode,
    errors: List(Error),
  )
}

type LexerMode {
  TreatSlashAsRegex
  TreatSlashAsDivision
}

pub type Error {
  UnknownCharacter(character: String)
  UnterminatedString
  UnterminatedComment
  UnterminatedRegularExpression
  UnterminatedTemplate
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
    UnterminatedComment -> "Unterminated comment"
    UnterminatedRegularExpression -> "Unterminated regular expression literal"
    UnterminatedString -> "Unterminated strint literal"
    UnterminatedTemplate -> "Unterminated template literal"
    ZeroPrefixedNumberInStrictMode ->
      "Zero-prefixed numbers are not allowed in strict mode"
  }
}

pub fn new(source: String) -> Lexer {
  Lexer(
    source:,
    ignore_comments: False,
    ignore_whitespace: False,
    strict_mode: False,
    mode: TreatSlashAsRegex,
    errors: [],
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

pub fn tokenise(lexer: Lexer) -> #(List(Token), List(Error)) {
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

fn do_tokenise(lexer: Lexer, tokens: List(Token)) -> #(List(Token), List(Error)) {
  case next(lexer) {
    #(lexer, token.EndOfFile) -> #(
      list.reverse([token.EndOfFile, ..tokens]),
      list.reverse(lexer.errors),
    )
    #(lexer, token.TemplateHead(_) as token) -> {
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
    True -> #(lexer, token)
    False -> next(lexer)
  }
}

fn next(lexer: Lexer) -> #(Lexer, Token) {
  case lexer.source {
    "" -> #(lexer, token.EndOfFile)

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
        token.LineTerminator(space),
        !lexer.ignore_whitespace,
      )

    "//" <> source -> {
      let #(lexer, contents) = lex_until_end_of_line(advance(lexer, source), "")
      maybe_token(
        lexer,
        token.SingleLineComment(contents),
        !lexer.ignore_comments,
      )
    }
    "/*" <> source -> lex_multiline_comment(advance(lexer, source), "")

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
      lex_leading_zero_number(advance(lexer, source), digit, False)

    "08" as digit <> source | "09" as digit <> source ->
      lex_leading_zero_number(advance(lexer, source), digit, True)

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
      lex_regex(advance(lexer, source), "", False)

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

    "'" as quote <> source | "\"" as quote <> source ->
      lex_string(advance(lexer, source), quote, "")

    "`" <> source -> lex_template_head(advance(lexer, source), "")

    _ ->
      case string.pop_grapheme(lexer.source) {
        Error(_) -> #(lexer, token.EndOfFile)
        Ok(#(character, source)) -> #(
          lexer |> advance(source) |> error(UnknownCharacter(character)),
          token.Unknown(character),
        )
      }
  }
}

fn lex_multiline_comment(lexer: Lexer, lexed: String) -> #(Lexer, Token) {
  case lexer.source {
    "*/" <> source ->
      maybe_token(
        advance(lexer, source),
        token.MultiLineComment(lexed),
        !lexer.ignore_comments,
      )
    _ ->
      case string.pop_grapheme(lexer.source) {
        Error(_) ->
          maybe_token(
            error(lexer, UnterminatedComment),
            token.MultiLineComment(lexed),
            !lexer.ignore_comments,
          )
        Ok(#(char, source)) ->
          lex_multiline_comment(advance(lexer, source), lexed <> char)
      }
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

    "e-" <> source, Initial, AfterNumber | "e-" <> source, Decimal, AfterNumber ->
      lex_number(advance(lexer, source), lexed <> "e-", Exponent, AfterExponent)

    "e" <> source, Initial, AfterNumber | "e" <> source, Decimal, AfterNumber ->
      lex_number(advance(lexer, source), lexed <> "e", Exponent, AfterExponent)

    "_" <> source, _, AfterNumber ->
      lex_number(advance(lexer, source), lexed <> "_", mode, AfterSeparator)

    "_" <> _, _, _ -> #(
      error(lexer, NumericSeparatorNotAllowed),
      token.Number(lexed),
    )

    "n" <> source, Initial, AfterNumber -> #(
      ensure_no_letters_after_numbers(advance(lexer, source)),
      token.BigInt(lexed),
    )

    _, _, AfterExponent -> #(
      error(lexer, ExpectedExponent),
      token.Number(lexed),
    )

    _, _, AfterSeparator -> #(
      error(lexer, NumericSeparatorNotAllowed),
      token.Number(lexed),
    )

    _, _, _ -> #(ensure_no_letters_after_numbers(lexer), token.Number(lexed))
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
        False -> lexer
        True -> error(lexer, ZeroPrefixedNumberInStrictMode)
      }
      #(lexer, token.Number(lexed))
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
  case string.pop_grapheme(lexer.source) {
    Error(_) -> #(
      error(lexer, UnterminatedString),
      token.UnterminatedString(quote:, contents:),
    )
    Ok(#("\n", _source)) | Ok(#("\r", _source)) -> #(
      error(lexer, UnterminatedString),
      token.UnterminatedString(quote:, contents:),
    )

    Ok(#(character, source)) if character == quote -> #(
      advance(lexer, source),
      token.String(quote:, contents:),
    )
    Ok(#("\\", source)) ->
      case string.pop_grapheme(source) {
        Error(_) -> #(
          error(lexer, UnterminatedString),
          token.UnterminatedString(quote:, contents:),
        )
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

fn lex_template_head(lexer: Lexer, lexed: String) -> #(Lexer, Token) {
  case lexer.source {
    "${" <> source -> #(advance(lexer, source), token.TemplateHead(lexed))
    "`" <> source -> #(advance(lexer, source), token.String("`", lexed))
    "\\" <> source ->
      case string.pop_grapheme(source) {
        Error(_) -> #(
          error(lexer, UnterminatedString),
          token.UnterminatedString("`", lexed),
        )
        Ok(#(character, source)) ->
          lex_template_head(advance(lexer, source), lexed <> "\\" <> character)
      }
    _ ->
      case string.pop_grapheme(lexer.source) {
        Error(_) -> #(
          error(lexer, UnterminatedString),
          token.UnterminatedString("`", lexed),
        )
        Ok(#(character, source)) ->
          lex_template_head(advance(lexer, source), lexed <> character)
      }
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
    LexTemplate(lexed) ->
      case lexer.source {
        "`" <> source -> #(advance(lexer, source), [
          token.TemplateTail(lexed),
          ..tokens
        ])
        "${" <> source -> {
          let lexer = advance(lexer, source)
          let token = token.TemplateMiddle(lexed)
          lex_template_parts(
            update_mode_with_token(lexer, token),
            [token, ..tokens],
            LexTokens(0),
          )
        }
        "\\" <> source ->
          case string.pop_grapheme(source) {
            Error(_) -> #(error(lexer, UnterminatedTemplate), [
              token.UnterminatedTemplate(lexed),
              ..tokens
            ])
            Ok(#(character, source)) ->
              lex_template_parts(
                advance(lexer, source),
                tokens,
                LexTemplate(lexed <> "\\" <> character),
              )
          }
        _ ->
          case string.pop_grapheme(lexer.source) {
            Error(_) -> #(error(lexer, UnterminatedTemplate), [
              token.UnterminatedTemplate(lexed),
              ..tokens
            ])
            Ok(#(character, source)) ->
              lex_template_parts(
                advance(lexer, source),
                tokens,
                LexTemplate(lexed <> character),
              )
          }
      }

    LexTokens(bracket_level) ->
      case next(lexer) {
        #(lexer, token.EndOfFile) -> #(lexer, tokens)
        #(lexer, token.RightBrace) if bracket_level == 0 ->
          lex_template_parts(lexer, tokens, LexTemplate(""))
        #(lexer, token.RightBrace as token) ->
          lex_template_parts(
            update_mode_with_token(lexer, token),
            [token, ..tokens],
            LexTokens(bracket_level - 1),
          )
        #(lexer, token.LeftBrace as token) ->
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
  case lexer.source {
    "/" <> source if !in_group -> #(
      advance(lexer, source),
      token.RegularExpression(lexed),
    )
    "[" <> source -> lex_regex(advance(lexer, source), lexed <> "[", True)
    "]" <> source -> lex_regex(advance(lexer, source), lexed <> "]", False)
    "\n" <> _source
    | "\r" <> _source
    | "\u{2028}" <> _source
    | "\u{2029}" <> _source -> #(
      error(lexer, UnterminatedRegularExpression),
      token.UnterminatedRegularExpression(lexed),
    )
    "\\" <> source ->
      case string.pop_grapheme(source) {
        Error(_) -> #(
          error(lexer, UnterminatedRegularExpression),
          token.UnterminatedRegularExpression(lexed),
        )
        Ok(#(character, source)) ->
          lex_regex(
            advance(lexer, source),
            lexed <> "\\" <> character,
            in_group,
          )
      }
    _ ->
      case string.pop_grapheme(lexer.source) {
        Error(_) -> #(
          error(lexer, UnterminatedRegularExpression),
          token.UnterminatedRegularExpression(lexed),
        )
        Ok(#(character, source)) ->
          lex_regex(advance(lexer, source), lexed <> character, in_group)
      }
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
    "\n" <> _source
    | "\r" <> _source
    | "\u{2028}" <> _source
    | "\u{2029}" <> _source -> #(lexer, lexed)
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

fn update_mode_with_token(lexer: Lexer, token: Token) -> Lexer {
  let mode = case token {
    // Comments and whitespace don't affect lexing mode
    token.SingleLineComment(_)
    | token.MultiLineComment(_)
    | token.HashBangComment(_)
    | token.Whitespace(_)
    | token.LineTerminator(_)
    | token.EndOfFile -> lexer.mode

    // Values make us look for division
    token.Identifier(_)
    | token.PrivateIdentifier(_)
    | token.Number(_)
    | token.BigInt(_)
    | token.String(..)
    | token.RegularExpression(_)
    | token.TemplateTail(_) -> TreatSlashAsDivision

    // These keywords act as values, so we look for division after them
    token.False | token.Null | token.This | token.True -> TreatSlashAsDivision

    // After a grouping we look for division
    token.RightParen | token.RightSquare -> TreatSlashAsDivision

    // These can be either postfix or prefix. Either way, we keep the lexing mode the same.
    token.DoublePlus | token.DoubleMinus -> lexer.mode

    // In any other case, we look for a regular expression next.
    _ -> TreatSlashAsRegex
  }

  Lexer(..lexer, mode:)
}

fn error(lexer: Lexer, error: Error) -> Lexer {
  Lexer(..lexer, errors: [error, ..lexer.errors])
}
