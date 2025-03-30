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
  case lexer.source {
    "" -> list.reverse(tokens)

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
      let #(lexer, tokens) = whitespace(advance(lexer, source), tokens, space)
      do_tokenise(lexer, tokens)
    }

    "\u{000A}" as space <> source
    | "\u{000D}" as space <> source
    | "\u{2028}" as space <> source
    | "\u{2029}" as space <> source ->
      do_tokenise(advance(lexer, source), case lexer.ignore_whitespace {
        True -> tokens
        False -> [token.LineTerminator(space), ..tokens]
      })

    "{" <> source ->
      do_tokenise(advance(lexer, source), [token.LeftBrace, ..tokens])
    "}" <> source ->
      do_tokenise(advance(lexer, source), [token.RightBrace, ..tokens])
    "(" <> source ->
      do_tokenise(advance(lexer, source), [token.LeftParen, ..tokens])
    ")" <> source ->
      do_tokenise(advance(lexer, source), [token.RightParen, ..tokens])
    "[" <> source ->
      do_tokenise(advance(lexer, source), [token.LeftSquare, ..tokens])
    "]" <> source ->
      do_tokenise(advance(lexer, source), [token.RightSquare, ..tokens])

    "..." <> source ->
      do_tokenise(advance(lexer, source), [token.TripleDot, ..tokens])
    "." <> source -> do_tokenise(advance(lexer, source), [token.Dot, ..tokens])
    ";" <> source ->
      do_tokenise(advance(lexer, source), [token.Semicolon, ..tokens])
    "," <> source ->
      do_tokenise(advance(lexer, source), [token.Comma, ..tokens])
    ":" <> source ->
      do_tokenise(advance(lexer, source), [token.Colon, ..tokens])
    "=>" <> source ->
      do_tokenise(advance(lexer, source), [token.Arrow, ..tokens])

    "<=" <> source ->
      do_tokenise(advance(lexer, source), [token.LessEqual, ..tokens])
    ">=" <> source ->
      do_tokenise(advance(lexer, source), [token.GreaterEqual, ..tokens])
    "===" <> source ->
      do_tokenise(advance(lexer, source), [token.TripleEqual, ..tokens])
    "!==" <> source ->
      do_tokenise(advance(lexer, source), [token.BangDoubleEqual, ..tokens])
    "==" <> source ->
      do_tokenise(advance(lexer, source), [token.DoubleEqual, ..tokens])
    "!=" <> source ->
      do_tokenise(advance(lexer, source), [token.BangEqual, ..tokens])

    "=" <> source ->
      do_tokenise(advance(lexer, source), [token.Equal, ..tokens])
    "+=" <> source ->
      do_tokenise(advance(lexer, source), [token.PlusEqual, ..tokens])
    "-=" <> source ->
      do_tokenise(advance(lexer, source), [token.MinusEqual, ..tokens])
    "*=" <> source ->
      do_tokenise(advance(lexer, source), [token.StarEqual, ..tokens])
    "/=" <> source ->
      do_tokenise(advance(lexer, source), [token.SlashEqual, ..tokens])
    "%=" <> source ->
      do_tokenise(advance(lexer, source), [token.PercentEqual, ..tokens])
    "**=" <> source ->
      do_tokenise(advance(lexer, source), [token.DoubleStarEqual, ..tokens])
    "<<=" <> source ->
      do_tokenise(advance(lexer, source), [token.DoubleLessEqual, ..tokens])
    ">>=" <> source ->
      do_tokenise(advance(lexer, source), [token.DoubleGreaterEqual, ..tokens])
    ">>>=" <> source ->
      do_tokenise(advance(lexer, source), [token.TripleGreaterEqual, ..tokens])
    "&=" <> source ->
      do_tokenise(advance(lexer, source), [token.AmpersandEqual, ..tokens])
    "|=" <> source ->
      do_tokenise(advance(lexer, source), [token.PipeEqual, ..tokens])
    "^=" <> source ->
      do_tokenise(advance(lexer, source), [token.CaratEqual, ..tokens])
    "&&=" <> source ->
      do_tokenise(advance(lexer, source), [token.DoubleAmpersandEqual, ..tokens])
    "||=" <> source ->
      do_tokenise(advance(lexer, source), [token.DoublePipeEqual, ..tokens])
    "??=" <> source ->
      do_tokenise(advance(lexer, source), [token.DoubleQuestionEqual, ..tokens])

    "<<" <> source ->
      do_tokenise(advance(lexer, source), [token.DoubleLess, ..tokens])
    ">>>" <> source ->
      do_tokenise(advance(lexer, source), [token.TripleGreater, ..tokens])
    ">>" <> source ->
      do_tokenise(advance(lexer, source), [token.DoubleGreater, ..tokens])

    "!" <> source -> do_tokenise(advance(lexer, source), [token.Bang, ..tokens])
    "&&" <> source ->
      do_tokenise(advance(lexer, source), [token.DoubleAmpersand, ..tokens])
    "||" <> source ->
      do_tokenise(advance(lexer, source), [token.DoublePipe, ..tokens])
    "??" <> source ->
      do_tokenise(advance(lexer, source), [token.DoubleQuestion, ..tokens])
    "?." <> source ->
      do_tokenise(advance(lexer, source), [token.QuestionDot, ..tokens])
    "?" <> source ->
      do_tokenise(advance(lexer, source), [token.Question, ..tokens])

    "<" <> source -> do_tokenise(advance(lexer, source), [token.Less, ..tokens])
    ">" <> source ->
      do_tokenise(advance(lexer, source), [token.Greater, ..tokens])

    "**" <> source ->
      do_tokenise(advance(lexer, source), [token.DoubleStar, ..tokens])
    "++" <> source ->
      do_tokenise(advance(lexer, source), [token.DoublePlus, ..tokens])
    "--" <> source ->
      do_tokenise(advance(lexer, source), [token.DoubleMinus, ..tokens])
    "+" <> source -> do_tokenise(advance(lexer, source), [token.Plus, ..tokens])
    "-" <> source ->
      do_tokenise(advance(lexer, source), [token.Minus, ..tokens])
    "*" <> source -> do_tokenise(advance(lexer, source), [token.Star, ..tokens])
    "/" <> source ->
      do_tokenise(advance(lexer, source), [token.Slash, ..tokens])
    "%" <> source ->
      do_tokenise(advance(lexer, source), [token.Percent, ..tokens])
    "&" <> source ->
      do_tokenise(advance(lexer, source), [token.Ampersand, ..tokens])
    "|" <> source -> do_tokenise(advance(lexer, source), [token.Pipe, ..tokens])
    "^" <> source ->
      do_tokenise(advance(lexer, source), [token.Caret, ..tokens])
    "~" <> source ->
      do_tokenise(advance(lexer, source), [token.Tilde, ..tokens])

    "#" <> source -> {
      let #(lexer, name) = lex_identifier(advance(lexer, source), "")
      do_tokenise(lexer, [token.PrivateIdentifier(name), ..tokens])
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

      let token = case name {
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
        "let" if lexer.strict_mode -> token.Let
        "static" if lexer.strict_mode -> token.Static
        "yield" if lexer.strict_mode -> token.Yield

        // Future reserved words
        "enum" -> token.Enum

        // Future reserved words in strict mode
        "implements" if lexer.strict_mode -> token.Implements
        "interface" if lexer.strict_mode -> token.Interface
        "package" if lexer.strict_mode -> token.Package
        "private" if lexer.strict_mode -> token.Private
        "protected" if lexer.strict_mode -> token.Protected

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

      do_tokenise(lexer, [token, ..tokens])
    }

    _ -> list.reverse(tokens)
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

fn whitespace(
  lexer: Lexer,
  tokens: List(Token),
  lexed: String,
) -> #(Lexer, List(Token)) {
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
      whitespace(advance(lexer, source), tokens, lexed <> space)

    _ -> #(lexer, case lexer.ignore_whitespace {
      True -> tokens
      False -> [token.Whitespace(lexed), ..tokens]
    })
  }
}

fn lex_until_end_of_line(lexer: Lexer, lexed: String) -> #(Lexer, String) {
  case lexer.source {
    "" -> #(lexer, lexed)
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
