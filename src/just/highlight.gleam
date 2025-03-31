import gleam/list
import gleam_community/ansi
import houdini
import just
import just/token as t

/// A highlighting token, containing information about the kind of syntax
/// being used. Many similar tokens (e.g. all keywords) are grouped together
/// to simplify them.
/// 
/// For syntax tokens, see `just/token.{type Token}`.
/// 
pub type Token {
  Whitespace(String)
  Keyword(String)
  Variable(String)
  Class(String)
  String(String)
  Regexp(String)
  Number(String)
  Function(String)
  Operator(String)
  Comment(String)
  Punctuation(String)
  Other(String)
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
pub fn to_ansi(code: String) -> String {
  to_tokens(code)
  |> list.fold("", fn(code, token) {
    code
    <> case token {
      Whitespace(s) -> ansi.reset(s)
      Keyword(s) -> ansi.yellow(s)
      Variable(s) -> ansi.reset(s)
      Class(s) -> ansi.cyan(s)
      String(s) -> ansi.green(s)
      Regexp(s) -> ansi.green(s)
      Number(s) -> ansi.green(s)
      Function(s) -> ansi.blue(s)
      Operator(s) -> ansi.magenta(s)
      Comment(s) -> ansi.italic(ansi.gray(s))
      Punctuation(s) -> ansi.reset(s)
      Other(s) -> ansi.reset(s)
    }
  })
}

/// Convert a string of JavaScript source code into an HTML string.
/// Each token is wrapped in a `<span>` with a class indicating the type of token.
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
pub fn to_html(code: String) -> String {
  to_tokens(code)
  |> list.fold("", fn(acc, token) {
    case token {
      Whitespace(s) -> acc <> s
      Keyword(s) ->
        acc <> "<span class=hl-keyword>" <> houdini.escape(s) <> "</span>"
      Variable(s) ->
        acc <> "<span class=hl-variable>" <> houdini.escape(s) <> "</span>"
      Class(s) ->
        acc <> "<span class=hl-class>" <> houdini.escape(s) <> "</span>"
      String(s) ->
        acc <> "<span class=hl-string>" <> houdini.escape(s) <> "</span>"
      Regexp(s) ->
        acc <> "<span class=hl-regexp>" <> houdini.escape(s) <> "</span>"
      Number(s) ->
        acc <> "<span class=hl-number>" <> houdini.escape(s) <> "</span>"
      Function(s) ->
        acc <> "<span class=hl-function>" <> houdini.escape(s) <> "</span>"
      Operator(s) ->
        acc <> "<span class=hl-operator>" <> houdini.escape(s) <> "</span>"
      Comment(s) ->
        acc <> "<span class=hl-comment>" <> houdini.escape(s) <> "</span>"
      Punctuation(s) ->
        acc <> "<span class=hl-punctuation>" <> houdini.escape(s) <> "</span>"
      Other(s) -> acc <> s
    }
  })
}

/// Convert a string of JavaScript source code into highlighting tokens.
/// Highlighting tokens only contain information about the kind of syntax
/// being used, grouping similar tokens (e.g. all keywords) into one category.
/// 
/// To convert code into syntax tokens, see `just.tokenise`.
/// 
pub fn to_tokens(code: String) -> List(Token) {
  let lexer = just.new(code)
  let #(tokens, _errors) = just.tokenise(lexer)
  do_to_tokens(tokens, [])
}

fn do_to_tokens(in: List(t.Token), out: List(Token)) -> List(Token) {
  case in {
    [] -> list.reverse(out)

    // Identifiers and specific constructs
    [t.Identifier(value), t.LeftParen, ..in] ->
      do_to_tokens(in, [Punctuation("("), Function(value), ..out])
    [t.ContextualKeyword(keyword), t.LeftParen, ..in] ->
      do_to_tokens(in, [
        Punctuation("("),
        Function(t.contextual_keyword_name(keyword)),
        ..out
      ])

    [t.Let, t.Whitespace(space), t.ContextualKeyword(keyword), ..in] ->
      do_to_tokens(in, [
        Variable(t.contextual_keyword_name(keyword)),
        Whitespace(space),
        Keyword("let"),
        ..out
      ])
    [t.Const, t.Whitespace(space), t.ContextualKeyword(keyword), ..in] ->
      do_to_tokens(in, [
        Variable(t.contextual_keyword_name(keyword)),
        Whitespace(space),
        Keyword("const"),
        ..out
      ])

    [t.New, t.Whitespace(space), t.Identifier(name), ..in] ->
      do_to_tokens(in, [Class(name), Whitespace(space), Keyword("new"), ..out])
    [t.Class, t.Whitespace(space), t.Identifier(name), ..in] ->
      do_to_tokens(in, [Class(name), Whitespace(space), Keyword("class"), ..out])
    [t.Extends, t.Whitespace(space), t.Identifier(name), ..in] ->
      do_to_tokens(in, [
        Class(name),
        Whitespace(space),
        Keyword("extends"),
        ..out
      ])
    [t.Instanceof, t.Whitespace(space), t.Identifier(name), ..in] ->
      do_to_tokens(in, [
        Class(name),
        Whitespace(space),
        Keyword("instanceof"),
        ..out
      ])

    [t.Identifier(name), ..in] -> do_to_tokens(in, [Variable(name), ..out])
    [t.PrivateIdentifier(name), ..in] ->
      do_to_tokens(in, [Variable("#" <> name), ..out])

    [t.ContextualKeyword(keyword), ..in] ->
      do_to_tokens(in, [Keyword(t.contextual_keyword_name(keyword)), ..out])

    // Comments and whitespace
    [t.SingleLineComment(value), ..in] ->
      do_to_tokens(in, [Comment("//" <> value), ..out])
    [t.MultiLineComment(value), ..in] ->
      do_to_tokens(in, [Comment("/*" <> value <> "*/"), ..out])
    [t.HashBangComment(value), ..in] ->
      do_to_tokens(in, [Comment("#!" <> value), ..out])
    [t.Whitespace(value), ..in] -> do_to_tokens(in, [Whitespace(value), ..out])
    [t.LineTerminator(value), ..in] ->
      do_to_tokens(in, [Whitespace(value), ..out])
    [t.EndOfFile, ..in] -> do_to_tokens(in, out)

    // Literals
    [t.Number(value), ..in] -> do_to_tokens(in, [Number(value), ..out])
    [t.BigInt(value), ..in] -> do_to_tokens(in, [Number(value <> "n"), ..out])
    [t.String(quote:, contents:), ..in] ->
      do_to_tokens(in, [String(quote <> contents <> quote), ..out])
    [t.RegularExpression(value), ..in] ->
      do_to_tokens(in, [Regexp("/" <> value <> "/"), ..out])
    [t.TemplateHead(value), ..in] ->
      do_to_tokens(in, [String("`" <> value <> "${"), ..out])
    [t.TemplateMiddle(value), ..in] ->
      do_to_tokens(in, [String("}" <> value <> "${"), ..out])
    [t.TemplateTail(value), ..in] ->
      do_to_tokens(in, [String("}" <> value <> "`"), ..out])

    // Keywords
    [t.Break, ..in] -> do_to_tokens(in, [Keyword("break"), ..out])
    [t.Case, ..in] -> do_to_tokens(in, [Keyword("case"), ..out])
    [t.Catch, ..in] -> do_to_tokens(in, [Keyword("catch"), ..out])
    [t.Class, ..in] -> do_to_tokens(in, [Keyword("class"), ..out])
    [t.Const, ..in] -> do_to_tokens(in, [Keyword("const"), ..out])
    [t.Continue, ..in] -> do_to_tokens(in, [Keyword("continue"), ..out])
    [t.Debugger, ..in] -> do_to_tokens(in, [Keyword("debugger"), ..out])
    [t.Default, ..in] -> do_to_tokens(in, [Keyword("default"), ..out])
    [t.Delete, ..in] -> do_to_tokens(in, [Keyword("delete"), ..out])
    [t.Do, ..in] -> do_to_tokens(in, [Keyword("do"), ..out])
    [t.Else, ..in] -> do_to_tokens(in, [Keyword("else"), ..out])
    [t.Export, ..in] -> do_to_tokens(in, [Keyword("export"), ..out])
    [t.Extends, ..in] -> do_to_tokens(in, [Keyword("extends"), ..out])
    [t.False, ..in] -> do_to_tokens(in, [Keyword("false"), ..out])
    [t.Finally, ..in] -> do_to_tokens(in, [Keyword("finally"), ..out])
    [t.For, ..in] -> do_to_tokens(in, [Keyword("for"), ..out])
    [t.Function, ..in] -> do_to_tokens(in, [Keyword("function"), ..out])
    [t.If, ..in] -> do_to_tokens(in, [Keyword("if"), ..out])
    [t.Import, ..in] -> do_to_tokens(in, [Keyword("import"), ..out])
    [t.In, ..in] -> do_to_tokens(in, [Keyword("in"), ..out])
    [t.Instanceof, ..in] -> do_to_tokens(in, [Keyword("instanceof"), ..out])
    [t.New, ..in] -> do_to_tokens(in, [Keyword("new"), ..out])
    [t.Null, ..in] -> do_to_tokens(in, [Keyword("null"), ..out])
    [t.Return, ..in] -> do_to_tokens(in, [Keyword("return"), ..out])
    [t.Super, ..in] -> do_to_tokens(in, [Keyword("super"), ..out])
    [t.Switch, ..in] -> do_to_tokens(in, [Keyword("switch"), ..out])
    [t.This, ..in] -> do_to_tokens(in, [Keyword("this"), ..out])
    [t.Throw, ..in] -> do_to_tokens(in, [Keyword("throw"), ..out])
    [t.True, ..in] -> do_to_tokens(in, [Keyword("true"), ..out])
    [t.Try, ..in] -> do_to_tokens(in, [Keyword("try"), ..out])
    [t.Typeof, ..in] -> do_to_tokens(in, [Keyword("typeof"), ..out])
    [t.Var, ..in] -> do_to_tokens(in, [Keyword("var"), ..out])
    [t.Void, ..in] -> do_to_tokens(in, [Keyword("void"), ..out])
    [t.While, ..in] -> do_to_tokens(in, [Keyword("while"), ..out])
    [t.With, ..in] -> do_to_tokens(in, [Keyword("with"), ..out])

    // Keywords in strict mode
    [t.Let, ..in] -> do_to_tokens(in, [Keyword("let"), ..out])
    [t.Static, ..in] -> do_to_tokens(in, [Keyword("static"), ..out])
    [t.Yield, ..in] -> do_to_tokens(in, [Keyword("yield"), ..out])

    // Future reserved words
    [t.Enum, ..in] -> do_to_tokens(in, [Keyword("enum"), ..out])

    // Future reserved words in strict mode
    [t.Implements, ..in] -> do_to_tokens(in, [Keyword("implements"), ..out])
    [t.Interface, ..in] -> do_to_tokens(in, [Keyword("interface"), ..out])
    [t.Package, ..in] -> do_to_tokens(in, [Keyword("package"), ..out])
    [t.Private, ..in] -> do_to_tokens(in, [Keyword("private"), ..out])
    [t.Protected, ..in] -> do_to_tokens(in, [Keyword("protected"), ..out])

    // Grouping
    [t.LeftBrace, ..in] -> do_to_tokens(in, [Punctuation("{"), ..out])
    [t.RightBrace, ..in] -> do_to_tokens(in, [Punctuation("}"), ..out])
    [t.LeftParen, ..in] -> do_to_tokens(in, [Punctuation("("), ..out])
    [t.RightParen, ..in] -> do_to_tokens(in, [Punctuation(")"), ..out])
    [t.LeftSquare, ..in] -> do_to_tokens(in, [Punctuation("["), ..out])
    [t.RightSquare, ..in] -> do_to_tokens(in, [Punctuation("]"), ..out])

    // Separators
    [t.Dot, ..in] -> do_to_tokens(in, [Punctuation("."), ..out])
    [t.TripleDot, ..in] -> do_to_tokens(in, [Punctuation("..."), ..out])
    [t.Semicolon, ..in] -> do_to_tokens(in, [Punctuation(";"), ..out])
    [t.Comma, ..in] -> do_to_tokens(in, [Punctuation(","), ..out])
    [t.Colon, ..in] -> do_to_tokens(in, [Punctuation(":"), ..out])
    [t.Arrow, ..in] -> do_to_tokens(in, [Punctuation("=>"), ..out])

    // Comparison
    [t.Less, ..in] -> do_to_tokens(in, [Operator("<"), ..out])
    [t.Greater, ..in] -> do_to_tokens(in, [Operator(">"), ..out])
    [t.LessEqual, ..in] -> do_to_tokens(in, [Operator("<="), ..out])
    [t.GreaterEqual, ..in] -> do_to_tokens(in, [Operator(">="), ..out])
    [t.DoubleEqual, ..in] -> do_to_tokens(in, [Operator("=="), ..out])
    [t.BangEqual, ..in] -> do_to_tokens(in, [Operator("!="), ..out])
    [t.TripleEqual, ..in] -> do_to_tokens(in, [Operator("==="), ..out])
    [t.BangDoubleEqual, ..in] -> do_to_tokens(in, [Operator("!=="), ..out])

    // Arithmetic
    [t.Plus, ..in] -> do_to_tokens(in, [Operator("+"), ..out])
    [t.Minus, ..in] -> do_to_tokens(in, [Operator("-"), ..out])
    [t.Star, ..in] -> do_to_tokens(in, [Operator("*"), ..out])
    [t.Slash, ..in] -> do_to_tokens(in, [Operator("/"), ..out])
    [t.Percent, ..in] -> do_to_tokens(in, [Operator("%"), ..out])
    [t.DoubleStar, ..in] -> do_to_tokens(in, [Operator("**"), ..out])
    [t.DoublePlus, ..in] -> do_to_tokens(in, [Operator("++"), ..out])
    [t.DoubleMinus, ..in] -> do_to_tokens(in, [Operator("--"), ..out])
    [t.DoubleLess, ..in] -> do_to_tokens(in, [Operator("<<"), ..out])
    [t.DoubleGreater, ..in] -> do_to_tokens(in, [Operator(">>"), ..out])
    [t.TripleGreater, ..in] -> do_to_tokens(in, [Operator(">>>"), ..out])
    [t.Ampersand, ..in] -> do_to_tokens(in, [Operator("&"), ..out])
    [t.Pipe, ..in] -> do_to_tokens(in, [Operator("|"), ..out])
    [t.Caret, ..in] -> do_to_tokens(in, [Operator("^"), ..out])
    [t.Tilde, ..in] -> do_to_tokens(in, [Operator("~"), ..out])

    // Logic
    [t.Bang, ..in] -> do_to_tokens(in, [Operator("!"), ..out])
    [t.DoubleAmpersand, ..in] -> do_to_tokens(in, [Operator("&&"), ..out])
    [t.DoublePipe, ..in] -> do_to_tokens(in, [Operator("||"), ..out])
    [t.Question, ..in] -> do_to_tokens(in, [Operator("?"), ..out])
    [t.DoubleQuestion, ..in] -> do_to_tokens(in, [Operator("??"), ..out])
    [t.QuestionDot, ..in] -> do_to_tokens(in, [Operator("?."), ..out])

    // Assignment
    [t.Equal, ..in] -> do_to_tokens(in, [Operator("="), ..out])
    [t.PlusEqual, ..in] -> do_to_tokens(in, [Operator("+="), ..out])
    [t.MinusEqual, ..in] -> do_to_tokens(in, [Operator("-="), ..out])
    [t.StarEqual, ..in] -> do_to_tokens(in, [Operator("*="), ..out])
    [t.SlashEqual, ..in] -> do_to_tokens(in, [Operator("/="), ..out])
    [t.PercentEqual, ..in] -> do_to_tokens(in, [Operator("%="), ..out])
    [t.DoubleStarEqual, ..in] -> do_to_tokens(in, [Operator("**="), ..out])
    [t.DoubleLessEqual, ..in] -> do_to_tokens(in, [Operator("<<="), ..out])
    [t.DoubleGreaterEqual, ..in] -> do_to_tokens(in, [Operator(">>="), ..out])
    [t.TripleGreaterEqual, ..in] -> do_to_tokens(in, [Operator(">>>="), ..out])
    [t.AmpersandEqual, ..in] -> do_to_tokens(in, [Operator("&="), ..out])
    [t.PipeEqual, ..in] -> do_to_tokens(in, [Operator("|="), ..out])
    [t.CaratEqual, ..in] -> do_to_tokens(in, [Operator("^="), ..out])
    [t.DoubleAmpersandEqual, ..in] -> do_to_tokens(in, [Operator("&&="), ..out])
    [t.DoublePipeEqual, ..in] -> do_to_tokens(in, [Operator("||="), ..out])
    [t.DoubleQuestionEqual, ..in] -> do_to_tokens(in, [Operator("??="), ..out])

    [t.Unknown(value), ..] -> do_to_tokens(in, [Other(value), ..out])
    [t.UnterminatedComment(value), ..] ->
      do_to_tokens(in, [Comment("/*" <> value), ..out])
    [t.UnterminatedRegularExpression(value), ..] ->
      do_to_tokens(in, [Regexp("/" <> value), ..out])
    [t.UnterminatedString(quote:, contents:), ..] ->
      do_to_tokens(in, [String(quote <> contents), ..out])
    [t.UnterminatedTemplate(contents), ..] ->
      do_to_tokens(in, [String(contents), ..out])
  }
}
