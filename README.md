# Just

A JavaScript lexer and syntax highlighter for Gleam!

[![Package Version](https://img.shields.io/hexpm/v/just)](https://hex.pm/packages/just)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/just/)

Just is a JavaScript lexer and syntax highlighter written in Gleam.  
The `just` module, based on [`glexer`](https://hexdocs.mp/glexer) exposes a
standard lexer API, allowing you to convert JavaScript source code into tokens.  
The `just/highlight` module allows you to highlight javascript code using ansi
colours, html or a custom format. Heavily inspired by [`contour`](https://hexdocs.pm/contour).

```sh
gleam add just@1
```

```gleam
import just
import just/highlight

pub fn main() {
  let code = "console.log('Hello, world!');"

  let lexer = just.new(code) |> just.strict_mode
  // Lex syntax tokens for parsing or other uses
  let #(tokens, errors) = just.tokenise(lexer)
  let assert [] = errors
  parse_js(tokens)

  // Highlight with ansi codes to print in the terminal
  let highlighted = highlight.ansi(code)
  io.println(highlighted)

  // Render to html to show in the browser
  let html = highlight.html(code)
  io.println("<pre><code>" <> html <> "</code></pre>")

  // Convert to "highlighting tokens" to highlight in some other way
  let highlight_tokens = highlight.tokens(code)
  highlight_tokens_some_other_way(highlight_tokens)
}
```

Further documentation can be found at <https://hexdocs.pm/just>.

### Missing features
The Just lexer should be able to accurately lex most valid JavaScript programs,
but there are a few things it is missing:

- Proper backtracking for Regular Expressions. Currently the lexer uses the previously
  lexed token to determine whether to lex a `/` character as a division operator or a
  regular expression. This means it can fail in some edge-cases.
- Lexing of full-unicode identifiers. JavaScript supports more than just ASCII characters
  to make up its identifiers. Currently, Just doesn't support non-ASCII characters in
  identifiers.
- Lexing identifier escape-sequences. Similarly, JavaScript allows unicode escape sequences
  as part of identifiers (e.g. `let \u0065 = 10;`). This is currently not supported by Just.
