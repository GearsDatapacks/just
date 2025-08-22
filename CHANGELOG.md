# Changelog

## v2.0.0 - 2025-08-22

- Merged the `just/token` and `just/highlight` modules into one `just` module.

- Renamed several variants and functions to prevent name clashes.
  - `just/token.to_source` -> `just.token_to_source`
  - `just.UnterminatedStringLiteral` -> `just.UnterminatedStringLiteral`
  - `just.UnterminatedMultilineComment` -> `just.UnterminatedMultilineComment`
  - `just.UnterminatedRegExpLiteral` -> `just.UnterminatedRegExpLiteral`
  - `just.UnterminatedTemplateLiteral` -> `just.UnterminatedTemplateLiteral`
  - `just/highlight.tokens` -> `just.highlight_tokens`
  - `just/highlight.ansi` -> `just.highlight_ansi`
  - `just/highlight.html` -> `just.highlight_html`
  - `just/highlight.{type Token}` -> `just.HighlightToken`

## v1.1.0 - 2025-04-02

- Updated the code for lexing Regular Expressions, Strings and Comments to use
  the `splitter` package for improved performance.

## v1.0.0 - 2025-04-01

- Initial release.
