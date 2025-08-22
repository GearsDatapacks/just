import birdie
import gleam/list
import gleeunit
import just
import simplifile

pub fn main() -> Nil {
  gleeunit.main()
}

fn assert_roundtrip(src: String, allow_errors: Bool) -> Nil {
  let #(tokens, errors) = just.new(src) |> just.tokenise
  case allow_errors {
    True -> Nil
    False -> {
      assert errors == []
      Nil
    }
  }

  assert just.to_source(tokens) == src
}

fn assert_tokens(src: String, tokens: List(just.Token)) -> Nil {
  do_assert_tokens(src, tokens, False)
}

fn assert_tokens_in_strict_mode(src: String, tokens: List(just.Token)) -> Nil {
  do_assert_tokens(src, tokens, True)
}

fn do_assert_tokens(
  src: String,
  tokens: List(just.Token),
  strict_mode: Bool,
) -> Nil {
  let lexer = case strict_mode {
    False -> just.new(src)
    True -> just.strict_mode(just.new(src))
  }
  let #(lexed, errors) = lexer |> just.ignore_whitespace |> just.tokenise
  assert errors == []
  let tokens = list.append(tokens, [just.EndOfFile])
  assert lexed == tokens
}

fn assert_errors(src: String, errors: List(just.Error)) -> Nil {
  do_assert_errors(src, errors, False)
}

fn assert_errors_in_strict_mode(src: String, errors: List(just.Error)) -> Nil {
  do_assert_errors(src, errors, True)
}

fn do_assert_errors(
  src: String,
  errors: List(just.Error),
  strict_mode: Bool,
) -> Nil {
  let lexer = case strict_mode {
    False -> just.new(src)
    True -> just.strict_mode(just.new(src))
  }
  let #(_, found_errors) = just.tokenise(lexer)
  assert found_errors == errors
}

pub fn stdlib_ffi_roundtrip_test() {
  let assert Ok(stdlib_ffi_file) =
    simplifile.read("build/packages/gleam_stdlib/src/gleam_stdlib.mjs")
  let assert Ok(stdlib_dict_file) =
    simplifile.read("build/packages/gleam_stdlib/src/dict.mjs")
  let assert Ok(stdlib_decode_file) =
    simplifile.read(
      "build/packages/gleam_stdlib/src/gleam_stdlib_decode_ffi.mjs",
    )

  assert_roundtrip(stdlib_ffi_file, False)
  assert_roundtrip(stdlib_dict_file, False)
  assert_roundtrip(stdlib_decode_file, False)
}

pub fn tokens_roundtrip_test() {
  let assert Ok(example_file) = simplifile.read("test/tokens.js")

  assert_roundtrip(example_file, False)
}

pub fn errors_roundtrip_test() {
  assert_roundtrip(
    "
let unknown = @;
let ustring = 'This string does not finish
let uregex = /uh oh
let badNumber = 0xabcdefg;
/*
This comment spans the rest of the program.
",
    True,
  )
}

pub fn regex_token_test() {
  let src = "r = /some regex[/\\]\\\\]\\//abcflags;"
  assert_tokens(src, [
    just.Identifier("r"),
    just.Equal,
    just.RegularExpression("some regex[/\\]\\\\]\\/", "abcflags"),
    just.Semicolon,
  ])
}

pub fn division_token_test() {
  let src = "x = a / b;"
  assert_tokens(src, [
    just.Identifier("x"),
    just.Equal,
    just.Identifier("a"),
    just.Slash,
    just.Identifier("b"),
    just.Semicolon,
  ])
}

pub fn ambiguous_slash_token_test() {
  let src =
    "a = b
/hi/g.exec(c).map(d);"
  assert_tokens(src, [
    just.Identifier("a"),
    just.Equal,
    just.Identifier("b"),
    just.Slash,
    just.Identifier("hi"),
    just.Slash,
    just.Identifier("g"),
    just.Dot,
    just.Identifier("exec"),
    just.LeftParen,
    just.Identifier("c"),
    just.RightParen,
    just.Dot,
    just.Identifier("map"),
    just.LeftParen,
    just.Identifier("d"),
    just.RightParen,
    just.Semicolon,
  ])
}

pub fn many_plus_test() {
  let src = "a+++++ ++ 1"
  assert_tokens(src, [
    just.Identifier("a"),
    just.DoublePlus,
    just.DoublePlus,
    just.Plus,
    just.DoublePlus,
    just.Number("1"),
  ])
}

pub fn long_token_test() {
  let src = ">>>= ??= ?. <<= &&= &="
  assert_tokens(src, [
    just.TripleGreaterEqual,
    just.DoubleQuestionEqual,
    just.QuestionDot,
    just.DoubleLessEqual,
    just.DoubleAmpersandEqual,
    just.AmpersandEqual,
  ])
}

pub fn numbers_test() {
  let src =
    "0n 1234n 2.3E10 5.4e-3 1e8 0xABCDEF 0Xabcdef123 0o1234 0O5670 0b101101 0B0010 0xabcdn 0123 0988"
  assert_tokens(src, [
    just.BigInt("0"),
    just.BigInt("1234"),
    just.Number("2.3E10"),
    just.Number("5.4e-3"),
    just.Number("1e8"),
    just.Number("0xABCDEF"),
    just.Number("0Xabcdef123"),
    just.Number("0o1234"),
    just.Number("0O5670"),
    just.Number("0b101101"),
    just.Number("0B0010"),
    just.BigInt("0xabcd"),
    just.Number("0123"),
    just.Number("0988"),
  ])
}

pub fn template_test() {
  let src =
    "`Hello! have some ${1 / 2 * 3} ${'interpola' + 'tion'}.
Even an object: ${{value: 1, wibble: {wobble: 10}}}. That's enough interpolation for me \\
but let\\`s try some \\escaping`"
  assert_tokens(src, [
    just.TemplateHead("Hello! have some "),
    just.Number("1"),
    just.Slash,
    just.Number("2"),
    just.Star,
    just.Number("3"),
    just.TemplateMiddle(" "),
    just.String("'", "interpola"),
    just.Plus,
    just.String("'", "tion"),
    just.TemplateMiddle(".\nEven an object: "),
    just.LeftBrace,
    just.Identifier("value"),
    just.Colon,
    just.Number("1"),
    just.Comma,
    just.Identifier("wibble"),
    just.Colon,
    just.LeftBrace,
    just.Identifier("wobble"),
    just.Colon,
    just.Number("10"),
    just.RightBrace,
    just.RightBrace,
    just.TemplateTail(
      ". That's enough interpolation for me \\\nbut let\\`s try some \\escaping",
    ),
  ])
}

pub fn unknown_character_test() {
  let src = "a@b"
  assert_errors(src, [just.UnknownCharacter("@")])
}

pub fn unterminated_string_eof_test() {
  let src = "\"Some string that doesn't end"
  assert_errors(src, [just.UnterminatedStringLiteral])
}

pub fn unterminated_string_newline_test() {
  let src =
    "\"Some string that tries
to continue on the next line"
  assert_errors(src, [just.UnterminatedStringLiteral])
}

pub fn unterminated_regular_expression_eof_test() {
  let src = "/some[regex]"
  assert_errors(src, [just.UnterminatedRegExpLiteral])
}

pub fn unterminated_regular_expression_newline_test() {
  let src =
    "/regexes[dont]like
newlines"
  assert_errors(src, [just.UnterminatedRegExpLiteral])
}

pub fn unterminated_comment_test() {
  let src =
    "/* This comment
spans the entire
file
and doesn't stop"
  assert_errors(src, [just.UnterminatedMultilineComment])
}

pub fn unterminated_template_test() {
  let src =
    "`The template
that never terminates"
  assert_errors(src, [just.UnterminatedStringLiteral])
}

pub fn unterminated_template_with_expressions_test() {
  let src =
    "`The template which has ${4 / 2}
interpolations
that ${(+\"a\"+[])[0] + \"ever\"} terminates"
  assert_errors(src, [just.UnterminatedTemplateLiteral])
}

pub fn letter_after_number_test() {
  let src = "123hello"
  assert_errors(src, [just.LetterAfterNumber])
}

pub fn letter_after_bigint_test() {
  let src = "123no"
  assert_errors(src, [just.LetterAfterNumber])
}

pub fn bigint_after_float_test() {
  let src = "1.2n"
  assert_errors(src, [just.LetterAfterNumber])
}

pub fn bigint_after_exponent_test() {
  let src = "1.2e3n"
  assert_errors(src, [just.LetterAfterNumber])
}

pub fn bigint_zero_prefix_test() {
  let src = "0712n"
  assert_errors(src, [just.LetterAfterNumber])
}

pub fn bigint_zero_prefix_decimal_test() {
  let src = "0890n"
  assert_errors(src, [just.LetterAfterNumber])
}

pub fn double_numeric_separator_test() {
  let src = "1__2"
  assert_errors(src, [just.NumericSeparatorNotAllowed])
}

pub fn trailing_numeric_separator_test() {
  let src = "1_2_"
  assert_errors(src, [just.NumericSeparatorNotAllowed])
}

pub fn trailing_decimal_numeric_separator_test() {
  let src = "1_2_.3"
  assert_errors(src, [just.NumericSeparatorNotAllowed])
}

pub fn leading_decimal_numeric_separator_test() {
  let src = "1_2._3"
  assert_errors(src, [just.NumericSeparatorNotAllowed])
}

pub fn trailing_exponent_numeric_separator_test() {
  let src = "1_2.3_e4"
  assert_errors(src, [just.NumericSeparatorNotAllowed])
}

pub fn leading_exponent_numeric_separator_test() {
  let src = "1_2.3e_4"
  assert_errors(src, [just.NumericSeparatorNotAllowed])
}

pub fn leading_negative_exponent_numeric_separator_test() {
  let src = "1_2.3e-_4"
  assert_errors(src, [just.NumericSeparatorNotAllowed])
}

pub fn missing_exponent_test() {
  let src = "1.2e"
  assert_errors(src, [just.ExpectedExponent])
}

pub fn missing_negative_exponent_test() {
  let src = "1.2e-"
  assert_errors(src, [just.ExpectedExponent])
}

pub fn empty_private_identifier_test() {
  let src = "let a = #"
  assert_errors(src, [just.InvalidPrivateIdentifier("#")])
}

pub fn number_private_identifier_test() {
  let src = "let a = #0priv"
  assert_errors(src, [just.InvalidPrivateIdentifier("#0priv")])
}

pub fn zero_prefixed_octal_number_in_strict_mode_test() {
  let src = "07767"
  assert_errors_in_strict_mode(src, [just.ZeroPrefixedNumberInStrictMode])
}

pub fn zero_prefixed_decimal_number_in_strict_mode_test() {
  let src = "09876"
  assert_errors_in_strict_mode(src, [just.ZeroPrefixedNumberInStrictMode])
}

pub fn plain_zero_in_strict_mode_test() {
  let src = "0"
  assert_tokens_in_strict_mode(src, [just.Number("0")])
}

fn assert_ansi_highlight(title: String, src: String) -> Nil {
  birdie.snap(
    src <> "\n\n---\n\n" <> just.highlight_ansi(src),
    "highlight_" <> title <> "_ansi",
  )
}

fn assert_html_highlight(title: String, src: String) -> Nil {
  birdie.snap(
    src <> "\n\n---\n\n" <> just.highlight_html(src),
    "highlight_" <> title <> "_html",
  )
}

pub fn basic_program_test() {
  assert_ansi_highlight(
    "basic_program",
    "
export function main() {
  const message = 'Hello, world!';
  console.log(message);
}

main();
",
  )
}

pub fn basic_program_html_test() {
  assert_html_highlight(
    "basic_program",
    "
export function main() {
  const message = 'Hello, world!';
  console.log(message);
}

main();
",
  )
}

pub fn arithmetic_test() {
  assert_ansi_highlight(
    "arithmetic",
    "
let x = 1 + 2 / 3.5e10;
let y = 0n * 0xff8n ** x;
x <= y === false;
",
  )
}

pub fn arithmetic_html_test() {
  assert_html_highlight(
    "arithmetic",
    "
let x = 1 + 2 / 3.5e10;
let y = 0n * 0xff8n ** x;
x <= y === false;
",
  )
}

pub fn comments_test() {
  assert_ansi_highlight(
    "comments",
    "#! shebang!
// This is some kind of comment
console.log(\"Hi\");
/*
Another comment
that spans several lines
*/
let x = /* this on is inline! */ 10;
",
  )
}

pub fn class_test() {
  assert_ansi_highlight(
    "class",
    "
class RegexWrapper {
  constructor(r) {
    this.r = r;
  }
}

const rw = new RegexWrapper(/hello [regex]?/);
console.log(rw instanceof RegexWrapper);
",
  )
}

pub fn errors_test() {
  assert_ansi_highlight(
    "errors",
    "
let unknown = @;
let ustring = 'This string does not finish
let uregex = /uh oh
let badNumber = 0xabcdefg;
/*
This comment spans the rest of the program.
",
  )
}
