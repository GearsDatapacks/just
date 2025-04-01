import birdie
import just/highlight

fn assert_ansi_highlight(title: String, src: String) -> Nil {
  birdie.snap(
    src <> "\n\n---\n\n" <> highlight.ansi(src),
    "highlight_" <> title <> "_ansi",
  )
}

fn assert_html_highlight(title: String, src: String) -> Nil {
  birdie.snap(
    src <> "\n\n---\n\n" <> highlight.html(src),
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
