#! This file contains all different types of tokens which `just` understands.

// This is a single line comment

/*
This is a
multiline
* /comment/
*/

someIdentifier #somePrivateIdentifier

1234567890 0b1101011 0xabcdefABCDEF012345697890
0o726363 012343 09378492 0 0xffen 0b10101010101111111111111n
0o3615n 0O123 0B10101 0Xabcd 1. .3 123e12 4E10 1.4e-9 8.3E-22


"A string! with some \" quotes, and other ' quotes, and more ` quotes. Even some escape sequences: \\ \
a newline too."
'A single quoted string, with a " quote, and different \' escape \\\
 sequences.`'

  ; /some regex[/\]\\]\//ig

`A template with no interpolation, but it does have various quotes '"\`\\ and
a newline too.`
`This template has ${1 + 2} different ${{message: "interpolated parts"}} inside of ${"i" + "t"}.`

break
case
catch
class
const
continue
debugger
default
delete
do
else
export
extends
false
finally
for
function
if
import
in
instanceof
new
null
return
super
switch
this
throw
true
try
typeof
var
void
while
with
let
static
yield
enum
implements
interface
package
private
protected

as
async
await
from
get
let
of
static
set
yield

{
}
(
)
[
]
.
...
;
,
:
=>
<
>
<=
>=
==
!=
===
!==
+
-
*
a /
%
**
++
--
<<
>>
>>>
&
|
^
~
!
&&
||
?
??
?.
=
+=
-=
*=
a /=
%=
**=
<<=
>>=
>>>=
&=
|=
^=
&&=
||=
??=
