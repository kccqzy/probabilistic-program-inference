# A Simple Probabilistic Program Inference Tool Supporting Loops

## Background

Probabilistic programming languages resemble typical programming
languages, but instead of being compiled and run, their goal is to
enable the use of the language as a medium to express a probabilistic
model and to enable automatic inference. This project provides an
inference tool for a rather simple kind of imperative probabilistic
programming language.

The probabilistic programming language (hereafter referred to as PPL)
described in this repository supports booleans and 8-bit unsigned
integers as its data types, and allows a wide variety of control flow
constructs, including conditionals, *loops*, including those loops whose
number of iterations isn't fixed. The PPL adds two fundamental language
constructs to allow probabilistic programming:

*   Drawing from a Bernoulli distribution or a uniform random distribution
*   Observation of an expression

Their meaning will become clear in the following examples.

## Running the Tool

Make sure you have a recent GHC and cabal installation. Then run

```
cabal new-run prob -- --infer /path/to/program.txt
```

to perform inference on a probabilistic program contained in the
mentioned file. Passing `--eval N` instead of `--infer` samples N
executions of the program rather than solving it exactly. This may be
useful to check the inference or if the inference is too slow.

If you do not wish to compile, download a pre-compiled executable from
the [GitHub release
page](https://github.com/kccqzy/probabilistic-program-inference/releases/tag/v2.0).
Releases are cut infrequently.

## Example Programs

### First Example

The simplest possible (while being non-trivial and probabilistic)
program models the flip of a fair coin:

```
r ~ bernoulli 0.5;
```

Save the above text in a file, and run the inference tool on that file.
The result is

```
═════════════╤════
"r" -> false │ 1/2
"r" ->  true │ 1/2
```

This means that, the inference engine determined that at the end of the
program, there are two outcomes: the variable `r` being either true or
false, (by which the author of the probabilistic program means tails or
heads), both with a probability of one half each.

### Second Example

We can of course flip two fair coins instead. These flips will be independent:

```
r1 ~ bernoulli 0.5;
r2 ~ bernoulli 0.5;
```

The inference tool reports:

```
════════════════════════════╤════
"r1" -> false "r2" -> false │ 1/4
"r1" -> false "r2" ->  true │ 1/4
"r1" ->  true "r2" -> false │ 1/4
"r1" ->  true "r2" ->  true │ 1/4
```

### Third Example

It is possible to introduce conditionals. For example, suppose we first
flip two coins; then only when both are tail (true) do we flip a third coin:

```
r1 ~ bernoulli 0.5;
r2 ~ bernoulli 0.5;

if r1 and r2 then {
  r3 ~ bernoulli 0.5;
} else {
  r3 := false;
}
```

The inference tool reports thus:

```
══════════════════════════════════════════╤════
"r1" -> false "r2" -> false "r3" -> false │ 1/4
"r1" -> false "r2" ->  true "r3" -> false │ 1/4
"r1" ->  true "r2" -> false "r3" -> false │ 1/4
"r1" ->  true "r2" ->  true "r3" -> false │ 1/8
"r1" ->  true "r2" ->  true "r3" ->  true │ 1/8
```

Notice that this time the probability figures are different, and some
outcomes are omitted because they are impossible. It is simply
impossible to have the first two coins not both true and the third also
true.

### Fourth Example

Conditionals may be convenient. But the real power comes with loops.
Suppose we would like to keep flipping a coin until it's false, and we
would like to figure out whether the number of times we flipped is even
or odd.

```
numberOfTimesOdd := false; 

do {
   coin ~ bernoulli 0.5;
   numberOfTimesOdd := not numberOfTimesOdd;
} while coin;
```

Running this through the tool gives this result:

```
════════════════════════════════════════════╤════
"coin" -> false "numberOfTimesOdd" ->  true │ 2/3
"coin" -> false "numberOfTimesOdd" -> false │ 1/3
```

First notice that in all possible outcomes, the variable `coin` is
always false, because if it were true, the loop would not terminate.

Second notice that it gave a probability of 2/3 and 1/3. How so? Well,
the probability that the number of times flipped is odd, is the sum of
the probabilities that the number of times flipped is 1, 3, 5, 7, etc,
so we have the infinite sum 2^(-1)+2^(-3)+2^(-5)+… which we can solve
manually to be 2/3, or trust [Wolfram
Alpha](https://www.wolframalpha.com/input/?i=Sum%5B2%5E%28-%282k%2B1%29%29%2C%7Bk%2C0%2CInfinity%7D%5D).

### Fifth Example

It would be nice in the previous program if we could actually use an
integer variable rather than keeping track of the last bit of a
conceptually integral variable. These days we can just declare one:

```
i : u8;
coin : bool;

do {
  coin ~ bernoulli 0.5;
  i := i + 1;
} while coin and i < 10;
```

which reports a geometric distribution over `i` directly:

```
═══════════════════════════╤═════════════════════
"coin" -> false "i" ->   1 │ 1/2
"coin" -> false "i" ->   2 │ 1/4
"coin" -> false "i" ->   3 │ 1/8
"coin" -> false "i" ->   4 │ 1/16
"coin" -> false "i" ->   5 │ 1/32
"coin" -> false "i" ->   6 │ 1/64 (≈ 0.015625)
"coin" -> false "i" ->   7 │ 1/128 (≈ 0.0078125)
"coin" -> false "i" ->   8 │ 1/256 (≈ 0.0039062)
"coin" -> false "i" ->   9 │ 1/512 (≈ 0.0019531)
"coin" ->  true "i" ->  10 │ 1/1024 (≈ 0.0009765)
"coin" -> false "i" ->  10 │ 1/1024 (≈ 0.0009765)
```

### Sixth Example

With loops, it is then obviously possible for the program not to
terminate. For example this one:

```
b := true;

while true do {
  b := not b
}
```

When run through the inference tool, the tool produces no
answer:

```
No results produced.
```

The reason is that the tool assumes that the program will terminate. All
probabilities are reported in terms of conditioning on termination. This program
does not terminate, therefore no probabilities are reported.

### Seventh Example

More sophisticated infinite loops are also possible. Consider for
example this program:

```
x ~ bernoulli 0.05;
do {
    y ~ bernoulli 0.25;
} while x;
```

Here, x can be true or false. But if x were to be true, the program
would never terminate. Since the program terminated, x must have been
false. So the loop body must be been run once. The tool thus reports:

```
══════════════════════════╤════
"x" -> false "y" -> false │ 3/4
"x" -> false "y" ->  true │ 1/4
```

### Eighth Example

So far we've only played with Bernoulli trials for booleans. For integers, we
have uniform random.

```
die: u8;
die ~ uniform 1 6;
```

This models rolling a fair die.

Unsurprisingly, this produces:

```
═════════════╤════
"die" ->   1 │ 1/6
"die" ->   2 │ 1/6
"die" ->   3 │ 1/6
"die" ->   4 │ 1/6
"die" ->   5 │ 1/6
"die" ->   6 │ 1/6
```

### Ninth Example

Suppose that we randomly pick a number from a clock face (i.e. an
integer from 1 to 12 inclusive). What can we say about the distance
between these two numbers?

```
hour1, hour2, result2, result : u8[wrap];
hour1 ~ uniform 1 12;
hour2 ~ uniform 1 12;

result := hour1 - hour2 + 12;
if result >= 12 then {
  result := result - 12
}

result2 := hour2 - hour1 + 12;
if result2 >= 12 then {
  result2 := result2 - 12
}

if result > result2 then {
  result := result2;
}
return result;
```

(Note that we don't have min/max functions for now, and we also don't
have modulo operations for now.)

The tool reports:

```
════╤═════
  1 │ 1/6
  2 │ 1/6
  3 │ 1/6
  4 │ 1/6
  5 │ 1/6
  0 │ 1/12
  6 │ 1/12
```

This result might be surprising if one has never pondered this before,
but it actually makes sense. It's equivalent to fixing the first number
at the top, and then randomly choosing the second number. Due to
symmetry, the results from 1 to 5 each appear twice, and 0 or 6 each
appears once.

### Tenth Example

Integers and uniform distributions are very powerful when used inside loops. For
example, suppose we keep rolling a fair die while keeping track of the running
sum, until the sum reaches 10. How many times do we need to roll?

```
count, sum, die : u8;
do {
    count := count + 1;
    die ~ uniform 1 6;
    sum := sum + die;
} while sum < 10;
return count;
```

The tool reports:

```
════╤════════════════════════
  3 │ 11/24
  4 │ 5/18
  2 │ 1/6
  5 │ 35/432 (≈ 0.0810185)
  6 │ 7/486 (≈ 0.0144032)
  7 │ 13/7776 (≈ 0.0016718)
  8 │ 23/186624 (≈ 0.0001232)
  9 │ 53/10077696 (≈ 5.2e-6)
 10 │ 1/10077696 (≈ 0)
 ```

### Eleventh Example

Suppose there are four cars and eight spaces. Initially the cars are located
in the left half of the spaces. At each iteration, each car flips a fair
coin. When the coin is H and if there is a space in front, then the car moves
forward by one space. How many iterations are needed to get all four cars in
the right half?

```
car1, car2, car3, car4: u8;
count: u8;
coin: bool;
moveCar1, moveCar2, moveCar3, moveCar4: bool;

car1 := 1;
car2 := 2;
car3 := 3;
car4 := 4;

do {
    coin ~ bernoulli 0.5;
    moveCar4 := car4 < 8 and coin;
    coin ~ bernoulli 0.5;
    moveCar3 := car4 != car3 + 1 and coin;
    coin ~ bernoulli 0.5;
    moveCar2 := car3 != car2 + 1 and coin;
    coin ~ bernoulli 0.5;
    moveCar1 := car2 != car1 + 1 and coin;

    count := count + 1;
    if moveCar4 then { car4 := car4 + 1 }
    if moveCar3 then { car3 := car3 + 1 }
    if moveCar2 then { car2 := car2 + 1 }
    if moveCar1 then { car1 := car1 + 1 }
} while not (car4 == 8 and car3 == 7 and car2 == 6 and car1 == 5) and count <= 25;
return count;
```

(We added a limit of 25 iterations.)

The tool reports:

```
════╤═════════════════════════════════════════════════════════════════════
 17 │ 7566194099838961/72057594037927936 (≈ 0.105002)
 16 │ 471428131602417/4503599627370496 (≈ 0.104678)
 18 │ 113554521621524465/1152921504606846976 (≈ 0.0984928)
 15 │ 27088835284977/281474976710656 (≈ 0.0962388)
 19 │ 1611003264434503665/18446744073709551616 (≈ 0.0873326)
 14 │ 1409710933745/17592186044416 (≈ 0.0801327)
 20 │ 21790833528900747249/295147905179352825856 (≈ 0.0738302)
 21 │ 282956818818947481585/4722366482869645213696 (≈ 0.0599184)
 13 │ 64852508145/1099511627776 (≈ 0.058983)
 22 │ 3546952479605470986225/75557863725914323419136 (≈ 0.0469435)
 26 │ 12825215918245664398508031/309485009821345068724781056 (≈ 0.0414405)
 12 │ 2551304433/68719476736 (≈ 0.0371263)
 23 │ 43118393808213261680625/1208925819614629174706176 (≈ 0.0356666)
 24 │ 510248916285034478960625/19342813113834066795298816 (≈ 0.0263792)
 11 │ 81872113/4294967296 (≈ 0.0190623)
 25 │ 5896338143010985124823025/309485009821345068724781056 (≈ 0.019052)
 10 │ 1997409/268435456 (≈ 0.0074409)
  9 │ 33105/16777216 (≈ 0.0019732)
  8 │ 305/1048576 (≈ 0.0002908)
  7 │ 1/65536 (≈ 1.52e-5)
```

### Twelfth Example

So far we've only played with one aspect of the probabilistic nature of
the PPL, drawing from a distribution. The second aspect is observation.

Observation allows part of the program to assert that a certain outcome
has been reached, therefore pruning some of the probabilities.

Now suppose we roll two such dice, and the sum of the numbers is ten.
What could the two rolls be? This program can find out:

```
die1, die2, total: u8;
die1 ~ uniform 1 6;
die2 ~ uniform 1 6;
total := die1 + die2;
observe total == 10;
```

The inference tool reports:

```
═══════════════════════════════════════════╤════
"die1" ->   4 "die2" ->   6 "total" ->  10 │ 1/3
"die1" ->   5 "die2" ->   5 "total" ->  10 │ 1/3
"die1" ->   6 "die2" ->   4 "total" ->  10 │ 1/3
```

The result says, there are three possibilities with equal probability: first
roll 4, second roll 6; both rolls 5; first roll 6, second roll 4.

### Thirteenth Example

Using `observe` inside loops warrants a special example. An observation is
always global. A failed observation is as if this execution has never happened.
The difference matters for loops whose iteration count is not fixed. Indeed,
observes inside loops essentially penalizes longer traces by a factor of
acceptance probability raised to the power of iteration count.

Consider the following two programs:

```
// Program A
sum, count, u: u8;
while sum < 10 do {
  u ~ uniform 1 2;
  sum := sum + u;
  count := count + 1;
}
return count;
```

and

```
// Program B
sum, count, u: u8;
while sum < 10 do {
  u ~ uniform 1 3;
  observe u != 3;
  sum := sum + u;
  count := count + 1;
}
return count;
```

It would appear that these two programs do the same thing. But that's not the
case. For Program A without the observe the tool reports:

```
════╤═════════════════════
  7 │ 55/128 (≈ 0.4296875)
  6 │ 5/16
  8 │ 49/256 (≈ 0.1914062)
  9 │ 17/512 (≈ 0.0332031)
  5 │ 1/32
 10 │ 1/512 (≈ 0.0019531)
```

Notice how all the denominators are powers of two. Indeed reaching a count of 10
requires 10 uniform draws every single one of which is 1. But for Program B the
tool gives different results:

```
════╤════════════════════════
  6 │ 810/1921 (≈ 0.4216553)
  7 │ 1485/3842 (≈ 0.3865174)
  8 │ 441/3842 (≈ 0.1147839)
  5 │ 243/3842 (≈ 0.0632483)
  9 │ 3/226 (≈ 0.0132743)
 10 │ 1/1921 (≈ 0.0005205)
```

### Fourteenth Example

The product of two fair dice, which is a good deal lumpier than their sum.
Where the sum is symmetric and peaks in one place, the product is spread over
the eighteen numbers that factor into 1 to 6 twice, weighted by how many
factorizations each has.

```
d1, d2, product : u8[wrap];

d1 ~ uniform 1 6;
d2 ~ uniform 1 6;
product := d1 * d2;

return product;
```

```
════╤═════
  6 │ 1/9
 12 │ 1/9
  4 │ 1/12
  2 │ 1/18
  3 │ 1/18
  5 │ 1/18
  8 │ 1/18
 10 │ 1/18
 15 │ 1/18
 18 │ 1/18
 20 │ 1/18
 24 │ 1/18
 30 │ 1/18
  1 │ 1/36
  9 │ 1/36
 16 │ 1/36
 25 │ 1/36
 36 │ 1/36
```

6 and 12 tie for the mode with four factorizations each (1x6, 2x3, 3x2, 6x1
and 2x6, 3x4, 4x3, 6x2), while the squares 1, 9, 16, 25 and 36 are reachable
one way only.

## Language Guide

The language has a fairly simple syntax, reminiscent of typical
imperative languages.

### Lexical Syntax

Comments either start with `//` and last till the end of the line, or
start with `/*` and end with `*/`.

The keywords of the language are `if`, `then`, `else`, `while`, `do`,
`skip`, `true`, `false`, `not`, `and`, `or`, `xor`, `bernoulli`,
`return`, `observe`, `as`, `uniform`, `bool`, and `u8`. They may not be
used as identifiers.

Special symbols are `(`, `)`, `{`, `}`, `[`, `]`, `;`, `,`, `:`, `:=`,
`~`, `!`, `&&`, `^`, `||`, `+`, `-`, `*`, `==`, `!=`, `<`, `<=`, `>`,
and `>=`.

Identifiers start with a letter, followed by zero of more alphanumeric
characters.

Integer literals are written in decimal and must be in the range 0 to
255.

### Expressions Syntax

Expressions in the language look like expressions in typical languages.
Expressions may be wrapped in parentheses. The terms of an expression are
the boolean literals `true` and `false`, an integer literal, or an
identifier.

The operators, from tightest to loosest:

*   A postfix cast, `e as bool`, `e as u8`, or `e as u8[wrap]` and
    likewise for the other two overflow behaviors. Casts may be chained.
*   `*`, left-associative.
*   `+` and `-`, left-associative.
*   The comparisons `==`, `!=`, `<`, `<=`, `>`, `>=`, which do not
    associate (`a < b < c` is a syntax error).
*   `not`, also spelled `!`.
*   `and`, also spelled `&&`, left-associative.
*   `xor`, also spelled `^`, left-associative.
*   `or`, also spelled `||`, left-associative.

Note that `not` binds *looser* than comparison, as it does in Python, so
`not a == b` means `not (a == b)`, and `a + b < c` means `(a + b) < c`.
`*` binds tighter than `+` and `-` as usual, so `a + b * c` is
`a + (b * c)`.

### Statements Syntax

A program consists of an optional declaration preamble, then a statement,
optionally followed by a `return` construct (consisting of the keyword `return`,
followed by one or more expressions, followed by a semicolon).

A statement is a collection of the following more specific kinds of statements,
separated by semicolons. A statement that ends in a closing brace does not need
a semicolon before the statement that follows it. A semicolon at the very end is
optional either way.

*   An *if*-statement, which consists of the keyword `if`, an
    expression, the keyword `then`, a statement enclosed in mandatory
    braces, the keyword `else`, followed by either another
    *if*-statement or a statement enclosed in mandatory braces.

*   A *while*-statement, which consists of the keyword `while`, an
    expression, the keyword `do`, followed by a statement enclosed in
    mandatory braces.

*   A *do*-*while*-statement, which consists of the keyword `do`, a
    statement enclosed in mandatory braces, the keyword `while`,
    followed by an expression.

*   A deterministic assignment statement, which consists of an
    identifier, the symbol `:=`, followed by an expression.

*   A non-deterministic assignment statement, which consists of an
    identifier, the symbol `~`, and a distribution. The distribution is
    either the keyword `bernoulli` followed by an arbitrary precision
    floating number without sign, or the keyword `uniform` optionally
    followed by two integer literals giving an inclusive lower and upper
    bound. A `bernoulli` may only be sampled into a `bool` and a
    `uniform` only into a `u8`; a `uniform` with no bounds covers the
    whole range of the type. Both are checked at parse time: the
    bernoulli parameter must lie in [0, 1], and the lower bound must not
    exceed the upper.

*   A *skip*-statement, which solely consists of the keyword `skip`.

*   An *observe*-statement, which consists of the keyword `observe`,
    followed by an expression.

*   Any statement enclosed in braces.

### Integers

A program may begin with a *declaration preamble*, a run of declarations
of the form

```
i, j : u8[wrap];
k : u8;
flag : bool;
```

Each declaration names one or more variables, a type (`bool` or `u8`),
the latter an 8-bit unsigned integer holding 0 to 255 and, for a `u8`,
an optional overflow behavior in brackets.

The preamble is optional, and its presence changes one thing: *if there
is a preamble, every variable the program uses must be declared*, and
using an undeclared one is an error. A program with no preamble is
treated as if every variable used is implicitly declared with type
`bool`. A variable that is declared but never used is not an error; it
simply appears in the output holding its initial value, which is `false`
for a `bool` and 0 for a `u8`.

#### Overflow

The bracketed annotation says what happens when an arithmetic operation
on that value does not fit in 8 bits. There are three choices, and the
default, when the brackets are omitted, is `saturate`:

*   `wrap`, where the result is taken modulo 256, as on ordinary hardware.

*   `saturate`, where the result is clamped, to 255 when an addition or a
    multiplication overflows and to 0 when a subtraction underflows.

*   `never`, where the overflow is asserted not to happen. It's like signed
    integers in C but better: instead of undefined behavior we get
    *conditioning*, like a failed `observe`: the traces in which the
    overflow occurred are removed and the remaining probabilities are
    renormalized. What the tool then reports is the distribution
    *conditional on no overflow ever having occurred*.

Naturally, choosing different overflow semantics will result in different
probabilities reported in programs that do experience overflow.

*`u8[wrap]`, `u8[saturate]` and `u8[never]` are three distinct types.*
The `+`, `-` and `*` operators require both operands to have exactly the
same type. With `a : u8[wrap]` and `b : u8[saturate]`, the expression
`a + b` is an error, and a cast on either operand says which was meant:

```
a as u8[saturate] + b    // saturating addition
a + b as u8[wrap]        // wrapping addition
a as u8[wrap] * b        // wrapping multiplication
```

Multiplication overflows far more readily than addition does: any two
operands above 15 already leave 0..255, so which behavior applies is
worth more thought than it is for `+`. `16 * 16` is 0 wrapping, 255
saturating, and a rejected trace under `never`.

There is no restriction on where arithmetic may appear. Even though the
`never` behavior is implicitly an `observe` which is statement, such
arithmetic can still remain anywhere, including in another `observe`
statement.

##### Casts are bit reinterpretations

A `u8`-to-`u8` cast is a pure retagging: it names the rule the *next*
operation will run under, and touches none of the eight bits. It follows
that a cast cannot undo what has already happened:

```
a : u8[wrap];
a := 200;

(a + a) as u8[saturate]    // 400 already wrapped to 144
```

To get 255 the cast has to be applied to the operands, before the
addition, not to its result.

##### Literals and constant arithmetic

An integer literal has no overflow behavior of its own. Internally its
type is the hidden `u8[_]`, of which the three real types are
subtypes, so it takes the behavior of whatever it is used with. That is
why `i := i + 1` needs no cast whatever `i` was declared as.

When *both* operands are constants there is nothing to take a behavior
from, and the operation is simply evaluated at compile time: `5 + 5` is
the literal `10`. A result outside 0 to 255 is then an error:

```
x : u8[wrap];
x := 200 + 200;    // error: constant arithmetic goes outside u8
x := 20 * 20;      // error: likewise, 400 does not fit
```

A bare `as u8`, like a bare `u8` in a declaration, means `u8[never]`.

##### Consequences of `never`

`never` conditions the program rather than producing a value, so unlike
the other two it needs a statement of its own. Two consequences:

*   **No short-circuiting.** Both operands of `and` and `or` are always
    evaluated, so a `never` overflow in either one rejects the trace
    regardless of the other's value; likewise an overflow anywhere in an
    `if` or `while` condition rejects it whichever branch would have been
    taken.

*   **A `while` guard conditions once per evaluation.** The guard is
    evaluated on entry and after every iteration, and the conditioning is
    emitted at each of those points, so the rejections compound across
    iterations. This matters when the loop iteration count is not fixed.

`wrap` and `saturate` need no statement, so a program that avoids `never`
keeps its guards pure expressions exactly as before.

#### Comparisons and casts

The six comparisons produce a `bool`. On u8s they **ignore the overflow
behavior**: any two u8s may be compared, since a comparison performs no
arithmetic and so has no overflow to rule on.

`==` and `!=` also apply to two bools, where they are the one-bit case
of the same operation: `p == q` is `not (p xor q)`. The four ordered
comparisons are u8-only; `p < q` on bools is rejected. A bool may not be
compared against a u8; convert first, as in `b as u8 == 1`.

A cast `e as bool` on a u8 is true exactly when the value is nonzero; `e
as u8` on a bool is 1 or 0. Assignment does not perform converts between
`bool` and `u8`: `x := b` with `x : u8` and `b : bool` is an error and
must be written `x := b as u8`.

#### Uniform distributions

`x ~ uniform` gives every value from 0 to 255 probability 1/256; `x ~
uniform lo hi` is flat over `lo` to `hi` inclusive. Both are exact.
Uniform distributions are *not* implemented via `observe`. This matters
for uniform distributions inside loops. (The `observe` rejection is
global, not per iteration.)

### Performance

The cost of inference is proportional to the number of reachable states
at each loop head, restricted to the variables that loop mentions. A
single `u8` costs one state, not 256.

What that means in practice:

*   **Wrapping arithmetic inside a loop is expensive.** Wrapping closes
    a counter's states into a single recurrent component, and the solver
    must do a dense solve over all of it. One such `u8` is a 256-state
    solve taking a few seconds; two in the same loop would be in the
    worst case a 65536-state solve and is probably not feasible.
    `saturate` and `never` leave the dynamics absorbing or acyclic
    instead, and are hundreds of times faster.

*   **Draw uniforms outside a loop when the program allows it.** A `u8`
    sampled inside a loop body puts up to eight random bits into that
    loop's footprint, multiplying the reachable states by up to 256.

*   **Use `return` at the end if possible.** In `return` mode the tool
    can optimize the program by dropping anything that cannot affect the
    returned value, including any temporary values introduced internally.
