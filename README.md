# A Simple Probabilistic Program Inference Tool Supporting Loops

## Background

Probabilistic programming languages resemble typical programming
languages, but instead of being compiled and run, their goal is to
enable the use of the language as a medium to express a probabilistic
model and to enable automatic inference. This project provides an
inference tool for a rather simple kind of imperative probabilistic
programming language.

The probabilistic programming language (hereafter referred to as PPL)
described in this repository supports booleans as the only data
structure available, but allows a wide variety of control flow
constructs, including conditionals, *loops*, including those loops whose
number of iterations isn't fixed. The PPL adds two fundamental language
constructs to allow probabilistic programming:

*   Bernoulli trial
*   Observation of an expression

Their meaning will become clear in the following examples.

## Running the Tool

If you wish to compile from source, have [the Haskell `stack`
tool](http://haskellstack.org) ready. It is a program for developing
Haskell projects, with automated compiler installation and dependency
installation. Then, run `stack build` in the root directory of the repo.

If you do not wish to compile, download a pre-compiled executable from
the [GitHub release
page](https://github.com/kccqzy/probabilistic-program-inference/releases/tag/v1.0).

Finally, run

```
stack exec -- prob /path/to/program.txt
```

to perform inference on a probabilistic program contained in
the mentioned file.

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
------------- ---
"r" ->  true  1/2
"r" -> false  1/2
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
---------------------------- ---
"r1" ->  true "r2" ->  true  1/4
"r1" -> false "r2" ->  true  1/4
"r1" ->  true "r2" -> false  1/4
"r1" -> false "r2" -> false  1/4
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
------------------------------------------ ---
"r1" ->  true "r2" ->  true "r3" ->  true  1/8
"r1" ->  true "r2" ->  true "r3" -> false  1/8
"r1" -> false "r2" ->  true "r3" -> false  1/4
"r1" ->  true "r2" -> false "r3" -> false  1/4
"r1" -> false "r2" -> false "r3" -> false  1/4
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
numberOfTimesOdd := false; // the last bit of a conceptual integer

do {
   coin ~ bernoulli 0.5;
   numberOfTimesOdd := not numberOfTimesOdd;
} while coin;
```

Running this through the tool gives this result:

```
-------------------------------------------- ---
"coin" -> false "numberOfTimesOdd" ->  true  2/3
"coin" -> false "numberOfTimesOdd" -> false  1/3
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
conceptually integral variable. Alas, let's implement additions
manually:

```
timesBit0 := false;
timesBit1 := false;
timesBit2 := false;
overflow := false;

do {
  coin ~ bernoulli 0.5;
  
  // Manually perform addition by one.
  if not timesBit0 then {
    timesBit0 := true;
  } else if not timesBit1 then {
      timesBit1 := true;
      timesBit0 := false;
  } else if not timesBit2 then {
      timesBit2 := true;
      timesBit1 := false;
      timesBit0 := false;
  } else {
      overflow := true;
  }
  
} while coin;
```

The code is not elegant, but it works. The tool now makes it clear we
are dealing with a geometric distribution:

```
--------------------------------------------------------------------------------------------------- -----
"coin" -> false "overflow" ->  true "timesBit0" ->  true "timesBit1" ->  true "timesBit2" ->  true  1/128
"coin" -> false "overflow" -> false "timesBit0" ->  true "timesBit1" ->  true "timesBit2" ->  true  1/128
"coin" -> false "overflow" -> false "timesBit0" -> false "timesBit1" ->  true "timesBit2" ->  true  1/64
"coin" -> false "overflow" -> false "timesBit0" ->  true "timesBit1" -> false "timesBit2" ->  true  1/32
"coin" -> false "overflow" -> false "timesBit0" -> false "timesBit1" -> false "timesBit2" ->  true  1/16
"coin" -> false "overflow" -> false "timesBit0" ->  true "timesBit1" ->  true "timesBit2" -> false  1/8
"coin" -> false "overflow" -> false "timesBit0" -> false "timesBit1" ->  true "timesBit2" -> false  1/4
"coin" -> false "overflow" -> false "timesBit0" ->  true "timesBit1" -> false "timesBit2" -> false  1/2
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

When run through the inference tool, the tool refuses to produce an
answer:

```
No results produced.
```

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
-------------------------- ---
"x" -> false "y" ->  true  1/4
"x" -> false "y" -> false  3/4
```

### Eighth Example

So far we've only played with one aspect of the probabilistic nature of
the PPL, the Bernoulli trial. The second aspect is observation.

Observation allows part of the program to assert that a certain outcome
has been reached, therefore pruning some of the probabilities.

Consider for example we would like to model the roll of a fair die. That
die has six sides, numbered one to six. We can choose to use three
boolean variables to represent each bit of the number. However, with
three bits we can represent zero and seven as well. We can then use the
`observe` construct to remove these possibilities:

```
b0 ~ bernoulli 0.5;
b1 ~ bernoulli 0.5;
b2 ~ bernoulli 0.5;

observe b0 || b1 || b2; // Not all zeros
observe !b0 || !b1 || !b2; // Not all ones
```

The inference tool reports:

```
------------------------------------------ ---
"b0" -> false "b1" ->  true "b2" ->  true  1/6
"b0" ->  true "b1" -> false "b2" ->  true  1/6
"b0" -> false "b1" -> false "b2" ->  true  1/6
"b0" ->  true "b1" ->  true "b2" -> false  1/6
"b0" -> false "b1" ->  true "b2" -> false  1/6
"b0" ->  true "b1" -> false "b2" -> false  1/6
```

### Ninth Example

Now suppose we roll two such dice, and the sum of the numbers is ten.
What could the two rolls be? This program can find out:

```
b0 ~ bernoulli 0.5;
b1 ~ bernoulli 0.5;
b2 ~ bernoulli 0.5;

// Not zero or seven.
observe b0 || b1 || b2;
observe !b0 || !b1 || !b2;

c0 ~ bernoulli 0.5;
c1 ~ bernoulli 0.5;
c2 ~ bernoulli 0.5;

// Not zero or seven.
observe c0 || c1 || c2;
observe !c0 || !c1 || !c2;

// Now b and c are results of the two rolls. We sum them up.

s0 := b0 xor c0;
carry := b0 and c0;

s1 := b1 xor c1 xor carry;
carry := b1 and c1 or carry and (b1 xor c1);

s2 := b2 xor c2 xor carry;
carry := b2 and c2 or carry and (b2 xor c2);

s3 := carry;

// We observe their sum to be ten.
observe not s0;
observe s1;
observe not s2;
observe s3;
```

The inference tool reports:

```
------------------------------------------------------------------------------------------------------------------------------------------------------------- ---
"b0" -> false "b1" -> false "b2" ->  true "c0" -> false "c1" ->  true "c2" ->  true "carry" ->  true "s0" -> false "s1" ->  true "s2" -> false "s3" ->  true  1/3
"b0" ->  true "b1" -> false "b2" ->  true "c0" ->  true "c1" -> false "c2" ->  true "carry" ->  true "s0" -> false "s1" ->  true "s2" -> false "s3" ->  true  1/3
"b0" -> false "b1" ->  true "b2" ->  true "c0" -> false "c1" -> false "c2" ->  true "carry" ->  true "s0" -> false "s1" ->  true "s2" -> false "s3" ->  true  1/3
```

The result is difficult to read due to the encoding, but it essentially
says, there are three possibilities with equal probability: first roll
4, second roll 6; both rolls 5; first roll 6, second roll 4.

## Language Guide

The language has a fairly simple syntax, reminiscent of typical
imperative languages.

### Lexical Syntax

Comments either start with `//` and last till the end of the line, or
start with `/*` and end with `*/`.

The keywords of the language are `if`, `then`, `else`, `while`, `do`,
`skip`, `true`, `false`, `not`, `and`, `or`, `xor`, `bernoulli`,
`return`, and `observe`. They may not be used as identifiers.

Special symbols are `(`, `)`, `{`, `}`, `;`, `:=`, `~`, `!`, `&&`, `^`,
and `||`.

Identifiers start with a letter, followed by zero of more alphanumeric
characters.

### Expressions Syntax

Expressions in the language look like expressions in typical languages.
There are four operators `not` (also spelled as `!`), `and` (also
spelled as `&&`), `xor` (also spelled as `^`), `or` (also spelled as
`||`), in that precedence. Expressions may be wrapped in parentheses.
The terms of the expression are the boolean literals `true`, `false`, or
an identifier.

### Statements Syntax

A program consists of a statement, optionally followed by a `return`
construct (consisting of the keyword `return`, followed by an
expression, followed by a semicolon).

A statement is a collection of the following more specific kinds of
statements, each followed by a semicolon:

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
    identifier, the symbol `~`, the keyword `bernoulli`, followed by an
    arbitrary precision floating number without sign. Exponential
    notation is supported. WARNING: if the exponent is too large, the
    tool will use lots of memory and crash. If the number is not between
    0 and 1, the behavior is undefined.
    
*   A *skip*-statement, which solely consists of the keyword `skip`.

*   An *observe*-statement, which consists of the keyword `observe`,
    followed by an expression.
    
*   Any statement enclosed in braces.

### Semantics

The semantics of this programming language is not clearly defined, and
was subject to discussion and debate among the author and his research
partners. Some results have been reached, but none achieved the kind of
simplicity, versatility, and elegance desired by the author.

## Implementation Note

How does this inference tool deal with loops? In simple words, the
entire program state throughout the loop is systematically explored. Due
to the properties of the language, program states will re-occur.
Furthermore, the probabilities of the various possible program states in
a loop are *linear*. This results in a linear system of at most 2^N
variables (in the program state), where *N* is the number of variables
in the program.

