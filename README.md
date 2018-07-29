# pyramid
Minimal interpreter for the WHILE language as described by the book Semantics with Applications: A Formal Introduction.

Wrote this to better understand fundamental Haskell and Language Engineering concepts. Instead of Megaparsec, this project makes use of Dr Nicolas Wu's Yoda library.

# Usage

![gif](https://user-images.githubusercontent.com/6099321/43364508-26a352a8-9313-11e8-9967-85676d31d6b9.gif)

Simply run `ghci pyramid.hs` to start the intrepreter.

To see all possible parses of a script, use `parse <parser> <input>`. The parser should generally be `stmnt`, with the input being a string. The return type is `[(String, Stmnt)]`. The first value in the tuple is the remainder string after parsing, with the second value being the parsed result.

To run a program, use `runScript <program>`.

To see the value of a variable upon a script's termination, run `valueOf <variable> (runScript <program>)`.

# Known issues

* Some extensions to the language, such as `begin` and `proc` are implemented in the parser but can not be compiled yet.
* No side-effect constructors exist. Consider implementing IO.
  ** As a result, for demo purposes, I have implemented `valueOf` as an over-simplifcation.
* Implement reading/writing from files.
* Implement arrays.
* Implement comments.
* `for` is simple to implement now that I have `while`. Implement it.
