# Haskaml

An OCaml interpreter\* written in Haskell

\* Absolutely not the main focus

Note: Code is very much in active development. Any strange decisions made in evaluating is most likely due to the requirements of CS 4.

## Features

- Simple Parsing
- Operator Precedence
- if / then / else
- Int, Float, Bool
- Let bindings
- Printing with Substitution Model
- Lambda "Shielding" (Substitution Model)

## Todo

- Parsing

    - if ... then ... else ...
    - Recrusive `let` Bindings 
    - Booleans

- Fixing printing for anonymous functions
- Closures
- Recrusive bindings
- Adding environments 
- Functionality for `and` mutual scoping
- Separate `show` and `disp` for substitution model printing

## Background

As I'm taking CS 4 (Functional Programming) at Caltech, I was told that I had to manually write out how S-expressions will be evaluated. Early on in the course, we were even instructed to use a model of substituting in variables that did not reflect how S-expressions are evaluated in reality. Instead of taking 5 minutes to write the evaluations down, I instead spent too much time writing this.

The long term goal is that our program will evaluate an OCaml script and display how it evaluates according to CS 4 guidelines. This means that this interpreter is more of a visualizer of how S-expressions are (incorrectly???) evaluated.

The correct evaluation methods using environments will be implemented when that point in class has been reached, or when I give up.

If you are Professor Vanier, please consider this my SURF application :)

- Eric Lee
