# Description
An implementation of the lambda calculus in OCaml built for learning purposes.
Running reduce.sh on a file containing a lambda expression will print the
beta-reduced.

# Dependencies
+ [OCaml](https://ocaml.org/)
+ [Dune](https://dune.build/)

# Installation
1. Clone this repository
2. Run `dune build`
3. Running `./reduce.sh example.src` should print `az`

# Syntax
[If you are not familiar with the lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus).
The syntax is as you would expect except for "λ" becomes "\\".
