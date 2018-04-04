
# The Ten Commandments

## The First Commandment

When recurring on a list of atoms, `lat`, ask two questions about it: `(null? lat)` and `else`.

When recurring on a number, `n`, ask two questions about it: `(zero? n)` and `else`.

When recurring on a list of S-expressions, `l`, ask three question about it: `(null? l)`, `(atom? (car l))`, and `else`.

## The Second Commandment

Use `cons` to build lists.

## The Third Commandment

When building a list, describe the first typical element, and then cons it onto the natural recursion.

