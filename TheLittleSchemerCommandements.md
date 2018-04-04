
# The Ten Commandments

## The First Commandment

When recurring on a list of atoms, `lat`, ask two questions about it: `(null? lat)` and `else`.

When recurring on a number, `n`, ask two questions about it: `(zero? n)` and `else`.

When recurring on a list of S-expressions, `l`, ask three question about it: `(null? l)`, `(atom? (car l))`, and `else`.

## The Second Commandment

Use `cons` to build lists.

## The Third Commandment

When building a list, describe the first typical element, and then cons it onto the natural recursion.

## The Fourth Commandment

Always change at least one argument while recurring.

* When recurring on a list of atoms, `lat`, use `(cdr lat)`.
* When recurring on a number, `n`, use `(sub1 n)`.
* And when recurring on a list of S-expressions, `l`, use `(car l)` and `(cdr l)` if neither `(null? l)` nor `(atom? (car l))` are true.

It must be changed to be closer to termination. The changing argument must be tested in the termination condition:

* When using `cdr`, test termination with `null?`
* When using `sub1`, test termination with `zero?`

