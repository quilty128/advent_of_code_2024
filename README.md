Since I am using this project to learn Haskell, I have not significantly
refactored my earlier solutions—even as I improved my skills by solving later
days. Any significant changes will/would be in a subdirectory of the original,
in a folder like `src/Day7/Optimized` or `src/Day7/Refactored`.

# Requirements
- `cabal` 3.0 or greater

# Usage
- Put each day's input into `input/dayN.txt` file, where 'N' is the `N`-th day.
- Use `cabal run dayN` to solve the `N`-th day.

# Explanations

## Day 7

### Part 1 

We use an `Int` (a signed integer of system wordsize) to represent the current
permutation of operations when testing all possible combinations, where each
zero bit represents `(+)` and each set bit represents `(*)`, then map this to
the corresponding list of operations, which can then be applied to the operands
in the row.

By using an `Int`, we can step through the permutations efficiently using
addition. And we can stop stepping once the (n + 1)-th bit becomes set
(one-indexed). This is done using `countTrailingZeros` from `Data.Bits`.

For example, to step through every permutation of two operations — to be used
for a three operand equation — we want to generate `[(+), (+)], [(+), (*)],
[(*), (+)], [(*), (*)]`, and this corresponds to `0b00, 0b01, 0b10, 0b11`, where
each of these integers is the successor of the previous. The successor of `0b11`
is `0b100`, so we stop stepping at `0b11` and return the list of permutations of
operations. Then we test if the result of applying any of the permutations to
the operands produces the test value to see if an equation is valid. This solves
part 1. 

#### Other Notes

There is no point using a smaller integer even though we probably need only 16
bits (in my input the longest “equation” has 12 operands, so 12 bits are
needed). Every Haskell int type (e.g. `Int16`) still consumes at least the
system’s wordsize. 

Also, it is safe to use `Int` from `Prelude.Int` on systems with a very small
wordsize. The Haskell language standard guarantees that `Int` has at least the
range `[-2^29 .. 2^29-1]`.

### Part 2

Like for part 1, map an integer to a list of operations and increment it to step
through. But since we now have 3 operations, we treat our integer like a ternary
integer.
