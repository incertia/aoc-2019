# Advent of Code 2019

This holds my AOC 2019 code, with some Haskell-related comments left in this
document. I will also be publishing a set of solutions in C, and one can compare
the difference in code required between the two languages.

## Day 1

### Part A

We just want to run each number through the fuel function ``(+(-2)) . (`div`
3)`` and taking a `sum` gives us the desired answer.

### Part B

We need to take the fuel of the fuel, so we repeatedly apply the fuel function
with `iterate` and only sum the positive values `sum . takeWhile (>0) . tail`.
We need the `tail` because `iterate` also applies its argument `0` times.

## Day 2

We are introduced to [`IntCode`](#intcode), which will probably make more
appearances so we develop the `IntCode` module which holds the implementation of
an `IntCode` machine.

### Part A

We are asked to replace some values of the program tape and run the machine,
reporting the initial value on the tape when it halts. This is fairly easy with
the `IntCode` API with `writeT 1 a >> writeT 2 b >> eval`.

### Part B

We are asked to find some input. We filter the search space `(,) <$> [0..99] <*>
[0..99]` with the result of running the machine and report back the answer.

## Day 3

### Part A

We make some data structures representing a line segment derived from the input
format and run the `intersect` function on all segments, picking out the results
that intersect. We apply the taxicab distance and take the `minimum`.

### Part B

Same as A, but we apply the total segment length distance and take the
`minimum`.

## Day 4

### Part A

The non-decreasing condition can be written as `x == sort x`. `group` turns runs
into a single list. We need a `group` with length at least `2`.

### Part B

Same as A but we need a group with length precisely `2`.

## Day 5

### Part A

Run the `IntCode` machine with the value `1` on its input tape and print the
diagnostic.

### Part B

Run the `IntCode` machine with the value `5` on its input tape and print the
diagnostic.

## Day 6

### Part A

We make a `Tree` out of the inputs, and map the nodes to their depth in the tree
and take the `sum`.

### Part B

We make an undirected graph by using adjacency lists as `HashSet`s. The number
of orbital jumps is just the shortest distance which can be found via normal
`bfs`. `t` represents the next layer of the BFS tree, found by visiting all
neighbors in the current layer `s`. `h` is a reduced version of the search graph
`g`, because we no longer need to visit the neighbors of `s` since they are
already visited.

## Day 7

### Part A

We write `chain` where it returns `head . view tapeOut` and does not have any
mutual recursion.

### Part B

We update `chain` to its current form and laziness takes care of everything. I
managed to come up with this beautiful approach after a night of heavy drinking
with my coworkers...

## Day 8

### Part A

Standard list manipulation stuff, take the minimal layer by the metric and
calculate the result.

### Part B

We now have to migrate to each solver spitting out `String` instead of
`Integer`... We run the results of stacking the layers through a display
function to make it easier for us to read.

## Day 9

### Part A

We switch the mode from `Bool` to `Mode` and modify `decode` accordingly,
returning the correct relative pointers `rptrs`. Take care that when decoding
immediate values you also apply the relative positioning by decoding with
`rptrs` instead of `ptrs`.

### Part B

Run the program with input `2`.

## Day 10

### Part A

Use `gcd` to compute vectors and you count the maximum unique vectors.

### Part B

By taking the cross product you can obtain a measure of "clockwise"-ness. Doing
some more math takes care of setting the up direction to be the smallest and
sorting by length. We kill the `head` of each list per go-around and we take the
200th list element (index `199`) for the solution.

## Day 11

### Part A

Use `scanl` to produce a snapshot of the hull at each step. Count the number of
keys for the number of locations painted.

### Part B

Get the `minimum` and `maximum` `x` and `y` coordinates and draw a picture. We
use some clever `sequence` and `fmap` tricks to avoid writing list
comprehensions.

## Day 14

### Part A

We reverse react `1 FUEL` and get the required amount of `ORE`. `cost` takes a
map of output materials and amounts and looks up the reaction producing that
output. We run just enough reactions to satisfy the output. We add the new input
materials and subtract the unreacted output materials.

### Part B

We do a binary search type thing. We compute the largest `x` such that `2^x
FUEL` can be produced with our supply. We then keep trying to add the largest
power of `2` to that number until we can no longer add any more. This is the
maximum fuel we can produce.

## `IntCode`

`eval` runs the machine by `decode`ing the current instruction and setting the
correct instruction pointer for the next instance of `eval`. `decode` outputs an
infinite list of tape locations and values to more easily support instructions
with many more arguments. We output in reverse order so we do not need to do
list concatenation for tape output. The program and memory tape is stored as a
`HashMap` so out of bound writes can add memory. Everything is wrapped in
`MonadState` because it's nice and we can use the stateful `lens` operators to
operate on the `TapeMachine`.
