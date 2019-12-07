# Advent of Code 2019

This holds my AOC 2019 code, with some comments left in this document.

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

## `IntCode`

`eval` runs the machine by `decode`ing the current instruction and setting the
correct instruction pointer for the next instance of `eval`. `decode` outputs an
infinite list of tape locations and values to more easily support instructions
with many more arguments. We output in reverse order so we do not need to do
list concatenation for tape output. The program and memory tape is stored as a
`HashMap` so out of bound writes can add memory. Everything is wrapped in
`MonadState` because it's nice and we can use the stateful `lens` operators to
operate on the `TapeMachine`.
