# simple_constraints
To archive my old code of a naive and simple constraint solver, with test problems on cryptarithmetic and sudoku.

This code was written years ago mainly for fun, and as coding exercise, so does not necessarily represent good coding styles or best practices.
It should be noted that this is not intended for serious use, but for fun only.

Having written a simple sudoku solver using DFS back at that time, and a simple regex puzzle solver, it occurred to me that it would be fun to try writing a simple constraint propagation using DFS which would be capable of solving at least some simple puzzles.

A constraint problem consists of a number of nodes, corresponding to the unknowns of the problem, and a number of constraints relating the nodes.
The focus is on making the definition of problem as uncluttered as possible.
