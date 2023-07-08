# Project Euler in Scala 3

Solutions to the first 54 Project Euler problems in Scala 3

## Usage

1.  Install Scala 3.3
2.  Clone repo
3.  run `sbt` in console
4.  In sbt console `run` will compute all solutions that have been implemented in sequence, starting from problem 1.

- Alternatively pass a sequence of solutions that you want, e.g. `run 3 14 50` will compute the solutions for problems 3, 14, and 50.

All the logic is in `pe.scala`. Some utility functions and data from questions are in `util.scala`, and auxilliary mathematical functions have been placed in `numbertheory.scala`.

## Todo

- [ ] add tests
- [ ] add more solutions (up to Problem 100)
