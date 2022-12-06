# Advent of Code 2022 - Haskell

### Index

1. [Building/Running](#Buidling/Running)
2. [Tests](#Tests)
3. [Completed Days](#Completed-Days)

This repositories stores my solutions in Haskell to [Advent of Code 2022](https://adventofcode.com/2022).

For more info on the approach to each day,
read the module header comment located at the top of each day's source file 
(an index is located below)


## Building/Running

This project leverages stack to compile and run the project, to build an executable simply run

```bash
$ stack run 
# Or for running particular days
$ stack run -- --days [INSERT DAY NUMBERS HERE] # ex 1 2 5 10
```

## Tests

To test functionaility of the project, There exists a test suite that can be run by invoking

```bash
$ stack test
# Or for only testing certain days
$ stack test --test-arguments="-d [INSERT DAY NUMBERS HERE]"
```

## Completed Days

Below is an index to every completed day's implementation source code (containing documentation of approach) and the challenge for the day

- [Day 0](src/Day00.hs) : *This is a test day supposed to server a placeholder until the challenge starts*
- [Day 1](src/Day01.hs) : [challenge](https://adventofcode.com/2022/day/1)
- [Day 2](src/Day02.hs) : [challenge](https://adventofcode.com/2022/day/2)
- [Day 3](src/Day03.hs) : [challenge](https://adventofcode.com/2022/day/3)
- [Day 4](src/Day04.hs) : [challenge](https://adventofcode.com/2022/day/4)
- [Day 5](src/Day05.hs) : [challenge](https://adventofcode.com/2022/day/5)
- [Day 6](src/Day06.hs) : [challenge](https://adventofcode.com/2022/day/6)

