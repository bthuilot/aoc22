# Advent of Code 2022 - Haskell

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

## Completed Day Index
- [Day 0](src/Day0.hs)
  - *NOTE*: This is a test day supposed to server a placeholder until the challenge starts

