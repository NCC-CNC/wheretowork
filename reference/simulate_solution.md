# Simulate a new solution

This function simulates a
[Solution](https://ncc-cnc.github.io/wheretowork/reference/Solution.md)
object.

## Usage

``` r
simulate_solution(
  dataset,
  themes,
  weights,
  includes = list(),
  excludes = list()
)
```

## Arguments

- dataset:

  [Dataset](https://ncc-cnc.github.io/wheretowork/reference/Dataset.md)
  object.

- themes:

  `list` of
  [Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md)
  objects.

- weights:

  `list` of
  [Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md)
  objects.

- includes:

  `list` of
  [Include](https://ncc-cnc.github.io/wheretowork/reference/Include.md)
  objects. Defaults to an empty list such that the solution is not
  simulated based on any
  [Include](https://ncc-cnc.github.io/wheretowork/reference/Include.md)
  objects.

- excludes:

  `list` of
  [Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md)
  objects. Defaults to an empty list such that the solution is not
  simulated based on any
  [Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md)
  objects.

## Value

A
[Solution](https://ncc-cnc.github.io/wheretowork/reference/Solution.md)
object.
