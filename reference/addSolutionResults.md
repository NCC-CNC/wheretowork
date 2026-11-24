# Add a solution results

Add a solution to the
[solutionResults](https://ncc-cnc.github.io/wheretowork/reference/solutionResults-widget.md)
widget.

## Usage

``` r
addSolutionResults(session = shiny::getDefaultReactiveDomain(), inputId, value)
```

## Arguments

- session:

  The `session` object passed to function given to `shinyServer` Default
  is
  [`shiny::getDefaultReactiveDomain()`](https://rdrr.io/pkg/shiny/man/domains.html).

- inputId:

  `character` The identifier of the input object.

- value:

  [Solution](https://ncc-cnc.github.io/wheretowork/reference/Solution.md)
  object.
