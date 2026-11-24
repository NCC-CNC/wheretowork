# Shiny bindings for `solutionSettings`

Use `solutionSettingsOutput()` to create a user interface element, and
`renderSolutionSettings()` to render the widget.

## Usage

``` r
solutionSettingsOutput(outputId, width = "100%", height = "auto")

renderSolutionSettings(expr, env = parent.frame(), quoted = FALSE)
```

## Arguments

- outputId:

  output variable to read from

- width, height:

  Must be a valid CSS unit (like `"100%"`, `"400px"`, `"auto"`) or a
  number, which will be coerced to a string and have `"px"` appended.

- expr:

  An expression that generates a
  [`solutionSettings()`](https://ncc-cnc.github.io/wheretowork/reference/solutionSettings-widget.md).

- env:

  The environment in which to evaluate `expr`.

- quoted:

  Is `expr` a quoted expression (with
  [`quote()`](https://rdrr.io/r/base/substitute.html))? This is useful
  if you want to save an expression in a variable.
