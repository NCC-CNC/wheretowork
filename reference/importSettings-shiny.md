# Shiny bindings for `importSettings`

Use `importSettingsOutput()` to create a user interface element, and
`renderImportSettings()` to render the widget.

## Usage

``` r
importSettingsOutput(outputId, width = "100%", height = "auto")

renderImportSettings(expr, env = parent.frame(), quoted = FALSE)
```

## Arguments

- outputId:

  output variable to read from

- width, height:

  Must be a valid CSS unit (like `"100%"`, `"400px"`, `"auto"`) or a
  number, which will be coerced to a string and have `"px"` appended.

- expr:

  An expression that generates a
  [`importSettings()`](https://ncc-cnc.github.io/wheretowork/reference/importSettings-widget.md)

- env:

  The environment in which to evaluate `expr`.

- quoted:

  Is `expr` a quoted expression (with
  [`quote()`](https://rdrr.io/r/base/substitute.html))? This is useful
  if you want to save an expression in a variable.
