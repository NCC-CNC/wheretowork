# Solution results

Constructs a widget for displaying solution results. This widget is
designed to be used in conjunction with an existing Leaflet Map within a
Shiny web application.

## Usage

``` r
solutionResults(x = list(), width = NULL, height = NULL, elementId = NULL)
```

## Arguments

- x:

  `list` containing
  [Solution](https://ncc-cnc.github.io/wheretowork/reference/Solution.md)
  objects. Defaults to an empty list object.

- width:

  `character` width of the displayed widget. Defaults to `NULL` such
  that the widget is automatically sized.

- height:

  `character` width of the displayed widget. Defaults to `NULL` such
  that the widget is automatically sized.

- elementId:

  `character` HTML identifier for the widget. Defaults to `NULL`.

## Server value

The widget does not send any server values.

## Examples

``` r
if (FALSE) { # \dontrun{
# run Shiny app to demo the sidebar pane
if (interactive()) {
  runExample("solutionResults")
}
} # }
```
