# Map manager

Constructs a widget for managing items on a Leaflet map. This widget is
designed to be used in conjunction with an existing Leaflet Map within a
Shiny web application.

## Usage

``` r
mapManager(x, width = NULL, height = NULL, elementId = NULL)
```

## Arguments

- x:

  [MapManager](https://ncc-cnc.github.io/wheretowork/reference/MapManager.md)
  object.

- width:

  `character` width of the displayed widget. Defaults to `NULL` such
  that the widget is automatically sized.

- height:

  `character` width of the displayed widget. Defaults to `NULL` such
  that the widget is automatically sized.

- elementId:

  `character` HTML identifier for the widget. Defaults to `NULL`.

## Server value

The widget sends a `list` with the following values to the server:

- id:

  `character` identifier for layer to update.

- setting:

  `character` name of the updated setting

- value:

  `numeric` or `logical` values.

## Examples

``` r
if (FALSE) { # \dontrun{
# run Shiny app to demo the sidebar pane
if (interactive()) {
  runExample("mapManager")
}
} # }
```
