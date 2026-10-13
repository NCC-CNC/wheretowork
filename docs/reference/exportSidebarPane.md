# Export sidebar pane

Constructs a sidebar pane for exporting data.

## Usage

``` r
exportSidebarPane(id)
```

## Arguments

- id:

  `character` identifier for the sidebar pane.

## Value

A `shiny.tag` object with the sidebar pane.

## Details

This is designed to be used as an argument to
[leaflet.extras2::sidebar_tabs](https://trafficonese.github.io/leaflet.extras2/reference/sidebar_tabs.html)
when specifying the user interface for a Shiny web application.

## Examples

``` r
if (FALSE) { # \dontrun{
# run Shiny app to demo the sidebar pane
if (interactive()) {
  runExample("importModal")
}
} # }
```
