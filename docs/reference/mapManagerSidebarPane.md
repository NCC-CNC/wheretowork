# Map manager sidebar pane

Constructs a sidebar pane for managing the layers on a
[`leaflet::leafletOutput()`](https://rstudio.github.io/leaflet/reference/map-shiny.html)
map.

## Usage

``` r
mapManagerSidebarPane(id, mapManagerId = paste0(id, "_settings"))
```

## Arguments

- id:

  `character` identifier for the sidebar pane.

- mapManagerId:

  `character` identifier for the
  [`mapManager()`](https://ncc-cnc.github.io/wheretowork/reference/mapManager-widget.md)
  widget to create within the sidebar pane. This widget is used to
  control the settings for new solutions. Defaults to
  `paste0(id, "_settings")`.

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
  runExample("mapManagerSidebarPane")
}
} # }
```
