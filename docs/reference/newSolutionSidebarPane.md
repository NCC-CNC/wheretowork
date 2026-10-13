# New solution sidebar pane

Constructs a sidebar pane for generating new solutions.

## Usage

``` r
newSolutionSidebarPane(id, solutionSettingsId = NULL)
```

## Arguments

- id:

  `character` identifier for the sidebar pane.

- solutionSettingsId:

  `character` identifier for the
  [`solutionSettings()`](https://ncc-cnc.github.io/wheretowork/reference/solutionSettings-widget.md)
  widget to create within the sidebar pane. This widget is used to
  control the settings for new solutions. Defaults to `NULL` such that
  the default argument is `paste0(id, "_settings")`.

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
  runExample("newSolutionSidebarPane")
}
} # }
```
