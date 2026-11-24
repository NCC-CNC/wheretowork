# Leaflet map

Create a leaflet map with customized default settings.

## Usage

``` r
leaflet_map(sidebar_ids)
```

## Arguments

- sidebar_ids:

  `character` vector containing HTML identifiers for the left and right
  sidebars.

## Value

[`leaflet::leaflet()`](https://rstudio.github.io/leaflet/reference/leaflet.html)
object.

## Details

The "Gray scale" and "Monochrome" basemaps use a CARTO API key, supplied
via the `CARTO_API_KEY` environmental variable (see `./.env.example`).
If it is unset, these basemaps will still load, but will display an "API
key required" watermark.
