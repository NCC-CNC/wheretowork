# Write spatial data

Write spatial data to disk.

## Usage

``` r
write_spatial_data(x, dir, name)
```

## Arguments

- x:

  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html) or
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object.

- dir:

  `character` directory to save data.

- name:

  `character` name of the spatial data file.

## Value

Invisible `TRUE` indicating success.

## Examples

``` r
# read and write raster data
f1 <- system.file("external/rlogo.grd", package = "raster")
d1 <- read_spatial_data(f1)
write_spatial_data(d1, tempdir(), "rlogo")

# read and write vector data
f2 <- system.file("shape/nc.shp", package = "sf")
d2 <- read_spatial_data(f2)
write_spatial_data(d2, tempdir(), "nc")
```
