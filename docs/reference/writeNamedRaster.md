# Write a Raster object to disk

Save a
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object to disk

## Usage

``` r
writeNamedRaster(x, filename, ...)
```

## Arguments

- x:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object.

- filename:

  `character` file path.

- ...:

  arguments passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html).

## Value

None.

## Details

This file saves a SpatRaster object to disk along.
