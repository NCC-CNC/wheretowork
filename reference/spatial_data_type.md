# Spatial data type

Identify if a spatial dataset has continuous or categorical data.

## Usage

``` r
spatial_data_type(x, index = 1, ...)

# S3 method for class 'sf'
spatial_data_type(x, index = 1, ...)

# S3 method for class 'SpatRaster'
spatial_data_type(x, index = 1, max_sample = 10000, ...)

# S3 method for class 'data.frame'
spatial_data_type(x, index = 1, ...)
```

## Arguments

- x:

  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html) or
  combined
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or [data.frame](https://rdrr.io/r/base/data.frame.html) dataset
  object.

- index:

  `integer` or `character` value indicating the field or layer for which
  to calculate statistics. Defaults to 1, such that the first
  field/layer is used to calculate statistics.

- ...:

  not used.

- max_sample:

  `integer` maximum number of cells in a raster stack to sample when
  checking if the data are continuous or categorical. Defaults to 10000.

## Value

A `character` indicating if the data are `"continuous"` or
`"categorical"`.
