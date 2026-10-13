# Spatial data statistics

Calculate statistics for a spatial dataset.

## Usage

``` r
spatial_data_statistics(x, type, index = 1)

# S3 method for class 'sf'
spatial_data_statistics(x, type, index = 1)

# S3 method for class 'SpatRaster'
spatial_data_statistics(x, type, index = 1)

# S3 method for class 'data.frame'
spatial_data_statistics(x, type, index = 1)
```

## Arguments

- x:

  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html) or a
  combined
  [terra::rast](https://rspatial.github.io/terra/reference/rast.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) dataset object.

- type:

  `character` indicating whether the dataset contains continuous (i.e.
  `"continuous"`) or categorical (i.e. `"categorical"`) data.

- index:

  `integer` or `character` value indicating the field or layer for which
  to calculate statistics. Defaults to 1, such that the first
  field/layer is used to calculate statistics.

## Value

A `list` containing statistics for the data.
