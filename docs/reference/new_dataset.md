# New dataset

Create a new
[Dataset](https://ncc-cnc.github.io/wheretowork/reference/Dataset.md)
object.

## Usage

``` r
new_dataset(
  spatial_path,
  attribute_path,
  boundary_path,
  spatial_data = NULL,
  attribute_data = NULL,
  boundary_data = NULL,
  id = uuid::UUIDgenerate()
)
```

## Arguments

- spatial_path:

  `character` file path for spatial data.

- attribute_path:

  `character` file path for attribute data.

- boundary_path:

  `character` file path for boundary data.

- spatial_data:

  `NULL`,
  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html), or
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object. Defaults to `NULL` such that data are automatically imported
  using the argument to `spatial_path`.

- attribute_data:

  `NULL`, or
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  object. Defaults to `NULL` such that data are automatically imported
  using the argument to `attribute_path`.

- boundary_data:

  `NULL`, or
  [`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  object. Defaults to `NULL` such that data are automatically imported
  using the argument to `boundary_path`.

- id:

  `character` unique identifier. Defaults to a random identifier
  ([`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html)).

## Value

A [Dataset](https://ncc-cnc.github.io/wheretowork/reference/Dataset.md)
object.

## Examples

``` r
# find data file paths
f1 <- system.file(
  "extdata", "projects", "sim_raster", "sim_raster_spatial.tif",
  package = "wheretowork"
)
f2 <- system.file(
  "extdata",  "projects", "sim_raster", "sim_raster_attribute.csv.gz",
  package = "wheretowork"
)
f3 <- system.file(
  "extdata",  "projects", "sim_raster", "sim_raster_boundary.csv.gz",
  package = "wheretowork"
)

# create new dataset
d <- new_dataset(f1, f2, f3)

# print object
print(d)
#> Dataset
#>   paths:
#>     spatial: C:/Users/shaun.astbury/AppData/Local/Temp/RtmpuUTJMm/temp_libpath23344d0a1f4c/wheretowork/extdata/projects/sim_raster/sim_raster_spatial.tif
#>     attribute: C:/Users/shaun.astbury/AppData/Local/Temp/RtmpuUTJMm/temp_libpath23344d0a1f4c/wheretowork/extdata/projects/sim_raster/sim_raster_attribute.csv.gz
#>     boundary: C:/Users/shaun.astbury/AppData/Local/Temp/RtmpuUTJMm/temp_libpath23344d0a1f4c/wheretowork/extdata/projects/sim_raster/sim_raster_boundary.csv.gz
```
