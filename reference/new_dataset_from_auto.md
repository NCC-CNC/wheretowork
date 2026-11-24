# New dataset from automatic calculations

Create a new
[Dataset](https://ncc-cnc.github.io/wheretowork/reference/Dataset.md)
object by automatically calculating all metadata from the underlying
data. This function is useful when pre-calculated metadata are not
available. Note this function will take longer to create variables than
other functions because it requires performing geospatial operations.

## Usage

``` r
new_dataset_from_auto(x, id = uuid::UUIDgenerate())
```

## Arguments

- x:

  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html) or a
  combined
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object.

- id:

  `character` unique identifier. Defaults to a random identifier
  ([`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html)).

## Value

A [Dataset](https://ncc-cnc.github.io/wheretowork/reference/Dataset.md)
object.

## Details

Create a new
[Dataset](https://ncc-cnc.github.io/wheretowork/reference/Dataset.md)
object.

## Examples

``` r
# find example data
f <- system.file(
  "extdata", "projects", "sim_raster", "sim_raster_spatial.tif",
  package = "wheretowork"
)

# import data
r <- suppressWarnings(terra::rast(f))
r <- c(r, r * 2, r * 3, r * 4)
names(r) <- c("r1", "r2", "r3", "r4")

# create new dataset
d <- new_dataset_from_auto(r)

# print object
print(d)
#> Dataset
#>   paths:
#>     spatial: memory
#>     attribute: memory
#>     boundary: memory
```
