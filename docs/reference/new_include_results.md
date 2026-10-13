# New include results

Create a new
[IncludeResults](https://ncc-cnc.github.io/wheretowork/reference/IncludeResults.md)
object to store results for a solution.

## Usage

``` r
new_include_results(include, held, id = uuid::UUIDgenerate())
```

## Arguments

- include:

  [Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md)
  object.

- held:

  `numeric` proportion of the include covered by the solution. (e.g. 0.1
  = 10%).

- id:

  `character` unique identifier. Defaults to a random identifier
  ([`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html)).

## Value

A
[IncludeResults](https://ncc-cnc.github.io/wheretowork/reference/IncludeResults.md)
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

# create new variable
v <- new_variable_from_auto(d, index = 1)

# create a new include
i <- new_include(name = "NDVI", variable = v)

# create a new include results object to store results
ir <- new_include_results(i, 80)

# print object
print(ir)
#> IncludeResults
#>   id:      4a5c5fc9-3f97-45fd-a535-8140200c39a8
#>   status: TRUE
#>   held: 80
#>   include: NDVI [status: TRUE]
#>   variable: .../sim_raster_spatial.tif#Brown_necked_Parrot_Brown_necked_ [total: 483 ]
```
