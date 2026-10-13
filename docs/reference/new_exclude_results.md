# New exclude results

Create a new
[ExcludeResults](https://ncc-cnc.github.io/wheretowork/reference/ExcludeResults.md)
object to store results for a solution.

## Usage

``` r
new_exclude_results(exclude, held, id = uuid::UUIDgenerate())
```

## Arguments

- exclude:

  [Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md)
  object.

- held:

  `numeric` proportion of the exclude covered by the solution. (e.g. 0.1
  = 10%).

- id:

  `character` unique identifier. Defaults to a random identifier
  ([`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html)).

## Value

A
[ExcludeResults](https://ncc-cnc.github.io/wheretowork/reference/ExcludeResults.md)
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

# create a new exclude
excl <- new_exclude(name = "Highways", variable = v)

# create a new exclude results object to store results
exclr <- new_exclude_results(excl, 80)

# print object
print(exclr)
#> ExcludeResults
#>   id:      d414aff9-9f78-4953-9a54-70a95ca2f015
#>   status: FALSE
#>   held: 80
#>   exclude: Highways [status: FALSE]
#>   variable: .../sim_raster_spatial.tif#Brown_necked_Parrot_Brown_necked_ [total: 483 ]
```
