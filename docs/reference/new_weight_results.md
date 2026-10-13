# New weight results

Create a new
[WeightResults](https://ncc-cnc.github.io/wheretowork/reference/WeightResults.md)
object to store results for a solution.

## Usage

``` r
new_weight_results(weight, held, id = uuid::UUIDgenerate())
```

## Arguments

- weight:

  [Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md)
  object.

- held:

  `numeric` proportion of the weight covered by the solution. (e.g. 0.1
  = 10%).

- id:

  `character` unique identifier. Defaults to a random identifier
  ([`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html)).

## Value

A
[WeightResults](https://ncc-cnc.github.io/wheretowork/reference/WeightResults.md)
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

# create a new weight
w <- new_weight(name = "NDVI", variable = v)

# create a new weight results object to store results
wr <- new_weight_results(w, 80)

# print object
print(w)
#> Weight
#>   id:       8f397c2b-ea29-47a6-8568-32af818c3c23
#>   name:     NDVI
#>   variable: .../sim_raster_spatial.tif#Brown_necked_Parrot_Brown_necked_ [total: 483 ]
#>   pane:  3e989f7b-570e-434f-9fa8-18e1c5e8b197-Brown_necked_Parrot_Brown_necked_
#>   current:  0
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   TRUE
#>   factor:   0
```
