# New feature results

Create a new
[FeatureResults](https://ncc-cnc.github.io/wheretowork/reference/FeatureResults.md)
object to store results for a solution.

## Usage

``` r
new_feature_results(feature, held, id = uuid::UUIDgenerate())
```

## Arguments

- feature:

  [Feature](https://ncc-cnc.github.io/wheretowork/reference/Feature.md)
  object.

- held:

  `numeric` proportion of the feature held by the solution. (e.g. 0.1 =
  10%).

- id:

  `character` unique identifier. Defaults to a random identifier
  ([`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html)).

## Value

A
[FeatureResults](https://ncc-cnc.github.io/wheretowork/reference/FeatureResults.md)
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

# create new feature
f <- new_feature(name = "Intact Alvar", variable = v)

# create a feature results object to store results for the feature
fr <- new_feature_results(f, held = 0.8)

# print object
print(fr)
#> FeatureResults
#>   id:      0c4de60d-19d6-4e94-9287-a2122cdaa744
#>   status: TRUE
#>   current: 0
#>   goal: 0.2
#>   held: 0.8
#>   feature: Intact Alvar [status: TRUE, current: 0, goal: 0.2]
#>     variable: .../sim_raster_spatial.tif#Brown_necked_Parrot_Brown_necked_ [total: 483 ]
```
