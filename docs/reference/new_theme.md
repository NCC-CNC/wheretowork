# New theme

Create a new
[Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md)
object.

## Usage

``` r
new_theme(
  name,
  feature,
  feature_order = as.double(rev(seq_along(feature))),
  id = uuid::UUIDgenerate()
)
```

## Arguments

- name:

  `character` Name to display.

- feature:

  `list` of
  [Feature](https://ncc-cnc.github.io/wheretowork/reference/Feature.md)
  objects.

- feature_order:

  `numeric` Relative order for displaying each feature on a map.
  Defaults to a reverse sequence of integer values.

- id:

  `character` unique identifier. Defaults to a random identifier
  ([`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html)).

## Value

A [Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md)
object.

## Examples

``` r
# find data path
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
f <- new_feature(name = "Intact Alvar map", variable = v)

# create a theme using the feature
x <- new_theme(name = "Intact Alvar", feature = f)

# print object
print(x)
#> Theme
#>   id:        f613904f-482a-4fed-a900-df774064ecf3
#>   name:      Intact Alvar
#>   feature: 
#>     Intact Alvar map [status: TRUE, current: 0, goal: 0.2]
#>       variable: .../sim_raster_spatial.tif#Brown_necked_Parrot_Brown_necked_ [total: 483 ]
```
