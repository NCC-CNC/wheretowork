# New theme results

Create a new
[ThemeResults](https://ncc-cnc.github.io/wheretowork/reference/ThemeResults.md)
object to store results for a solution.

## Usage

``` r
new_theme_results(theme, feature_results, id = uuid::UUIDgenerate())
```

## Arguments

- theme:

  [Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md)
  object.

- feature_results:

  [FeatureResults](https://ncc-cnc.github.io/wheretowork/reference/FeatureResults.md)
  object or a `list` of such objects.

- id:

  `character` unique identifier. Defaults to a random identifier
  ([`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html)).

## Value

A
[ThemeResults](https://ncc-cnc.github.io/wheretowork/reference/ThemeResults.md)
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

# create a theme using the single feature
th <- new_theme(name = "Intact Alvar", feature = f)

# create a feature results object to store results for the feature
fr <- new_feature_results(f, held = 0.8)

# create a theme results object to store results for the feature
thr <- new_theme_results(th, fr)

# print object
print(thr)
#> ThemeResults
#>   id:      0e6f0378-b795-4de4-ae5a-4fb33e6b615c
```
