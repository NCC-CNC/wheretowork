# New feature

Create a new
[Feature](https://ncc-cnc.github.io/wheretowork/reference/Feature.md)
object.

## Usage

``` r
new_feature(
  name,
  variable,
  visible = TRUE,
  invisible = NA_real_,
  loaded = TRUE,
  hidden = FALSE,
  downloadable = TRUE,
  status = TRUE,
  current = 0,
  goal = 0.2,
  limit_goal = 0,
  id = uuid::UUIDgenerate(),
  pane = paste(uuid::UUIDgenerate(), variable$index, sep = "-")
)
```

## Arguments

- name:

  `character` Name of the feature.

- variable:

  [Variable](https://ncc-cnc.github.io/wheretowork/reference/Variable.md)
  object.

- visible:

  `logical` The initial visible value. This is used to determine if the
  feature is displayed (or not) or not the map. Defaults to `TRUE`.

- invisible:

  `numeric` date/time. A time stamp date given to when a loaded layer is
  first turned invisible. This is used to keep track of loaded invisible
  layers to offload once the cache threshold has been reached. Defaults
  to `NA_real_`.

- loaded:

  `logical` The initial loaded value. This is used to determine if the
  feature has been loaded into the DOM. Defaults to `FALSE`.

- hidden:

  `logical` The hidden value. This is used to determine if the feature
  can ever be displayed. Unlike `visible`, if this parameter is `FALSE`
  then a feature can never be viewed on the map. Defaults to `FALSE`.

- downloadable:

  `logical` The downloadable value. This is used to determine if the
  feature can be download. Set downloadable to `FALSE` for sensitive
  layers that should not be avaiable for download. Defaults to `TRUE`.

- status:

  `logical` The initial status value. This is used to display
  information on whether the feature is selected (or not) for subsequent
  analysis. Defaults to `TRUE`.

- current:

  `numeric` current proportion of values held in existing conservation
  areas (e.g. 0.1 = 10%).

- goal:

  `numeric` The initial goal for the feature. Note that goal values are
  specified as proportions, such that a value of 0.1 corresponds to 10%.
  Defaults to 0.3 (i.e. 30%).

- limit_goal:

  `numeric` The minimum goal (inclusive) that can be selected for the
  feature. Note that goal values are specified as proportions, such that
  a value of 0.1 corresponds to 10%. Defaults to 0 (i.e. 0%).

- id:

  `character` unique identifier. Defaults to a random identifier
  ([`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html)).

- pane:

  `character` unique map pane identifier. Defaults to a random
  identifier
  ([`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html))
  concatenated with layer index.

## Value

A [Feature](https://ncc-cnc.github.io/wheretowork/reference/Feature.md)
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
v <- new_variable_from_auto(dataset = d, index = 1)

# create feature using the variable
f <- new_feature(name = "Intact Alvar", variable = v)

# print object
print(f)
#> Feature
#>   id:       946d0a3b-fbbf-4817-9e2b-a81f8a90c91e
#>   name:     Intact Alvar
#>   variable: .../sim_raster_spatial.tif#Brown_necked_Parrot_Brown_necked_ [total: 483 ]
#>   pane:  0d853416-9735-435a-a79a-0e501788e817-Brown_necked_Parrot_Brown_necked_
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:   FALSE
#>   downloadable:   TRUE
#>   status:   TRUE
#>   current:  0
#>   goal:     0.2
```
