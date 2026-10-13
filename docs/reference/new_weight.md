# New weight

Create a new
[Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md)
object.

## Usage

``` r
new_weight(
  name,
  variable,
  visible = TRUE,
  invisible = NA_real_,
  loaded = TRUE,
  hidden = FALSE,
  downloadable = TRUE,
  status = TRUE,
  current = 0,
  factor = 0,
  id = uuid::UUIDgenerate(),
  pane = paste(uuid::UUIDgenerate(), variable$index, sep = "-")
)
```

## Arguments

- name:

  `character` Name to display.

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

- factor:

  `numeric` initial factor value. Defaults to 0.

- id:

  `character` unique identifier. Defaults to a random identifier
  ([`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html)).

- pane:

  `character` unique map pane identifier. Defaults to a random
  identifier
  ([`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html))
  concatenated with layer index.

## Value

A [Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md)
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

# print object
print(w)
#> Weight
#>   id:       9a6a0374-5428-4a66-8362-b9f831e52108
#>   name:     NDVI
#>   variable: .../sim_raster_spatial.tif#Brown_necked_Parrot_Brown_necked_ [total: 483 ]
#>   pane:  394520de-f48d-46ca-8828-8d90d9196183-Brown_necked_Parrot_Brown_necked_
#>   current:  0
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   TRUE
#>   factor:   0
```
