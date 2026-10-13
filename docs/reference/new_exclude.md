# New exclude

Create a new
[Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md)
object.

## Usage

``` r
new_exclude(
  name,
  variable,
  mandatory = FALSE,
  visible = TRUE,
  invisible = NA_real_,
  loaded = TRUE,
  hidden = FALSE,
  downloadable = TRUE,
  status = FALSE,
  overlap = NA_character_,
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

- mandatory:

  `logical` value indicating if object is mandatory for generating
  solutions.

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

- overlap:

  `character` vector that remains `NA_character_` until set in the
  [SolutionSettings](https://ncc-cnc.github.io/wheretowork/reference/SolutionSettings.md)
  object. `overlap` defines the include and exclude overlap.

- id:

  `character` unique identifier. Defaults to a random identifier
  ([`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html)).

- pane:

  `character` unique map pane identifier. Defaults to a random
  identifier
  ([`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html))
  concatenated with layer index.

## Value

A [Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md)
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
e <- new_exclude(name = "Protected areas", variable = v)

# print object
print(e)
#> Exclude
#>   id:       be49efc2-c9a3-45d3-a490-1c65635babb7
#>   name:     Protected areas
#>   variable: .../sim_raster_spatial.tif#Brown_necked_Parrot_Brown_necked_ [total: 483 ]
#>   pane:  913a8e80-7b18-46b4-98b6-5f26169dc039-Brown_necked_Parrot_Brown_necked_
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   FALSE
#>   overlap:   NA
```
