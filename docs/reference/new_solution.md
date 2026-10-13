# New solution

Create a new
[Solution](https://ncc-cnc.github.io/wheretowork/reference/Solution.md)
object.

## Usage

``` r
new_solution(
  name,
  variable,
  visible,
  invisible = NA_real_,
  loaded = TRUE,
  parameters,
  statistics,
  theme_results,
  weight_results,
  include_results,
  exclude_results,
  id = uuid::UUIDgenerate(),
  hidden = FALSE,
  downloadable = TRUE,
  pane = paste(uuid::UUIDgenerate(), variable$index, sep = "-")
)
```

## Arguments

- name:

  `character` name for new solution.

- variable:

  [Variable](https://ncc-cnc.github.io/wheretowork/reference/Variable.md)
  object with the solution.

- visible:

  `logical` should the solution be visible on a map?

- invisible:

  `numeric` date/time. A time stamp date given to when a loaded layer is
  first turned invisible. This is used to keep track of loaded invisible
  layers to offload once the cache threshold has been reached. Defaults
  to `NA_real_`.

- loaded:

  `logical` The initial loaded value. This is used to determine if the
  feature is loaded (or not) or not the map. Defaults to `FALSE`.

- parameters:

  `list` of
  [Parameter](https://ncc-cnc.github.io/wheretowork/reference/Parameter.md)
  objects.

- statistics:

  `list` of
  [Statistic](https://ncc-cnc.github.io/wheretowork/reference/Statistic.md)
  objects.

- theme_results:

  `list` of
  [ThemeResults](https://ncc-cnc.github.io/wheretowork/reference/ThemeResults.md)
  objects.

- weight_results:

  `list` of
  [WeightResults](https://ncc-cnc.github.io/wheretowork/reference/WeightResults.md)
  objects.

- include_results:

  `list` of
  [IncludeResults](https://ncc-cnc.github.io/wheretowork/reference/IncludeResults.md)
  objects.

- exclude_results:

  `list` of
  [ExcludeResults](https://ncc-cnc.github.io/wheretowork/reference/ExcludeResults.md)
  objects.

- id:

  `character` unique identifier. Defaults to a random identifier
  ([`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html)).

- hidden:

  `logical` should the solution be hidden from map?

- downloadable:

  `logical` can the solution be downloaded?

- pane:

  `character` unique map pane identifier. Defaults to a random
  identifier
  ([`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html))
  concatenated with layer index.

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

# create variables
v1 <- new_variable_from_auto(dataset = d, index = 1)
v2 <- new_variable_from_auto(dataset = d, index = 2)

# create features using variables
f1 <- new_feature(
  name = "Possum", variable = v2,
  goal = 0.2, status = FALSE, current = 0.5, id = "F1"
)

# create themes using the features
t1 <- new_theme("Species", f1, id = "T1")

# create a feature results object to store results for the feature
fr1 <- new_feature_results(f1, held = 0.8)

# create a theme results object to store results for the feature
tr1 <- new_theme_results(t1, fr1)

# create parameters
p1 <- new_parameter(name = "Spatial clustering")
p2 <- new_parameter(name = "Optimality gap")

# create a new solution
s <- new_solution(
  name = "solution001",
  variable = v2,
  visible = TRUE,
  parameters = list(p1, p2),
  statistics = list(),
  theme_results = list(tr1),
  weight_results = list(),
  include_results = list(),
  exclude_results = list(),
  id = "solution1",
  hidden = FALSE,
  downloadable = TRUE
)
```
