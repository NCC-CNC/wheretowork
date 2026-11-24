# New solution from result

Create a new
[Solution](https://ncc-cnc.github.io/wheretowork/reference/Solution.md)
object using a
[Result](https://ncc-cnc.github.io/wheretowork/reference/Result.md)
object

## Usage

``` r
new_solution_from_result(
  name,
  visible,
  invisible = NA_real_,
  loaded = TRUE,
  dataset,
  settings,
  result,
  legend,
  id = uuid::UUIDgenerate(),
  hidden = FALSE,
  downloadable = TRUE,
  pane = NA_character_
)
```

## Arguments

- name:

  `character` name for new solution.

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

- dataset:

  [Dataset](https://ncc-cnc.github.io/wheretowork/reference/Dataset.md)
  object.

- settings:

  [SolutionSettings](https://ncc-cnc.github.io/wheretowork/reference/SolutionSettings.md)
  object.

- result:

  [Result](https://ncc-cnc.github.io/wheretowork/reference/Result.md)
  object.

- legend:

  [ManualLegend](https://ncc-cnc.github.io/wheretowork/reference/ManualLegend.md)
  object.

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

## Value

A
[Solution](https://ncc-cnc.github.io/wheretowork/reference/Solution.md)
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

# create parameters
p1 <- new_parameter(name = "Spatial clustering")
p2 <- new_parameter(name = "Optimality gap")

# create solution settings using the themes and weight
ss <- new_solution_settings(
  themes = list(t1),
  weights = list(),
  includes = list(),
  excludes = list(),
  parameters = list(p1, p2)
)

# create solution values
values <- sample(
  c(0, 1), length(d$get_planning_unit_indices()), replace = TRUE
)

# create result object
r <- new_result(
  values = values,
  area = 12,
  perimeter = 10,
  theme_coverage = calculate_coverage(values, ss$get_theme_data()),
  weight_coverage = calculate_coverage(values, ss$get_weight_data()),
  include_coverage = calculate_coverage(values, ss$get_include_data()),
  exclude_coverage = calculate_coverage(values, ss$get_exclude_data()),
  theme_settings = ss$get_theme_settings(),
  weight_settings = ss$get_weight_settings(),
  include_settings = ss$get_include_settings(),
  exclude_settings = ss$get_exclude_settings(),
  parameters = ss$parameters
)

# create solution using result object
s <- new_solution_from_result(
  name = "solution001",
  visible = TRUE,
  dataset = d,
  settings = ss,
  result = r,
  legend =  new_manual_legend(
    values = c(0, 1),
    colors = c("#00FFFF00", "#112233FF"),
    labels = c("not selected", "selected")
  ),
  hidden = FALSE,
  downloadable = TRUE
)
```
