# New Result

Create a new
[Result](https://ncc-cnc.github.io/wheretowork/reference/Result.md)
object. This object is used to store information used to generate a
[Solution](https://ncc-cnc.github.io/wheretowork/reference/Solution.md)
object.

## Usage

``` r
new_result(
  values,
  area,
  perimeter,
  theme_coverage,
  weight_coverage,
  include_coverage,
  exclude_coverage,
  theme_settings,
  weight_settings,
  include_settings,
  exclude_settings,
  parameters,
  id = uuid::UUIDgenerate()
)
```

## Arguments

- values:

  `numeric` status of planning units.

- area:

  `numeric` total area (m^2) of selected planning units.

- perimeter:

  `numeric` total perimeter (m) of selected planning units. Or
  `NA_real_` (see Details for more information).

- theme_coverage:

  `numeric` vector containing the proportion of each feature within each
  theme that is covered by the result.

- weight_coverage:

  `numeric` vector containing the proportion of each weight that is
  covered by the result.

- include_coverage:

  `numeric` vector containing the proportion of each include that is
  covered by the result.

- exclude_coverage:

  `numeric` vector containing the proportion of each exclude that is
  covered by the result.

- theme_settings:

  `data.frame` containing the theme settings.

- weight_settings:

  `data.frame` containing the weight settings.

- include_settings:

  `data.frame` containing the include settings.

- exclude_settings:

  `data.frame` containing the exclude settings.

- parameters:

  `list` of
  [Parameter](https://ncc-cnc.github.io/wheretowork/reference/Parameter.md)
  objects.

- id:

  `character` identifier value.

## Value

A [Result](https://ncc-cnc.github.io/wheretowork/reference/Result.md)
object.

## Details

If the boundary matrix was skipped (see `new_dataset_from_auto`), The
`min_set_result` function returns a total_perimeter of `NA_real_`.

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
v3 <- new_variable_from_auto(dataset = d, index = 3)
v4 <- new_variable_from_auto(dataset = d, index = 4)
v5 <- new_variable_from_auto(dataset = d, index = 5)
v6 <- new_variable_from_auto(dataset = d, index = 6)

# create a weight using a variable
w <- new_weight(
  name = "Human Footprint Index", variable = v1,
  factor = -90, status = FALSE, id = "W1"
)

# create features using variables
f1 <- new_feature(
  name = "Possum", variable = v2,
  goal = 0.2, status = FALSE, current = 0.5, id = "F1"
)
f2 <- new_feature(
  name = "Forests", variable = v3,
  goal = 0.3, status = FALSE, current = 0.9, id = "F2"
)
f3 <- new_feature(
  name = "Shrubs", variable = v4,
  goal = 0.6, status = TRUE, current = 0.4, id = "F3"
)

# create themes using the features
t1 <- new_theme("Species", f1, id = "T1")
t2 <- new_theme("Ecoregions", list(f2, f3), id = "T2")

# create an include using a variable
incl <- new_include(
  name = "Protected areas", variable = v5,
  status = FALSE, id = "I1"
)

# create an exclude using a variable
encl <- new_exclude(
  name = "Urban areas", variable = v6,
  status = FALSE, id = "E1"
)

# create parameters
p1 <- new_parameter(name = "Spatial clustering")
p2 <- new_parameter(name = "Optimality gap")

# create solution settings using the themes and weight
ss <- new_solution_settings(
  themes = list(t1, t2), weights = list(w), includes = list(incl),
  excludes = list(encl), parameters = list(p1, p2)
)

# create solution values
values <- sample(
  c(0, 1), length(d$get_planning_unit_indices()), replace = TRUE
)

# create object
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

# print object
print(r)
#> Result object
```
