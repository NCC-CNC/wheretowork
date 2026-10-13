# Generate result using minimum shortfall formulation

Create a new
[Result](https://ncc-cnc.github.io/wheretowork/reference/Result.md)
object by generating a prioritization using the minimum shortfall
formulation of the reserve selection problem.

## Usage

``` r
min_shortfall_result(
  area_budget_proportion,
  area_data,
  boundary_data,
  theme_data,
  weight_data,
  include_data,
  exclude_data,
  theme_settings,
  weight_settings,
  include_settings,
  exclude_settings,
  parameters,
  overlap = FALSE,
  gap_1 = 0,
  gap_2 = 0,
  boundary_gap = 0.1,
  cache = cachem::cache_mem(),
  time_limit_1 = .Machine$integer.max,
  time_limit_2 = .Machine$integer.max,
  verbose = FALSE,
  id = uuid::UUIDgenerate(),
  try_gurobi = FALSE
)
```

## Arguments

- area_budget_proportion:

  `numeric` budget for the solution. This should be a proportion (with
  values ranging between 0 and 1) indicating the maximum spatial extent
  of the solution.

- area_data:

  `numeric` vector containing the area of each planning unit.

- boundary_data:

  [`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  containing the boundary data, or or `NA`.

- theme_data:

  [`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  containing the theme data.

- weight_data:

  [`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  containing the weight data.

- include_data:

  [`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  containing the include data.

- exclude_data:

  [`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  containing the exclude data.

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

- overlap:

  `logical` if `TRUE`, excludes takes precedence over includes.

- gap_1:

  `numeric` relative optimality gap value for initial optimization.
  Defaults to 0.

- gap_2:

  `numeric` relative optimality gap value for spatial clustering.
  Defaults to 0.

- boundary_gap:

  `numeric` value used to control the level of spatial clustering in the
  solution. Defaults to 0.1.

- cache:

  [`cachem::cache_mem()`](https://cachem.r-lib.org/reference/cache_mem.html)
  object used to cache intermediate calculations. Defaults to an empty
  cache such that (effectively) no cache is used.

- time_limit_1:

  `numeric` time limit (seconds) for initial optimization run. Defaults
  to the maximum integer.

- time_limit_2:

  `numeric` time limit (seconds) for spatial clustering optimization
  run. Defaults to the maximum integer.

- verbose:

  `logical` value indicating if information should be displayed when
  generating solutions. Defaults to `FALSE`.

- id:

  `character` identifier for new result.

- try_gurobi:

  `logical` value indicating if the Gurobi solver should be used when
  generating solutions. Defaults to `FALSE`.

## Value

A
[Solution](https://ncc-cnc.github.io/wheretowork/reference/Solution.md)
object containing the solution.

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

# create an included using a variable
i <- new_include(
  name = "Protected areas", variable = v5,
  status = FALSE, id = "I1"
)

# create an excluded using a variable
e <- new_exclude(
  name = "Expensive areas", variable = v6,
  status = FALSE, id = "E1"
)

# create parameters
p1 <- new_parameter(name = "Spatial clustering")
p2 <- new_parameter(name = "Optimality gap")

# create solution settings using the themes and weight
ss <- new_solution_settings(
  themes = list(t1, t2), weights = list(w), includes = list(i),
  excludes = list(e), parameters = list(p1, p2)
)

# create result
x <- min_shortfall_result(
 id = "R1",
 area_budget_proportion = 0.78,
 area_data = d$get_planning_unit_areas(),
 boundary_data = d$get_boundary_data(),
 theme_data = ss$get_theme_data(),
 weight_data = ss$get_weight_data(),
 include_data = ss$get_include_data(),
 exclude_data = ss$get_exclude_data(),
 theme_settings = ss$get_theme_settings(),
 weight_settings = ss$get_weight_settings(),
 include_settings = ss$get_include_settings(),
 exclude_settings = ss$get_exclude_settings(),
 parameters = ss$parameters,
 gap_1 = p2$value * p2$status,
 boundary_gap = p1$value * p1$status
)

# print object
print(x)
#> Result object
```
