# New solution settings

Create a new
[SolutionSettings](https://ncc-cnc.github.io/wheretowork/reference/SolutionSettings.md)
object.

## Usage

``` r
new_solution_settings(themes, weights, includes, excludes, parameters)
```

## Arguments

- themes:

  `list` of
  [Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md)
  objects.

- weights:

  `list` of
  [Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md)
  objects.

- includes:

  `list` of
  [Include](https://ncc-cnc.github.io/wheretowork/reference/Include.md)
  objects.

- excludes:

  `list` of
  [Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md)
  objects.

- parameters:

  `list` of
  [Parameter](https://ncc-cnc.github.io/wheretowork/reference/Parameter.md)
  objects.

## Value

A
[SolutionSettings](https://ncc-cnc.github.io/wheretowork/reference/SolutionSettings.md)
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
i1 <- new_include(
  name = "Protected areas", variable = v5,
  status = FALSE, id = "I1"
)

# create an included using a variable
i2 <- new_include(
  name = "Bases", variable = v5,
  status = FALSE, id = "I2"
) 

# create an exclude using a variable
e1 <- new_exclude(
  name = "Urban areas", variable = v6,
  status = FALSE, id = "E1"
)

# create an exclude using a variable
e2 <- new_exclude(
  name = "Pot holes", variable = v3,
  status = FALSE, id = "E2"
)

# create parameters
p1 <- new_parameter(name = "Spatial clustering")
p2 <- new_parameter(name = "Optimality gap")

# create solution settings using the themes and weight
ss <- new_solution_settings(
  themes = list(t1, t2), weights = list(w), includes = list(i1),
  excludes = list(e1, e2), parameters = list(p1, p2)
)

# print object
print(ss)
#> SolutionSettings
#>   themes: 
#>     Species:
#>       Possum [status: FALSE, current: 0.5, goal: 0.2]
#>         variable: .../sim_raster_spatial.tif#Gough_Island_Finch [total: 25747.97 ]
#>     Ecoregions:
#>       Forests [status: FALSE, current: 0.9, goal: 0.3]
#>         variable: .../sim_raster_spatial.tif#Manus_Boobook [total: 23474.88 ]
#>       Shrubs [status: TRUE, current: 0.4, goal: 0.6]
#>         variable: .../sim_raster_spatial.tif#Red_chested_Flufftail [total: 322.75 ]
#>   weights: 
#>     Human Footprint Index [status: FALSE, current: 0, factor: -90]
#>       variable: .../sim_raster_spatial.tif#Brown_necked_Parrot_Brown_necked_ [total: 483 ]
#>   includes: 
#>     Protected areas [status: FALSE]
#>       variable: .../sim_raster_spatial.tif#Chestnut_headed_Flufftail [total: 307.21 ]
#>   excludes: 
#>     Urban areas [status: FALSE]
#>       variable: .../sim_raster_spatial.tif#Tsingy_Wood_Rail [total: 27374.71 ]
#>     Pot holes [status: FALSE]
#>       variable: .../sim_raster_spatial.tif#Manus_Boobook [total: 23474.88 ]
#>   parameters: 
#>     Spatial clustering [status: TRUE, value: 0 ]
#>     Optimality gap [status: TRUE, value: 0 ]
```
