# Simulate themes

This function simulates
[Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md)
objects.

## Usage

``` r
simulate_themes(
  dataset,
  n_single_themes,
  n_multi_themes,
  lambda = 5,
  continuous = NA
)
```

## Arguments

- dataset:

  [Dataset](https://ncc-cnc.github.io/wheretowork/reference/Dataset.md)
  object.

- n_single_themes:

  `integer` number of themes with a single feature to simulate.

- n_multi_themes:

  `integer` number of themes with a multiple features to simulate.

- lambda:

  `numeric` lambda parameter for simulating the number of features for
  themes that contain multiple features. This parameter is used as a
  argument to `rpois(n, lambda = lambda)`. Defaults to 5.

- continuous:

  `logical` should the data be continuous? Defaults to `NA`. If `NA`,
  simulated themes are a mix of continuous and categorical defined by a
  conditional: `stats::runif(1) > 0.5`

## Value

A `list` of simulated
[Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md)
objects.

## See also

[new_theme](https://ncc-cnc.github.io/wheretowork/reference/new_theme.md).

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

# simulate data
x <- simulate_themes(data = d, n_single_themes = 3, n_multi_themes = 2)
# print results
print(x)
#> [[1]]
#> Theme
#>   id:        e31fe3f1-6121-4700-9e54-4198c3660bc0
#>   name:      Sarus Crane
#>   feature: 
#>     Sarus Crane habitat [status: TRUE, current: 0.45, goal: 0.81]
#>       variable: .../sim_raster_spatial.tif#Sarus_Crane [total: 33181.57 ha]
#> 
#> [[2]]
#> Theme
#>   id:        0babf82e-5690-40da-8926-75c86ed93588
#>   name:      Jerdon's Courser
#>   feature: 
#>     Jerdon's Courser habitat [status: TRUE, current: 0.59, goal: 0.57]
#>       variable: .../sim_raster_spatial.tif#Jerdon_s_Courser [total: 32290.19 ha]
#> 
#> [[3]]
#> Theme
#>   id:        6deaf799-e284-438b-ab03-f2bdd69bc41e
#>   name:      Village Weaver (Mottled)
#>   feature: 
#>     Village Weaver (Mottled) habitat [status: TRUE, current: 0.19, goal: 0.66]
#>       variable: .../sim_raster_spatial.tif#Village_Weaver_Mottled_ [total: 3706.42 ha]
#> 
#> [[4]]
#> Theme
#>   id:        b029a803-72c4-4de7-972d-b8da5a8ed64e
#>   name:      Treecreepers
#>   feature: 
#>     Short-toed Treecreeper [status: TRUE, current: 0.43, goal: 0.51]
#>       variable: .../sim_raster_spatial.tif#Brown_Creeper_montana_idahoensis_ [total: 615 ha]
#>     Brown Creeper [status: TRUE, current: 0.52, goal: 0.83]
#>       variable: .../sim_raster_spatial.tif#Brown_Creeper_montana_idahoensis_ [total: 615 ha]
#>     Sikkim Treecreeper [status: TRUE, current: 0.34, goal: 0.65]
#>       variable: .../sim_raster_spatial.tif#Brown_Creeper_montana_idahoensis_ [total: 615 ha]
#>     Hume's Treecreeper [status: TRUE, current: 0.17, goal: 0.72]
#>       variable: .../sim_raster_spatial.tif#Brown_Creeper_montana_idahoensis_ [total: 615 ha]
#>     African Spotted Creeper [status: TRUE, current: 0.12, goal: 0.62]
#>       variable: .../sim_raster_spatial.tif#Brown_Creeper_montana_idahoensis_ [total: 615 ha]
#>     Brown Creeper (americana/nigrescens) [status: TRUE, current: 0.56, goal: 0.85]
#>       variable: .../sim_raster_spatial.tif#Brown_Creeper_montana_idahoensis_ [total: 615 ha]
#>     Brown Creeper (occidentalis Group) [status: TRUE, current: 0.31, goal: 0.62]
#>       variable: .../sim_raster_spatial.tif#Brown_Creeper_montana_idahoensis_ [total: 615 ha]
#>     Brown Creeper (montana/idahoensis) [status: TRUE, current: 0.11, goal: 0.6]
#>       variable: .../sim_raster_spatial.tif#Brown_Creeper_montana_idahoensis_ [total: 615 ha]
#> 
#> [[5]]
#> Theme
#>   id:        2a110b07-7da7-4253-871b-ac512b33c1ac
#>   name:      Wrens
#>   feature: 
#>     Eurasian Wren (Hebridean) [status: TRUE, current: 0.31, goal: 0.67]
#>       variable: .../sim_raster_spatial.tif#Cactus_Wren [total: 25711.04 ha]
#>     Carolina Wren (Northeast Mexico/South Texas) [status: TRUE, current: 0.46, goal: 0.52]
#>       variable: .../sim_raster_spatial.tif#Cactus_Wren [total: 25711.04 ha]
#>     House Wren (Grenada) [status: TRUE, current: 0.49, goal: 0.81]
#>       variable: .../sim_raster_spatial.tif#Cactus_Wren [total: 25711.04 ha]
#>     House Wren [status: TRUE, current: 0.17, goal: 0.65]
#>       variable: .../sim_raster_spatial.tif#Cactus_Wren [total: 25711.04 ha]
#>     Winter Wren [status: TRUE, current: 0.33, goal: 0.68]
#>       variable: .../sim_raster_spatial.tif#Cactus_Wren [total: 25711.04 ha]
#>     Thrush-like Wren [status: TRUE, current: 0.18, goal: 0.78]
#>       variable: .../sim_raster_spatial.tif#Cactus_Wren [total: 25711.04 ha]
#>     Rufous-naped Wren (Sclater's) [status: TRUE, current: 0.15, goal: 0.68]
#>       variable: .../sim_raster_spatial.tif#Cactus_Wren [total: 25711.04 ha]
#>     Cactus Wren [status: TRUE, current: 0.49, goal: 0.67]
#>       variable: .../sim_raster_spatial.tif#Cactus_Wren [total: 25711.04 ha]
#> 
```
