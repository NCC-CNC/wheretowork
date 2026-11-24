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
#>   id:        fc819f16-4bbc-4360-b292-04c717d3edea
#>   name:      Drab Seedeater
#>   feature: 
#>     Drab Seedeater habitat [status: TRUE, current: 0.3, goal: 0.83]
#>       variable: .../sim_raster_spatial.tif#Drab_Seedeater [total: 360.86 ha]
#> 
#> [[2]]
#> Theme
#>   id:        adac5ea4-574f-481c-86ed-0319e927a0c9
#>   name:      Melodious Warbler
#>   feature: 
#>     Melodious Warbler habitat [status: TRUE, current: 0.35, goal: 0.69]
#>       variable: .../sim_raster_spatial.tif#Melodious_Warbler [total: 647 ha]
#> 
#> [[3]]
#> Theme
#>   id:        735ef8a7-6fa3-4b70-b2d0-97a7a5c27874
#>   name:      Eurasian Blue Tit
#>   feature: 
#>     Eurasian Blue Tit habitat [status: TRUE, current: 0.55, goal: 0.81]
#>       variable: .../sim_raster_spatial.tif#Eurasian_Blue_Tit [total: 3671.06 ha]
#> 
#> [[4]]
#> Theme
#>   id:        ff33da89-445d-47b2-ba6a-6b745c14b01b
#>   name:      Leafbirds
#>   feature: 
#>     Blue-winged Leafbird (Javan) [status: TRUE, current: 0.3, goal: 0.58]
#>       variable: .../sim_raster_spatial.tif#Yellow_throated_Leafbird [total: 25637.35 ha]
#>     Blue-winged Leafbird (Blue-winged) [status: TRUE, current: 0.14, goal: 0.61]
#>       variable: .../sim_raster_spatial.tif#Yellow_throated_Leafbird [total: 25637.35 ha]
#>     Blue-winged Leafbird [status: TRUE, current: 0.24, goal: 0.61]
#>       variable: .../sim_raster_spatial.tif#Yellow_throated_Leafbird [total: 25637.35 ha]
#>     Sumatran Leafbird [status: TRUE, current: 0.13, goal: 0.61]
#>       variable: .../sim_raster_spatial.tif#Yellow_throated_Leafbird [total: 25637.35 ha]
#>     Jerdon's Leafbird [status: TRUE, current: 0.4, goal: 0.72]
#>       variable: .../sim_raster_spatial.tif#Yellow_throated_Leafbird [total: 25637.35 ha]
#>     Yellow-throated Leafbird [status: TRUE, current: 0.45, goal: 0.72]
#>       variable: .../sim_raster_spatial.tif#Yellow_throated_Leafbird [total: 25637.35 ha]
#> 
#> [[5]]
#> Theme
#>   id:        a3f215ec-b115-4a64-aed5-a4f53c54f334
#>   name:      Ground Babblers and Allies
#>   feature: 
#>     Horsfield's Babbler (Hartert's) [status: TRUE, current: 0.41, goal: 0.69]
#>       variable: .../sim_raster_spatial.tif#Ferruginous_Babbler [total: 13878.87 ha]
#>     Black-crowned Fulvetta [status: TRUE, current: 0.11, goal: 0.67]
#>       variable: .../sim_raster_spatial.tif#Ferruginous_Babbler [total: 13878.87 ha]
#>     Short-tailed Scimitar-Babbler [status: TRUE, current: 0.44, goal: 0.69]
#>       variable: .../sim_raster_spatial.tif#Ferruginous_Babbler [total: 13878.87 ha]
#>     Moustached Babbler [status: TRUE, current: 0.37, goal: 0.69]
#>       variable: .../sim_raster_spatial.tif#Ferruginous_Babbler [total: 13878.87 ha]
#>     Black-throated Wren-Babbler [status: TRUE, current: 0.24, goal: 0.85]
#>       variable: .../sim_raster_spatial.tif#Ferruginous_Babbler [total: 13878.87 ha]
#>     Horsfield's Babbler [status: TRUE, current: 0.57, goal: 0.82]
#>       variable: .../sim_raster_spatial.tif#Ferruginous_Babbler [total: 13878.87 ha]
#>     Ferruginous Babbler [status: TRUE, current: 0.42, goal: 0.87]
#>       variable: .../sim_raster_spatial.tif#Ferruginous_Babbler [total: 13878.87 ha]
#> 
```
