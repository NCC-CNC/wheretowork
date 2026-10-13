# Simulate weights

This function simulates
[Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md)
objects.

## Usage

``` r
simulate_weights(dataset, n, continuous = TRUE)
```

## Arguments

- dataset:

  [Dataset](https://ncc-cnc.github.io/wheretowork/reference/Dataset.md)
  object.

- n:

  `integer` number of weights to simulate.

- continuous:

  `logical` should the data be continuous? Defaults to `TRUE`.

## Value

A `list` of simulated
[Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md)
objects.

## See also

[new_weight](https://ncc-cnc.github.io/wheretowork/reference/new_weight.md).

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
x <- simulate_weights(dataset = d, n = 2)
# print results
print(x)
#> [[1]]
#> Weight
#>   id:       b23a3b71-07c5-4f60-bbe7-2a229737df62
#>   name:     Invasive species
#>   variable: .../sim_raster_spatial.tif#Invasive_species [total: 144.37 ha]
#>   pane:  1e35a679-ea75-4b82-8db0-b2e9f8f3abd6-Invasive_species
#>   current:  0.54
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   TRUE
#>   factor:   0
#> 
#> [[2]]
#> Weight
#>   id:       644579bd-384f-48c0-928e-dea0f1c22503
#>   name:     Transportation network
#>   variable: .../sim_raster_spatial.tif#Transportation_network [total: 179.99 ha]
#>   pane:  6b464130-bde3-4988-b39d-2adf17d476a0-Transportation_network
#>   current:  0.56
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   TRUE
#>   factor:   0
#> 

```
