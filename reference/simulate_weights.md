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
#>   id:       e701bdc0-a154-4318-9350-7a202a7ebd1c
#>   name:     Transportation network
#>   variable: .../sim_raster_spatial.tif#Transportation_network [total: 133.47 ha]
#>   pane:  ee472820-6458-4939-81fa-0a8d0b8521dc-Transportation_network
#>   current:  0.38
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
#>   id:       8fcb68e5-6476-458c-8c1b-6fd8d489084c
#>   name:     Population count
#>   variable: .../sim_raster_spatial.tif#Population_count [total: 153.48 ha]
#>   pane:  cd560da1-c469-4b69-acd6-cc933e9aac0b-Population_count
#>   current:  0.41
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   TRUE
#>   factor:   0
#> 

```
