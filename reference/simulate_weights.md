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
#>   id:       b42bb6da-ccfa-4c80-b2a3-7a6009ce58cc
#>   name:     Transportation network
#>   variable: .../sim_raster_spatial.tif#Transportation_network [total: 133.47 ha]
#>   pane:  a8f3fea9-1888-4a12-97d5-de36704499c8-Transportation_network
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
#>   id:       e3566a05-44af-4e86-b85d-e7e7798e4a4b
#>   name:     Population count
#>   variable: .../sim_raster_spatial.tif#Population_count [total: 153.48 ha]
#>   pane:  e25e8e9f-6f4e-4092-9115-01c898829837-Population_count
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
