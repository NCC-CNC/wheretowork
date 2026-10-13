# Simulate excludes

This function simulates
[Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md)
objects.

## Usage

``` r
simulate_excludes(dataset, n)
```

## Arguments

- dataset:

  [Dataset](https://ncc-cnc.github.io/wheretowork/reference/Dataset.md)
  object.

- n:

  `integer` number of objects to simulate.

## Value

A `list` of simulated
[Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md)
objects.

## See also

[new_exclude](https://ncc-cnc.github.io/wheretowork/reference/new_exclude.md).

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
x <- simulate_excludes(dataset = d, n = 2)
# print results
print(x)
#> [[1]]
#> Exclude
#>   id:       22c69187-b8a3-4c74-b3d6-1ce2823c743c
#>   name:     Agriculture area
#>   variable: .../sim_raster_spatial.tif#Agriculture_area [total: 130 ]
#>   pane:  2e265742-b179-43a9-a264-6b0d9586c101-Agriculture_area
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   FALSE
#>   overlap:   NA
#> 
#> [[2]]
#> Exclude
#>   id:       4a64d299-167a-40b1-b932-10d9ef96e640
#>   name:     Shipping area
#>   variable: .../sim_raster_spatial.tif#Shipping_area [total: 123 ]
#>   pane:  30445678-812a-4466-9257-ff93e0fa1357-Shipping_area
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   FALSE
#>   overlap:   NA
#> 
```
