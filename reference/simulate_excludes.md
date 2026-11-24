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
#>   id:       35ce49a1-efc7-4961-b3e0-57522907f402
#>   name:     Uplands
#>   variable: .../sim_raster_spatial.tif#Uplands [total: 134 ]
#>   pane:  bce89a73-b958-48cd-87a2-152b9327026d-Uplands
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
#>   id:       c8559429-4a23-417a-b995-f62235428c19
#>   name:     Expensive land
#>   variable: .../sim_raster_spatial.tif#Expensive_land [total: 142 ]
#>   pane:  c1ef236d-2a32-406f-957a-8f1309057c48-Expensive_land
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   FALSE
#>   overlap:   NA
#> 
```
