# Simulate includes

This function simulates
[Include](https://ncc-cnc.github.io/wheretowork/reference/Include.md)
objects.

## Usage

``` r
simulate_includes(dataset, n)
```

## Arguments

- dataset:

  [Dataset](https://ncc-cnc.github.io/wheretowork/reference/Dataset.md)
  object.

- n:

  `integer` number of objects to simulate.

## Value

A `list` of simulated
[Include](https://ncc-cnc.github.io/wheretowork/reference/Include.md)
objects.

## See also

[new_include](https://ncc-cnc.github.io/wheretowork/reference/new_include.md).

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
x <- simulate_includes(dataset = d, n = 2)
# print results
print(x)
#> [[1]]
#> Include
#>   id:       55e5df4d-4d05-464b-a254-27e557a73917
#>   name:     OECM areas
#>   variable: .../sim_raster_spatial.tif#OECM_areas [total: 168 ]
#>   pane:  c45483fd-d065-41a7-88b0-5d26c671af31-OECM_areas
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   TRUE
#>   overlap:   NA
#> 
#> [[2]]
#> Include
#>   id:       ae246093-d951-4ba6-8a96-6732ab09fff4
#>   name:     Special Protection Area
#>   variable: .../sim_raster_spatial.tif#Special_Protection_Area [total: 156 ]
#>   pane:  07c1028a-0546-4a8d-8ac7-1d59d3138ecf-Special_Protection_Area
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   TRUE
#>   overlap:   NA
#> 
```
