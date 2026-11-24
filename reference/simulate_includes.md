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
#>   id:       f880f442-14f3-4c10-93d0-aa3ae7c3a305
#>   name:     OECM areas
#>   variable: .../sim_raster_spatial.tif#OECM_areas [total: 168 ]
#>   pane:  e4db752e-91d4-4402-93e3-a642cc7b8ca2-OECM_areas
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
#>   id:       2b57fba7-6e84-4d1a-9908-a4ba8870c72a
#>   name:     Special Protection Area
#>   variable: .../sim_raster_spatial.tif#Special_Protection_Area [total: 156 ]
#>   pane:  a73f87e9-e77a-44b5-b291-2d8e358d1da7-Special_Protection_Area
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   TRUE
#>   overlap:   NA
#> 
```
