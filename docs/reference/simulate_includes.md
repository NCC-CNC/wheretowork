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
#>   id:       87dd6179-700b-45c8-946b-8976ba07d63f
#>   name:     World Heritage Sites
#>   variable: .../sim_raster_spatial.tif#World_Heritage_Sites [total: 135 ]
#>   pane:  a5f4d737-93e7-4ce7-974d-fa657d932d92-World_Heritage_Sites
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
#>   id:       7f6ca809-ab97-42f4-b940-871c43bfb318
#>   name:     Managed resource areas
#>   variable: .../sim_raster_spatial.tif#Managed_resource_areas [total: 153 ]
#>   pane:  512e5714-91d7-4115-9c7c-bf998ae7a403-Managed_resource_areas
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   TRUE
#>   overlap:   NA
#> 
```
