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
#>   id:       12555d2e-08ff-4cd8-ae20-012aa69324d0
#>   name:     Uplands
#>   variable: .../sim_raster_spatial.tif#Uplands [total: 134 ]
#>   pane:  01a86c09-2186-408e-89a0-143776a232f8-Uplands
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
#>   id:       ca9edead-0e9a-4afa-8e10-d55467abb44e
#>   name:     Expensive land
#>   variable: .../sim_raster_spatial.tif#Expensive_land [total: 142 ]
#>   pane:  9f8b2588-db1b-4734-8769-a52d97a4e8fb-Expensive_land
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   FALSE
#>   overlap:   NA
#> 
```
