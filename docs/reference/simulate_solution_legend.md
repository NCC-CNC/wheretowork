# Simulate solution legend

This function simulates a legend for a
[Solution](https://ncc-cnc.github.io/wheretowork/reference/Solution.md)
object. It is primarily used for testing the package.

## Usage

``` r
simulate_solution_legend()
```

## Value

A
[ManualLegend](https://ncc-cnc.github.io/wheretowork/reference/ManualLegend.md)
object.

## Examples

``` r
print(simulate_solution_legend())
#> <ManualLegend>
#>   Public:
#>     clone: function (deep = FALSE) 
#>     colors: #00FFFF00 #B3E2CD
#>     export: function () 
#>     get_color_map: function () 
#>     get_resample_method: function () 
#>     get_widget_data: function () 
#>     initialize: function (values, colors, labels) 
#>     labels: not selected selected
#>     values: 0 1
```
