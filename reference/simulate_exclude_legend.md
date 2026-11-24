# Simulate exclude legend

This function simulates a legend for a
[Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md)
object. It is primarily used for testing the package.

## Usage

``` r
simulate_exclude_legend()
```

## Value

A
[ManualLegend](https://ncc-cnc.github.io/wheretowork/reference/ManualLegend.md)
object.

## Examples

``` r
print(simulate_exclude_legend())
#> <ManualLegend>
#>   Public:
#>     clone: function (deep = FALSE) 
#>     colors: #00FFFF00 #1B9E77
#>     export: function () 
#>     get_color_map: function () 
#>     get_resample_method: function () 
#>     get_widget_data: function () 
#>     initialize: function (values, colors, labels) 
#>     labels: included excluded
#>     values: 0 1
```
