# Simulate include legend

This function simulates a legend for a
[Include](https://ncc-cnc.github.io/wheretowork/reference/Include.md)
object. It is primarily used for testing the package.

## Usage

``` r
simulate_include_legend()
```

## Value

A
[ManualLegend](https://ncc-cnc.github.io/wheretowork/reference/ManualLegend.md)
object.

## Examples

``` r
print(simulate_include_legend())
#> <ManualLegend>
#>   Public:
#>     clone: function (deep = FALSE) 
#>     colors: #00FFFF00 #E41A1C
#>     export: function () 
#>     get_color_map: function () 
#>     get_resample_method: function () 
#>     get_widget_data: function () 
#>     initialize: function (values, colors, labels) 
#>     labels: not included included
#>     values: 0 1
```
