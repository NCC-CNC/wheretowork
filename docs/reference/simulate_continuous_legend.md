# Simulate continuous legend

This function simulates a continuous legend object. It is primarily used
for testing the package.

## Usage

``` r
simulate_continuous_legend()
```

## Value

A
[ContinuousLegend](https://ncc-cnc.github.io/wheretowork/reference/ContinuousLegend.md)
object.

## See also

[`new_continuous_legend()`](https://ncc-cnc.github.io/wheretowork/reference/new_continuous_legend.md).

## Examples

``` r
print(simulate_continuous_legend())
#> <ContinuousLegend>
#>   Public:
#>     clone: function (deep = FALSE) 
#>     colors: #F7FCFD #E0ECF4 #BFD3E6 #9EBCDA #8C96C6 #8C6BB1 #88419D  ...
#>     export: function () 
#>     get_color_map: function () 
#>     get_resample_method: function () 
#>     get_widget_data: function () 
#>     initialize: function (min_value, max_value, colors, n) 
#>     max_value: 55.5285304349381
#>     min_value: 11.9102191529237
#>     n: 4
```
