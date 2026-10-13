# Simulate categorical legend

This function simulates a categorical legend object. It is primarily
used for testing the package.

## Usage

``` r
simulate_categorical_legend()
```

## Value

A
[CategoricalLegend](https://ncc-cnc.github.io/wheretowork/reference/CategoricalLegend.md)
object.

## See also

[`new_categorical_legend()`](https://ncc-cnc.github.io/wheretowork/reference/new_categorical_legend.md).

## Examples

``` r
print(simulate_continuous_legend())
#> <ContinuousLegend>
#>   Public:
#>     clone: function (deep = FALSE) 
#>     colors: #FFFFE5 #F7FCB9 #D9F0A3 #ADDD8E #78C679 #41AB5D #238443  ...
#>     export: function () 
#>     get_color_map: function () 
#>     get_resample_method: function () 
#>     get_widget_data: function () 
#>     initialize: function (min_value, max_value, colors, n) 
#>     max_value: 60.334155241726
#>     min_value: 13.4214912424795
#>     n: 4
```
