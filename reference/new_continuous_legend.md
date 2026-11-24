# New continuous legend

Create a new
[ContinuousLegend](https://ncc-cnc.github.io/wheretowork/reference/ContinuousLegend.md)
object.

## Usage

``` r
new_continuous_legend(min_value, max_value, colors, n = 4)
```

## Arguments

- min_value:

  `numeric` Minimum value to show in the legend.

- max_value:

  `numeric` Maximum value to show in the legend.

- colors:

  `character` Colors to show in the color the legend. These colors
  should be in hex format (e.g. `"#AABBCC"`) and are used to generate
  the color bar.

- n:

  `integer` Number of labels to display in the color bar. Default to 4.

## Value

A
[ContinuousLegend](https://ncc-cnc.github.io/wheretowork/reference/ContinuousLegend.md)
object.

## Examples

``` r
# create new object
l <- new_continuous_legend(0, 1, c("#000000", "#DDDDDD"))

# print object
print(l)
#> <ContinuousLegend>
#>   Public:
#>     clone: function (deep = FALSE) 
#>     colors: #000000 #DDDDDD
#>     export: function () 
#>     get_color_map: function () 
#>     get_resample_method: function () 
#>     get_widget_data: function () 
#>     initialize: function (min_value, max_value, colors, n) 
#>     max_value: 1
#>     min_value: 0
#>     n: 4
```
