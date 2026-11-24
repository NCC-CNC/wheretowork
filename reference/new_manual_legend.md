# New manual legend

Create a new
[ManualLegend](https://ncc-cnc.github.io/wheretowork/reference/ManualLegend.md)
object.

## Usage

``` r
new_manual_legend(values, colors, labels)
```

## Arguments

- values:

  `numeric` Values that are linked to the labels.

- colors:

  `character` Colors to show in the legend. These colors should be in
  hex format (e.g. `"#AABBCC"`). Arguments should contain two different
  colors.

- labels:

  `character` Labels to show in the legend.

## Value

A
[ManualLegend](https://ncc-cnc.github.io/wheretowork/reference/ManualLegend.md)
object.

## Examples

``` r
# create new object
l <- new_manual_legend(c(0, 1), c("#000000", "#AAAAAA"), c("a", "b"))

# print object
print(l)
#> <ManualLegend>
#>   Public:
#>     clone: function (deep = FALSE) 
#>     colors: #000000 #AAAAAA
#>     export: function () 
#>     get_color_map: function () 
#>     get_resample_method: function () 
#>     get_widget_data: function () 
#>     initialize: function (values, colors, labels) 
#>     labels: a b
#>     values: 0 1
```
