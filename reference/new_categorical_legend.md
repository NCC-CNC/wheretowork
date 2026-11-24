# New categorical legend

Create a new
[CategoricalLegend](https://ncc-cnc.github.io/wheretowork/reference/CategoricalLegend.md)
object.

## Usage

``` r
new_categorical_legend(values, colors)
```

## Arguments

- values:

  `numeric` Values to show in the legend.

- colors:

  `character` Colors to show in the legend. These colors should be in
  hex format (e.g. `"#AABBCC"`).

## Value

A
[CategoricalLegend](https://ncc-cnc.github.io/wheretowork/reference/CategoricalLegend.md)
object.

## Examples

``` r
# create new object
l <- new_categorical_legend(c(0, 1, 2), c("#000000", "#444444", "#AAAAAA"))

# print object
print(l)
#> <CategoricalLegend>
#>   Public:
#>     clone: function (deep = FALSE) 
#>     colors: #000000 #444444 #AAAAAA
#>     export: function () 
#>     get_color_map: function () 
#>     get_resample_method: function () 
#>     get_widget_data: function () 
#>     initialize: function (values, colors) 
#>     values: 0 1 2
```
