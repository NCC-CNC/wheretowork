# New statistic

Create a new
[Statistic](https://ncc-cnc.github.io/wheretowork/reference/Statistic.md)
object.

## Usage

``` r
new_statistic(name, value, units, proportion = NA_real_)
```

## Arguments

- name:

  `character` name of statistic.

- value:

  `numeric` value or `NA_real_`. This parameter describes the statistic
  in absolute terms (e.g. 30).

- units:

  `character` value. This parameter contains the units for the statistic
  (e.g. \`"km

- proportion:

  `numeric` value (optional) . This parameter describes the statistic in
  relative terms (e.g. 30%). Note that values are expressed as a
  proportion (e.g. 0.3 indicates 30%).

## Value

A
[Statistic](https://ncc-cnc.github.io/wheretowork/reference/Statistic.md)
object.

## Examples

``` r
# create a statistic
x <- new_statistic(name = "Area", value = 12, units = "ha", proportion = 0.4)

# print object
print(x)
#> Area 12 ha [40%]
```
