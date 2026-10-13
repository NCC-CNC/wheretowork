# New parameter

Create a new
[Parameter](https://ncc-cnc.github.io/wheretowork/reference/Parameter.md)
object.

## Usage

``` r
new_parameter(
  name,
  status = TRUE,
  value = 0,
  min_value = 0,
  max_value = 100,
  step_value = 1,
  hide = FALSE,
  disable = FALSE,
  units = "",
  reference_value = NA_real_,
  reference_units = "",
  tool_tip = "",
  id = uuid::UUIDgenerate()
)
```

## Arguments

- name:

  `character` Name to display.

- status:

  `logical` The initial status value. This is used to display
  information on whether the feature is selected (or not) for subsequent
  analysis. Defaults to `TRUE`.

- value:

  `numeric` initial value. Defaults to 0.

- min_value:

  `numeric` minimum value. Defaults to 0.

- max_value:

  `numeric` maximum value.

- step_value:

  `numeric` step value. Defaults to 1.

- hide:

  `logical` indicating if the slider should be hidden when the setting
  switch is on. Defaults to `FALSE`.

- disable:

  `logical` indicating if the setting should be disabled. Defaults to
  `FALSE`.

- units:

  `character` units. Defaults to an empty `character` object.

- reference_value:

  `numeric` reference value. This parameter is useful if the `value` is
  a relative value, because the total expected amount (i.e.
  `reference_value * value`) can be reported. Defaults to `NA_real_`
  indicating that no reference value information should be reported.

- reference_units:

  `character` units for the reference value. Defaults to "".

- tool_tip:

  `character` description of parameter setting.

- id:

  `character` unique identifier. Defaults to a random identifier
  ([`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html)).

## Value

A
[Parameter](https://ncc-cnc.github.io/wheretowork/reference/Parameter.md)
object.

## Examples

``` r
# create a new parameter
p <- new_parameter(name = "Spatial clustering")

# print object
print(p)
#> Parameter
#>   id:       743a0e31-0571-414d-a4fd-0210a5d2e551
#>   name:     Spatial clustering
#>   status:   TRUE
#>   value:   0 
#>   reference_value:   NA 
```
