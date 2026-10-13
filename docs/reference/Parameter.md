# Parameter class

Parameter class

Parameter class

## Details

Definition for the Parameter class.

## See also

[`new_parameter()`](https://ncc-cnc.github.io/wheretowork/reference/new_parameter.md).

## Public fields

- `id`:

  `character` value.

- `name`:

  `character` value.

- `status`:

  `logical` value.

- `value`:

  `numeric` initial value value.

- `min_value`:

  `numeric` minimum value.

- `max_value`:

  `numeric` maximum value.

- `step_value`:

  `numeric` step value.

- `hide`:

  `logical` value.

- `disable`:

  `logical` value.

- `units`:

  `character` value.

- `reference_value`:

  `numeric` reference value.

- `reference_units`:

  `character` units for reference value.

- `tool_tip`:

  `character` description of parameter setting.

## Methods

### Public methods

- [`Parameter$new()`](#method-Parameter-new)

- [`Parameter$print()`](#method-Parameter-print)

- [`Parameter$repr()`](#method-Parameter-repr)

- [`Parameter$get_status()`](#method-Parameter-get_status)

- [`Parameter$get_value()`](#method-Parameter-get_value)

- [`Parameter$get_setting()`](#method-Parameter-get_setting)

- [`Parameter$set_status()`](#method-Parameter-set_status)

- [`Parameter$set_value()`](#method-Parameter-set_value)

- [`Parameter$set_setting()`](#method-Parameter-set_setting)

- [`Parameter$get_widget_data()`](#method-Parameter-get_widget_data)

- [`Parameter$export()`](#method-Parameter-export)

- [`Parameter$get_results_data()`](#method-Parameter-get_results_data)

- [`Parameter$clone()`](#method-Parameter-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a new Parameter object.

#### Usage

    Parameter$new(
      id,
      name,
      status,
      value,
      min_value,
      max_value,
      step_value,
      hide,
      disable,
      units,
      reference_value,
      reference_units,
      tool_tip
    )

#### Arguments

- `id`:

  `character` value.

- `name`:

  `character` value.

- `status`:

  `logical` value.

- `value`:

  `numeric` initial value.

- `min_value`:

  `numeric` minimum value.

- `max_value`:

  `numeric` maximum value.

- `step_value`:

  `numeric` step value.

- `hide`:

  `logical` value.

- `disable`:

  `logical` value.

- `units`:

  `character` value.

- `reference_value`:

  `numeric` value.

- `reference_units`:

  `character` value.

- `tool_tip`:

  `character` value.

#### Returns

A new Parameter object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    Parameter$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    Parameter$repr(start = "[", end = "]")

#### Arguments

- `start`:

  `character` symbol used to start the setting list. Defaults to `"["`.

- `end`:

  `character` symbol used to start the setting list. Defaults to `"]"`.

#### Returns

`character` value.

------------------------------------------------------------------------

### Method `get_status()`

Get status.

#### Usage

    Parameter$get_status()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_value()`

Get value.

#### Usage

    Parameter$get_value()

#### Returns

`numeric` value.

------------------------------------------------------------------------

### Method `get_setting()`

Get setting.

#### Usage

    Parameter$get_setting(name)

#### Arguments

- `name`:

  `character` setting name. Available options are `"status"`, or
  `"value"`.

#### Returns

Value.

------------------------------------------------------------------------

### Method `set_status()`

Set status.

#### Usage

    Parameter$set_status(value)

#### Arguments

- `value`:

  `logical` new value.

------------------------------------------------------------------------

### Method `set_value()`

Set value.

#### Usage

    Parameter$set_value(value)

#### Arguments

- `value`:

  `numeric` new value.

------------------------------------------------------------------------

### Method `set_setting()`

Set setting.

#### Usage

    Parameter$set_setting(name, value)

#### Arguments

- `name`:

  `character` setting name. Available options are `"status"`, or
  `"value"`.

- `value`:

  `ANY` new value.

------------------------------------------------------------------------

### Method `get_widget_data()`

Get data for displaying the parameter in a widget.

#### Usage

    Parameter$get_widget_data()

------------------------------------------------------------------------

### Method `export()`

Export settings.

#### Usage

    Parameter$export()

#### Returns

`list` object.

------------------------------------------------------------------------

### Method `get_results_data()`

Get results data.

#### Usage

    Parameter$get_results_data()

#### Returns

`data.frame` object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Parameter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
