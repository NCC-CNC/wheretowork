# Statistic class

Statistic class

Statistic class

## Details

Definition for the Statistic class.

## See also

[`new_statistic()`](https://ncc-cnc.github.io/wheretowork/reference/new_statistic.md).

## Public fields

- `name`:

  `character` value.

- `value`:

  `numeric` value

- `units`:

  `character` value.

- `proportion`:

  `numeric` value

## Methods

### Public methods

- [`Statistic$new()`](#method-Statistic-new)

- [`Statistic$print()`](#method-Statistic-print)

- [`Statistic$repr()`](#method-Statistic-repr)

- [`Statistic$get_widget_data()`](#method-Statistic-get_widget_data)

- [`Statistic$get_results_data()`](#method-Statistic-get_results_data)

- [`Statistic$clone()`](#method-Statistic-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a Statistic object.

#### Usage

    Statistic$new(name, value, units, proportion)

#### Arguments

- `name`:

  `character` value.

- `value`:

  `numeric` value

- `units`:

  `character` value.

- `proportion`:

  `numeric` value.

#### Returns

A Statistic object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    Statistic$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    Statistic$repr(...)

#### Arguments

- `...`:

  not used.

#### Returns

`character` value.

------------------------------------------------------------------------

### Method `get_widget_data()`

Get widget data.

#### Usage

    Statistic$get_widget_data()

#### Returns

`list` object.

------------------------------------------------------------------------

### Method `get_results_data()`

Get results data.

#### Usage

    Statistic$get_results_data()

#### Returns

`data.frame` object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Statistic$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
