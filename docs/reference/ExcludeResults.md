# Exclude results class

Exclude results class

Exclude results class

## Details

Definition for the `ExcludeResults` class.

## See also

[`new_exclude_results()`](https://ncc-cnc.github.io/wheretowork/reference/new_exclude_results.md).

## Public fields

- `id`:

  `character` value.

- `exclude`:

  [Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md)
  object.

- `status`:

  `logical` value.

- `held`:

  `numeric` value.

## Methods

### Public methods

- [`ExcludeResults$new()`](#method-ExcludeResults-new)

- [`ExcludeResults$print()`](#method-ExcludeResults-print)

- [`ExcludeResults$repr()`](#method-ExcludeResults-repr)

- [`ExcludeResults$get_results_data()`](#method-ExcludeResults-get_results_data)

- [`ExcludeResults$get_widget_data()`](#method-ExcludeResults-get_widget_data)

- [`ExcludeResults$clone()`](#method-ExcludeResults-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a new `ExcludeResults` object.

#### Usage

    ExcludeResults$new(id, exclude, held)

#### Arguments

- `id`:

  `character` value.

- `exclude`:

  [Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md)
  object.

- `held`:

  `numeric` value.

#### Returns

A new `ExcludeResults` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    ExcludeResults$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    ExcludeResults$repr(start = "[", end = "]")

#### Arguments

- `start`:

  `character` symbol used to start the setting list. Defaults to `"["`.

- `end`:

  `character` symbol used to start the setting list. Defaults to `"]"`.

#### Returns

`character` value.

------------------------------------------------------------------------

### Method `get_results_data()`

Get results.

#### Usage

    ExcludeResults$get_results_data()

#### Returns

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
object.

------------------------------------------------------------------------

### Method `get_widget_data()`

Get data for displaying the object in a
[`solutionResults()`](https://ncc-cnc.github.io/wheretowork/reference/solutionResults-widget.md)
widget.

#### Usage

    ExcludeResults$get_widget_data()

#### Returns

`list` with widget data.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ExcludeResults$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
