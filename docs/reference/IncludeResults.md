# Include results class

Include results class

Include results class

## Details

Definition for the `IncludeResults` class.

## See also

[`new_include_results()`](https://ncc-cnc.github.io/wheretowork/reference/new_include_results.md).

## Public fields

- `id`:

  `character` value.

- `include`:

  [Include](https://ncc-cnc.github.io/wheretowork/reference/Include.md)
  object.

- `status`:

  `logical` value.

- `held`:

  `numeric` value.

## Methods

### Public methods

- [`IncludeResults$new()`](#method-IncludeResults-new)

- [`IncludeResults$print()`](#method-IncludeResults-print)

- [`IncludeResults$repr()`](#method-IncludeResults-repr)

- [`IncludeResults$get_results_data()`](#method-IncludeResults-get_results_data)

- [`IncludeResults$get_widget_data()`](#method-IncludeResults-get_widget_data)

- [`IncludeResults$clone()`](#method-IncludeResults-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a new `IncludeResults` object.

#### Usage

    IncludeResults$new(id, include, held)

#### Arguments

- `id`:

  `character` value.

- `include`:

  [Include](https://ncc-cnc.github.io/wheretowork/reference/Include.md)
  object.

- `held`:

  `numeric` value.

#### Returns

A new `IncludeResults` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    IncludeResults$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    IncludeResults$repr(start = "[", end = "]")

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

    IncludeResults$get_results_data()

#### Returns

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
object.

------------------------------------------------------------------------

### Method `get_widget_data()`

Get data for displaying the object in a
[`solutionResults()`](https://ncc-cnc.github.io/wheretowork/reference/solutionResults-widget.md)
widget.

#### Usage

    IncludeResults$get_widget_data()

#### Returns

`list` with widget data.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    IncludeResults$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
