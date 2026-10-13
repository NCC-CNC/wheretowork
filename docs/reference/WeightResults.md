# Weight results class

Weight results class

Weight results class

## Details

Definition for the `WeightResults` class.

## See also

[`new_weight_results()`](https://ncc-cnc.github.io/wheretowork/reference/new_weight_results.md).

## Public fields

- `id`:

  `character` value.

- `weight`:

  [Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md)
  object.

- `status`:

  `logical` value.

- `factor`:

  `numeric` value.

- `current`:

  `numeric` value.

- `held`:

  `numeric` value.

## Methods

### Public methods

- [`WeightResults$new()`](#method-WeightResults-new)

- [`WeightResults$print()`](#method-WeightResults-print)

- [`WeightResults$repr()`](#method-WeightResults-repr)

- [`WeightResults$get_results_data()`](#method-WeightResults-get_results_data)

- [`WeightResults$get_widget_data()`](#method-WeightResults-get_widget_data)

- [`WeightResults$clone()`](#method-WeightResults-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a new `WeightResults` object.

#### Usage

    WeightResults$new(id, weight, held)

#### Arguments

- `id`:

  `character` value.

- `weight`:

  [Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md)
  object.

- `held`:

  `numeric` value.

#### Returns

A new `WeightResults` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    WeightResults$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    WeightResults$repr(start = "[", end = "]")

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

    WeightResults$get_results_data()

#### Returns

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
object.

------------------------------------------------------------------------

### Method `get_widget_data()`

Get data for displaying the object in a
[`solutionResults()`](https://ncc-cnc.github.io/wheretowork/reference/solutionResults-widget.md)
widget.

#### Usage

    WeightResults$get_widget_data()

#### Returns

`list` with widget data.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    WeightResults$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
