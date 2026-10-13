# Theme results class

Theme results class

Theme results class

## Details

Definition for the `ThemeResults` class.

## See also

[`new_theme_results()`](https://ncc-cnc.github.io/wheretowork/reference/new_theme_results.md).

## Public fields

- `id`:

  `character` value.

- `theme`:

  [Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md)
  object.

- `feature_results`:

  `list` of
  [FeatureResults](https://ncc-cnc.github.io/wheretowork/reference/FeatureResults.md)
  objects.

## Methods

### Public methods

- [`ThemeResults$new()`](#method-ThemeResults-new)

- [`ThemeResults$print()`](#method-ThemeResults-print)

- [`ThemeResults$repr()`](#method-ThemeResults-repr)

- [`ThemeResults$get_results_data()`](#method-ThemeResults-get_results_data)

- [`ThemeResults$get_widget_data()`](#method-ThemeResults-get_widget_data)

- [`ThemeResults$clone()`](#method-ThemeResults-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a new `ThemeResults` object.

#### Usage

    ThemeResults$new(id, theme, feature_results)

#### Arguments

- `id`:

  `character` value.

- `theme`:

  \`[Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md)
  object.

- `feature_results`:

  `list` of
  [FeatureResults](https://ncc-cnc.github.io/wheretowork/reference/FeatureResults.md)
  objects.

#### Returns

A new `ThemeResults` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    ThemeResults$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    ThemeResults$repr(start = "[", end = "]")

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

    ThemeResults$get_results_data()

#### Returns

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
object.

------------------------------------------------------------------------

### Method `get_widget_data()`

Get data for displaying the object in a
[`solutionResults()`](https://ncc-cnc.github.io/wheretowork/reference/solutionResults-widget.md)
widget.

#### Usage

    ThemeResults$get_widget_data()

#### Returns

`list` with widget data.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ThemeResults$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
