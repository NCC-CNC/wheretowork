# Continuous legend class

Continuous legend class

Continuous legend class

## Details

Definition for the `ContinuousLegend` class.

## Public fields

- `min_value`:

  `numeric` value.

- `max_value`:

  `numeric` value.

- `colors`:

  `character` vector.

- `n`:

  `numeric` value.

## Methods

### Public methods

- [`ContinuousLegend$new()`](#method-ContinuousLegend-new)

- [`ContinuousLegend$get_widget_data()`](#method-ContinuousLegend-get_widget_data)

- [`ContinuousLegend$get_resample_method()`](#method-ContinuousLegend-get_resample_method)

- [`ContinuousLegend$get_color_map()`](#method-ContinuousLegend-get_color_map)

- [`ContinuousLegend$export()`](#method-ContinuousLegend-export)

- [`ContinuousLegend$clone()`](#method-ContinuousLegend-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a `ContinuousLegend` object.

#### Usage

    ContinuousLegend$new(min_value, max_value, colors, n)

#### Arguments

- `min_value`:

  `numeric` value.

- `max_value`:

  `numeric` value.

- `colors`:

  `character` vector of colors.

- `n`:

  `numeric` value.

#### Returns

A new `ContinuousLegend` object.

------------------------------------------------------------------------

### Method `get_widget_data()`

Get data for creating a widget.

#### Usage

    ContinuousLegend$get_widget_data()

#### Returns

A new `ContinuousLegend` object.

------------------------------------------------------------------------

### Method `get_resample_method()`

Get resample method.

#### Usage

    ContinuousLegend$get_resample_method()

#### Returns

`character` object.

------------------------------------------------------------------------

### Method `get_color_map()`

Get a function for mapping values to colors.

#### Usage

    ContinuousLegend$get_color_map()

#### Returns

A `function` object.

------------------------------------------------------------------------

### Method `export()`

Export settings

#### Usage

    ContinuousLegend$export()

#### Returns

`list` object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ContinuousLegend$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
