# Manual legend class

Manual legend class

Manual legend class

## Details

Definition for the `ManualLegend` class.

## Public fields

- `values`:

  `values` vector.

- `colors`:

  `character` vector.

- `labels`:

  `character` vector.

## Methods

### Public methods

- [`ManualLegend$new()`](#method-ManualLegend-new)

- [`ManualLegend$get_resample_method()`](#method-ManualLegend-get_resample_method)

- [`ManualLegend$get_color_map()`](#method-ManualLegend-get_color_map)

- [`ManualLegend$get_widget_data()`](#method-ManualLegend-get_widget_data)

- [`ManualLegend$export()`](#method-ManualLegend-export)

- [`ManualLegend$clone()`](#method-ManualLegend-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a `ManualLegend` object.

#### Usage

    ManualLegend$new(values, colors, labels)

#### Arguments

- `values`:

  `numeric` vector of values.

- `colors`:

  `character` vector of colors.

- `labels`:

  `character` vector of labels.

#### Returns

A new `ManualLegend` object.

------------------------------------------------------------------------

### Method `get_resample_method()`

Get resample method.

#### Usage

    ManualLegend$get_resample_method()

#### Returns

`character` object.

------------------------------------------------------------------------

### Method `get_color_map()`

Get a function for mapping values to colors.

#### Usage

    ManualLegend$get_color_map()

#### Returns

A `function` object.

------------------------------------------------------------------------

### Method `get_widget_data()`

Get data for creating a widget.

#### Usage

    ManualLegend$get_widget_data()

#### Returns

A `list` object.

------------------------------------------------------------------------

### Method `export()`

Export settings

#### Usage

    ManualLegend$export()

#### Returns

`list` object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ManualLegend$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
