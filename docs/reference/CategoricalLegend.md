# Categorical legend class

Categorical legend class

Categorical legend class

## Details

Definition for the `CategoricalLegend` class.

## Public fields

- `values`:

  `numeric` vector.

- `colors`:

  `character` vector.

## Methods

### Public methods

- [`CategoricalLegend$new()`](#method-CategoricalLegend-new)

- [`CategoricalLegend$get_widget_data()`](#method-CategoricalLegend-get_widget_data)

- [`CategoricalLegend$get_resample_method()`](#method-CategoricalLegend-get_resample_method)

- [`CategoricalLegend$get_color_map()`](#method-CategoricalLegend-get_color_map)

- [`CategoricalLegend$export()`](#method-CategoricalLegend-export)

- [`CategoricalLegend$clone()`](#method-CategoricalLegend-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a `CategoricalLegend` object.

#### Usage

    CategoricalLegend$new(values, colors)

#### Arguments

- `values`:

  `numeric` value.

- `colors`:

  `character` vector of colors.

#### Returns

A new `CategoricalLegend` object.

------------------------------------------------------------------------

### Method `get_widget_data()`

Get data for creating a widget.

#### Usage

    CategoricalLegend$get_widget_data()

#### Returns

A new `CategoricalLegend` object.

------------------------------------------------------------------------

### Method `get_resample_method()`

Get resample method.

#### Usage

    CategoricalLegend$get_resample_method()

#### Returns

`character` object.

------------------------------------------------------------------------

### Method `get_color_map()`

Get a function for mapping values to colors.

#### Usage

    CategoricalLegend$get_color_map()

#### Returns

A `function` object.

------------------------------------------------------------------------

### Method `export()`

Export settings

#### Usage

    CategoricalLegend$export()

#### Returns

`list` object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CategoricalLegend$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
