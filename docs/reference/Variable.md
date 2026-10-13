# Variable class

Variable class

Variable class

## Details

Definition for the Variable class.

## See also

[`new_variable()`](https://ncc-cnc.github.io/wheretowork/reference/new_variable.md).

## Public fields

- `dataset`:

  `Dataset` object.

- `index`:

  `character` value.

- `total`:

  `numeric` value.

- `units`:

  `character` value.

- `legend`:

  `Legend` object.

- `provenance`:

  [Provenance](https://ncc-cnc.github.io/wheretowork/reference/Provenance.md)
  object.

## Methods

### Public methods

- [`Variable$new()`](#method-Variable-new)

- [`Variable$print()`](#method-Variable-print)

- [`Variable$repr()`](#method-Variable-repr)

- [`Variable$verify()`](#method-Variable-verify)

- [`Variable$get_data()`](#method-Variable-get_data)

- [`Variable$export()`](#method-Variable-export)

- [`Variable$render()`](#method-Variable-render)

- [`Variable$update_render()`](#method-Variable-update_render)

- [`Variable$clone()`](#method-Variable-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a Variable object.

#### Usage

    Variable$new(dataset, index, total, units, legend, provenance)

#### Arguments

- `dataset`:

  `Dataset` value.

- `index`:

  `character` or `integer` value.

- `total`:

  `numeric` value.

- `units`:

  `character` value.

- `legend`:

  `Legend` object.

- `provenance`:

  [Provenance](https://ncc-cnc.github.io/wheretowork/reference/Provenance.md)
  object.

#### Returns

A new Variable object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    Variable$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    Variable$repr(start = "[", end = "]")

#### Arguments

- `start`:

  `character` symbol used to start the setting list. Defaults to `"["`.

- `end`:

  `character` symbol used to start the setting list. Defaults to `"]"`.

#### Returns

`character` value.

------------------------------------------------------------------------

### Method `verify()`

Verify that the data can be extracted from the dataset.

#### Usage

    Variable$verify()

#### Returns

invisible `TRUE` indicating success.

------------------------------------------------------------------------

### Method `get_data()`

Get the data.

#### Usage

    Variable$get_data()

#### Returns

[`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)
or
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object.

------------------------------------------------------------------------

### Method `export()`

Export settings

#### Usage

    Variable$export()

#### Returns

`list` object.

------------------------------------------------------------------------

### Method `render()`

Render variable on map.

#### Usage

    Variable$render(x, id, zindex, visible)

#### Arguments

- `x`:

  [`leaflet::leaflet()`](https://rstudio.github.io/leaflet/reference/leaflet.html)
  object.

- `id`:

  `character` identifier for map pane.

- `zindex`:

  `numeric` z-index for ordering.

- `visible`:

  `logical` should the variable be visible?

#### Returns

[`leaflet::leaflet()`](https://rstudio.github.io/leaflet/reference/leaflet.html)
object.

------------------------------------------------------------------------

### Method `update_render()`

Update rendering of variable on map.

#### Usage

    Variable$update_render(x, id, zindex, visible)

#### Arguments

- `x`:

  [`leaflet::leafletProxy()`](https://rstudio.github.io/leaflet/reference/leafletProxy.html)
  object.

- `id`:

  `character` identifier for map pane.

- `zindex`:

  `numeric` z-index for ordering.

- `visible`:

  `logical` should the variable be visible?

#### Returns

[`leaflet::leaflet()`](https://rstudio.github.io/leaflet/reference/leaflet.html)
object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Variable$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
