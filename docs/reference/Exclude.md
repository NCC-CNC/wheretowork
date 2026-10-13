# Exclude class

Exclude class

Exclude class

## Details

Definition for the Exclude class.

## See also

[`new_exclude()`](https://ncc-cnc.github.io/wheretowork/reference/new_exclude.md).

## Public fields

- `id`:

  `character` value.

- `name`:

  `character` value.

- `variable`:

  [Variable](https://ncc-cnc.github.io/wheretowork/reference/Variable.md)
  object.

- `pane`:

  `character` name.

- `mandatory`:

  `logical` value.

- `visible`:

  `logical` value.

- `invisible`:

  `numeric` date/time value.

- `loaded`:

  `logical` value.

- `hidden`:

  `logical` value.

- `downloadable`:

  `logical` value.

- `status`:

  `logical` value.

- `overlap`:

  `character` vector.

## Methods

### Public methods

- [`Exclude$new()`](#method-Exclude-new)

- [`Exclude$print()`](#method-Exclude-print)

- [`Exclude$repr()`](#method-Exclude-repr)

- [`Exclude$get_layer_name()`](#method-Exclude-get_layer_name)

- [`Exclude$get_layer_index()`](#method-Exclude-get_layer_index)

- [`Exclude$get_layer_pane()`](#method-Exclude-get_layer_pane)

- [`Exclude$get_id()`](#method-Exclude-get_id)

- [`Exclude$get_visible()`](#method-Exclude-get_visible)

- [`Exclude$get_invisible()`](#method-Exclude-get_invisible)

- [`Exclude$get_loaded()`](#method-Exclude-get_loaded)

- [`Exclude$get_hidden()`](#method-Exclude-get_hidden)

- [`Exclude$get_downloadable()`](#method-Exclude-get_downloadable)

- [`Exclude$get_status()`](#method-Exclude-get_status)

- [`Exclude$get_overlap()`](#method-Exclude-get_overlap)

- [`Exclude$get_data()`](#method-Exclude-get_data)

- [`Exclude$set_new_pane()`](#method-Exclude-set_new_pane)

- [`Exclude$get_setting()`](#method-Exclude-get_setting)

- [`Exclude$set_visible()`](#method-Exclude-set_visible)

- [`Exclude$set_invisible()`](#method-Exclude-set_invisible)

- [`Exclude$set_loaded()`](#method-Exclude-set_loaded)

- [`Exclude$set_status()`](#method-Exclude-set_status)

- [`Exclude$set_setting()`](#method-Exclude-set_setting)

- [`Exclude$get_solution_settings_widget_data()`](#method-Exclude-get_solution_settings_widget_data)

- [`Exclude$get_map_manager_widget_data()`](#method-Exclude-get_map_manager_widget_data)

- [`Exclude$export()`](#method-Exclude-export)

- [`Exclude$render_on_map()`](#method-Exclude-render_on_map)

- [`Exclude$update_on_map()`](#method-Exclude-update_on_map)

- [`Exclude$clone()`](#method-Exclude-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a new Exclude object.

#### Usage

    Exclude$new(
      id,
      name,
      variable,
      pane,
      mandatory,
      visible,
      invisible,
      loaded,
      hidden,
      downloadable,
      status,
      overlap
    )

#### Arguments

- `id`:

  `character` value.

- `name`:

  `character` value.

- `variable`:

  [Variable](https://ncc-cnc.github.io/wheretowork/reference/Variable.md)
  object.

- `pane`:

  `character` value.

- `mandatory`:

  `logical` value.

- `visible`:

  `logical` value.

- `invisible`:

  `numeric` date/time value.

- `loaded`:

  `logical` value.

- `hidden`:

  `logical` value.

- `downloadable`:

  `logical` value.

- `status`:

  `logical` value.

- `overlap`:

  `character` vector.

#### Returns

A new Exclude object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    Exclude$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    Exclude$repr(start = "[", end = "]")

#### Arguments

- `start`:

  `character` symbol used to start the setting list. Defaults to `"["`.

- `end`:

  `character` symbol used to start the setting list. Defaults to `"]"`.

#### Returns

`character` value.

------------------------------------------------------------------------

### Method `get_layer_name()`

Get layer names.

#### Usage

    Exclude$get_layer_name()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_layer_index()`

Get layer index values.

#### Usage

    Exclude$get_layer_index()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_layer_pane()`

Get layer pane class.

#### Usage

    Exclude$get_layer_pane()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_id()`

Get exclude identifier.

#### Usage

    Exclude$get_id()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_visible()`

Get visible.

#### Usage

    Exclude$get_visible()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_invisible()`

Get invisible.

#### Usage

    Exclude$get_invisible()

#### Returns

`numeric` date/time value.

------------------------------------------------------------------------

### Method `get_loaded()`

Get loaded.

#### Usage

    Exclude$get_loaded()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_hidden()`

Get hidden.

#### Usage

    Exclude$get_hidden()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_downloadable()`

Get downloadable.

#### Usage

    Exclude$get_downloadable()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_status()`

Get status.

#### Usage

    Exclude$get_status()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_overlap()`

Get overlap.

#### Usage

    Exclude$get_overlap()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_data()`

Get the data.

#### Usage

    Exclude$get_data()

#### Returns

[`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)
or
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object.

------------------------------------------------------------------------

### Method `set_new_pane()`

Set new pane.

#### Usage

    Exclude$set_new_pane(id, index)

#### Arguments

- `id`:

  `character` unique identifier.

- `index`:

  `character` variable index.

#### Returns

`character` value.

------------------------------------------------------------------------

### Method `get_setting()`

Get setting.

#### Usage

    Exclude$get_setting(name)

#### Arguments

- `name`:

  `character` setting name. Available options are `"status"` or
  `"visible"`.

#### Returns

Value.

------------------------------------------------------------------------

### Method `set_visible()`

Set visible.

#### Usage

    Exclude$set_visible(value)

#### Arguments

- `value`:

  `logical` new value.

------------------------------------------------------------------------

### Method `set_invisible()`

Set invisible.

#### Usage

    Exclude$set_invisible(value)

#### Arguments

- `value`:

  `numeric` date/time value.

------------------------------------------------------------------------

### Method `set_loaded()`

Set loaded.

#### Usage

    Exclude$set_loaded(value)

#### Arguments

- `value`:

  `logical` new value.

------------------------------------------------------------------------

### Method `set_status()`

Set status.

#### Usage

    Exclude$set_status(value)

#### Arguments

- `value`:

  `logical` new value.

------------------------------------------------------------------------

### Method `set_setting()`

Set setting.

#### Usage

    Exclude$set_setting(name, value)

#### Arguments

- `name`:

  `character` setting name. Available options are `"status"` or
  \`"visible"“.

- `value`:

  `ANY` new value.

------------------------------------------------------------------------

### Method `get_solution_settings_widget_data()`

Get data for displaying the exclude in a
[`solutionSettings()`](https://ncc-cnc.github.io/wheretowork/reference/solutionSettings-widget.md)
widget.

#### Usage

    Exclude$get_solution_settings_widget_data()

#### Returns

`list` with widget data.

------------------------------------------------------------------------

### Method `get_map_manager_widget_data()`

Get data for displaying the exclude in a
[`mapManager()`](https://ncc-cnc.github.io/wheretowork/reference/mapManager-widget.md)
widget.

#### Usage

    Exclude$get_map_manager_widget_data()

#### Returns

`list` with widget data.

------------------------------------------------------------------------

### Method `export()`

Export settings.

#### Usage

    Exclude$export()

#### Returns

`list` object.

------------------------------------------------------------------------

### Method `render_on_map()`

Render on map.

#### Usage

    Exclude$render_on_map(x, zindex)

#### Arguments

- `x`:

  [`leaflet::leaflet()`](https://rstudio.github.io/leaflet/reference/leaflet.html)
  object.

- `zindex`:

  `numeric` z-index for ordering.

#### Returns

[`leaflet::leaflet()`](https://rstudio.github.io/leaflet/reference/leaflet.html)
object.

------------------------------------------------------------------------

### Method `update_on_map()`

Render on map.

#### Usage

    Exclude$update_on_map(x, zindex)

#### Arguments

- `x`:

  [`leaflet::leafletProxy()`](https://rstudio.github.io/leaflet/reference/leafletProxy.html)
  object.

- `zindex`:

  `numeric` z-index for ordering.

#### Returns

[`leaflet::leafletProxy()`](https://rstudio.github.io/leaflet/reference/leafletProxy.html)
object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Exclude$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
