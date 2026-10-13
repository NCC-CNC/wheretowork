# Weight class

Weight class

Weight class

## Details

Definition for the Weight class.

## See also

[`new_weight()`](https://ncc-cnc.github.io/wheretowork/reference/new_weight.md).

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

- `visible`:

  `logical` value.

- `invisible`:

  `numeric` date/time.

- `loaded`:

  `logical` value.

- `hidden`:

  `logical` value.

- `downloadable`:

  `logical` value.

- `status`:

  `logical` value.

- `current`:

  `numeric` value.

- `factor`:

  `numeric` value.

- `min_factor`:

  `numeric` minimum factor value.

- `max_factor`:

  `numeric` maximum factor value.

- `step_factor`:

  `numeric` step factor value.

## Methods

### Public methods

- [`Weight$new()`](#method-Weight-new)

- [`Weight$print()`](#method-Weight-print)

- [`Weight$repr()`](#method-Weight-repr)

- [`Weight$get_layer_name()`](#method-Weight-get_layer_name)

- [`Weight$get_layer_index()`](#method-Weight-get_layer_index)

- [`Weight$get_layer_pane()`](#method-Weight-get_layer_pane)

- [`Weight$get_id()`](#method-Weight-get_id)

- [`Weight$get_visible()`](#method-Weight-get_visible)

- [`Weight$get_invisible()`](#method-Weight-get_invisible)

- [`Weight$get_loaded()`](#method-Weight-get_loaded)

- [`Weight$get_hidden()`](#method-Weight-get_hidden)

- [`Weight$get_downloadable()`](#method-Weight-get_downloadable)

- [`Weight$get_status()`](#method-Weight-get_status)

- [`Weight$get_current()`](#method-Weight-get_current)

- [`Weight$get_factor()`](#method-Weight-get_factor)

- [`Weight$get_data()`](#method-Weight-get_data)

- [`Weight$set_new_pane()`](#method-Weight-set_new_pane)

- [`Weight$get_setting()`](#method-Weight-get_setting)

- [`Weight$set_visible()`](#method-Weight-set_visible)

- [`Weight$set_invisible()`](#method-Weight-set_invisible)

- [`Weight$set_loaded()`](#method-Weight-set_loaded)

- [`Weight$set_status()`](#method-Weight-set_status)

- [`Weight$set_factor()`](#method-Weight-set_factor)

- [`Weight$set_current()`](#method-Weight-set_current)

- [`Weight$set_setting()`](#method-Weight-set_setting)

- [`Weight$get_solution_settings_widget_data()`](#method-Weight-get_solution_settings_widget_data)

- [`Weight$get_map_manager_widget_data()`](#method-Weight-get_map_manager_widget_data)

- [`Weight$export()`](#method-Weight-export)

- [`Weight$render_on_map()`](#method-Weight-render_on_map)

- [`Weight$update_on_map()`](#method-Weight-update_on_map)

- [`Weight$clone()`](#method-Weight-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a new Weight object.

#### Usage

    Weight$new(
      id,
      name,
      variable,
      pane,
      visible,
      invisible,
      loaded,
      hidden,
      downloadable,
      status,
      current,
      factor,
      min_factor,
      max_factor,
      step_factor
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

- `current`:

  `logical` value.

- `factor`:

  `numeric` initial factor value.

- `min_factor`:

  `numeric` minimum factor value.

- `max_factor`:

  `numeric` maximum factor value.

- `step_factor`:

  `numeric` step factor value.

#### Returns

A new Weight object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    Weight$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    Weight$repr(start = "[", end = "]")

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

    Weight$get_layer_name()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_layer_index()`

Get layer index values.

#### Usage

    Weight$get_layer_index()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_layer_pane()`

Get layer pane class.

#### Usage

    Weight$get_layer_pane()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_id()`

Get weight identifier.

#### Usage

    Weight$get_id()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_visible()`

Get visible.

#### Usage

    Weight$get_visible()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_invisible()`

Get invisible.

#### Usage

    Weight$get_invisible()

#### Returns

`numeric` date/time value.

------------------------------------------------------------------------

### Method `get_loaded()`

Get loaded.

#### Usage

    Weight$get_loaded()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_hidden()`

Get hidden.

#### Usage

    Weight$get_hidden()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_downloadable()`

Get downloadable.

#### Usage

    Weight$get_downloadable()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_status()`

Get status.

#### Usage

    Weight$get_status()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_current()`

Get current.

#### Usage

    Weight$get_current()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_factor()`

Get factor.

#### Usage

    Weight$get_factor()

#### Returns

`numeric` value.

------------------------------------------------------------------------

### Method `get_data()`

Get the data.

#### Usage

    Weight$get_data()

#### Returns

[`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)
or
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object.

------------------------------------------------------------------------

### Method `set_new_pane()`

Set new pane.

#### Usage

    Weight$set_new_pane(id, index)

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

    Weight$get_setting(name)

#### Arguments

- `name`:

  `character` setting name. Available options are `"status"` `"factor"`,
  `"current"`, or `"visible"`.

#### Returns

Value.

------------------------------------------------------------------------

### Method `set_visible()`

Set visible.

#### Usage

    Weight$set_visible(value)

#### Arguments

- `value`:

  `logical` new value.

------------------------------------------------------------------------

### Method `set_invisible()`

Set invisible.

#### Usage

    Weight$set_invisible(value)

#### Arguments

- `value`:

  `numeric` date/time value.

------------------------------------------------------------------------

### Method `set_loaded()`

Set loaded.

#### Usage

    Weight$set_loaded(value)

#### Arguments

- `value`:

  `logical` new value.

------------------------------------------------------------------------

### Method `set_status()`

Set status.

#### Usage

    Weight$set_status(value)

#### Arguments

- `value`:

  `logical` new value.

------------------------------------------------------------------------

### Method `set_factor()`

Set factor.

#### Usage

    Weight$set_factor(value)

#### Arguments

- `value`:

  `numeric` new value.

------------------------------------------------------------------------

### Method `set_current()`

Set current.

#### Usage

    Weight$set_current(value)

#### Arguments

- `value`:

  `numeric` new value.

------------------------------------------------------------------------

### Method `set_setting()`

Set setting.

#### Usage

    Weight$set_setting(name, value)

#### Arguments

- `name`:

  `character` setting name. Available options are `"status"` `"factor"`,
  `"current"`, or \`"visible"“.

- `value`:

  `ANY` new value.

------------------------------------------------------------------------

### Method `get_solution_settings_widget_data()`

Get data for displaying the theme in a
[`solutionSettings()`](https://ncc-cnc.github.io/wheretowork/reference/solutionSettings-widget.md)
widget.

#### Usage

    Weight$get_solution_settings_widget_data()

#### Returns

`list` with widget data.

------------------------------------------------------------------------

### Method `get_map_manager_widget_data()`

Get data for displaying the theme in a
[`mapManager()`](https://ncc-cnc.github.io/wheretowork/reference/mapManager-widget.md)
widget.

#### Usage

    Weight$get_map_manager_widget_data()

#### Returns

`list` with widget data.

------------------------------------------------------------------------

### Method `export()`

Export settings.

#### Usage

    Weight$export()

#### Returns

`list` object.

------------------------------------------------------------------------

### Method `render_on_map()`

Render on map.

#### Usage

    Weight$render_on_map(x, zindex)

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

    Weight$update_on_map(x, zindex)

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

    Weight$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
