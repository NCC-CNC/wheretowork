# Include class

Include class

Include class

## Details

Definition for the Include class.

## See also

[`new_include()`](https://ncc-cnc.github.io/wheretowork/reference/new_include.md).

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

  `numeric` date/time.

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

- [`Include$new()`](#method-Include-new)

- [`Include$print()`](#method-Include-print)

- [`Include$repr()`](#method-Include-repr)

- [`Include$get_layer_name()`](#method-Include-get_layer_name)

- [`Include$get_layer_index()`](#method-Include-get_layer_index)

- [`Include$get_layer_pane()`](#method-Include-get_layer_pane)

- [`Include$get_id()`](#method-Include-get_id)

- [`Include$get_visible()`](#method-Include-get_visible)

- [`Include$get_invisible()`](#method-Include-get_invisible)

- [`Include$get_loaded()`](#method-Include-get_loaded)

- [`Include$get_hidden()`](#method-Include-get_hidden)

- [`Include$get_downloadable()`](#method-Include-get_downloadable)

- [`Include$get_status()`](#method-Include-get_status)

- [`Include$get_overlap()`](#method-Include-get_overlap)

- [`Include$get_data()`](#method-Include-get_data)

- [`Include$set_new_pane()`](#method-Include-set_new_pane)

- [`Include$get_setting()`](#method-Include-get_setting)

- [`Include$set_visible()`](#method-Include-set_visible)

- [`Include$set_invisible()`](#method-Include-set_invisible)

- [`Include$set_loaded()`](#method-Include-set_loaded)

- [`Include$set_status()`](#method-Include-set_status)

- [`Include$set_setting()`](#method-Include-set_setting)

- [`Include$get_solution_settings_widget_data()`](#method-Include-get_solution_settings_widget_data)

- [`Include$get_map_manager_widget_data()`](#method-Include-get_map_manager_widget_data)

- [`Include$export()`](#method-Include-export)

- [`Include$render_on_map()`](#method-Include-render_on_map)

- [`Include$update_on_map()`](#method-Include-update_on_map)

- [`Include$clone()`](#method-Include-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a new Include object.

#### Usage

    Include$new(
      id,
      name,
      variable,
      pane,
      invisible,
      loaded,
      mandatory,
      visible,
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

- `invisible`:

  `numeric` date/time value.

- `loaded`:

  `logical` value.

- `mandatory`:

  `logical` value.

- `visible`:

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

A new Include object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    Include$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    Include$repr(start = "[", end = "]")

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

    Include$get_layer_name()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_layer_index()`

Get layer index values.

#### Usage

    Include$get_layer_index()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_layer_pane()`

Get layer pane class.

#### Usage

    Include$get_layer_pane()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_id()`

Get include identifier.

#### Usage

    Include$get_id()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_visible()`

Get visible.

#### Usage

    Include$get_visible()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_invisible()`

Get invisible.

#### Usage

    Include$get_invisible()

#### Returns

`numeric` date/time value.

------------------------------------------------------------------------

### Method `get_loaded()`

Get loaded.

#### Usage

    Include$get_loaded()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_hidden()`

Get hidden.

#### Usage

    Include$get_hidden()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_downloadable()`

Get downloadable.

#### Usage

    Include$get_downloadable()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_status()`

Get status.

#### Usage

    Include$get_status()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_overlap()`

Get overlap.

#### Usage

    Include$get_overlap()

#### Returns

`character` value.

------------------------------------------------------------------------

### Method `get_data()`

Get the data.

#### Usage

    Include$get_data()

#### Returns

[`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)
or
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object.

------------------------------------------------------------------------

### Method `set_new_pane()`

Set new pane.

#### Usage

    Include$set_new_pane(id, index)

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

    Include$get_setting(name)

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

    Include$set_visible(value)

#### Arguments

- `value`:

  `logical` new value.

------------------------------------------------------------------------

### Method `set_invisible()`

Set invisible.

#### Usage

    Include$set_invisible(value)

#### Arguments

- `value`:

  `date/time` value or `NA`.

------------------------------------------------------------------------

### Method `set_loaded()`

Set loaded.

#### Usage

    Include$set_loaded(value)

#### Arguments

- `value`:

  `logical` new value.

------------------------------------------------------------------------

### Method `set_status()`

Set status.

#### Usage

    Include$set_status(value)

#### Arguments

- `value`:

  `logical` new value.

------------------------------------------------------------------------

### Method `set_setting()`

Set setting.

#### Usage

    Include$set_setting(name, value)

#### Arguments

- `name`:

  `character` setting name. Available options are `"status"` or
  \`"visible"“.

- `value`:

  `ANY` new value.

------------------------------------------------------------------------

### Method `get_solution_settings_widget_data()`

Get data for displaying the theme in a
[`solutionSettings()`](https://ncc-cnc.github.io/wheretowork/reference/solutionSettings-widget.md)
widget.

#### Usage

    Include$get_solution_settings_widget_data()

#### Returns

`list` with widget data.

------------------------------------------------------------------------

### Method `get_map_manager_widget_data()`

Get data for displaying the theme in a
[`mapManager()`](https://ncc-cnc.github.io/wheretowork/reference/mapManager-widget.md)
widget.

#### Usage

    Include$get_map_manager_widget_data()

#### Returns

`list` with widget data.

------------------------------------------------------------------------

### Method `export()`

Export settings.

#### Usage

    Include$export()

#### Returns

`list` object.

------------------------------------------------------------------------

### Method `render_on_map()`

Render on map.

#### Usage

    Include$render_on_map(x, zindex)

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

    Include$update_on_map(x, zindex)

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

    Include$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
