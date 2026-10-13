# Theme class

Theme class

Theme class

## Details

Definition for the Theme class.

## See also

[`new_theme()`](https://ncc-cnc.github.io/wheretowork/reference/new_theme.md).

## Public fields

- `id`:

  `character` value.

- `name`:

  `character` value.

- `feature`:

  `list` of
  [Feature](https://ncc-cnc.github.io/wheretowork/reference/Feature.md)
  objects.

- `feature_order`:

  `numeric` value.

## Methods

### Public methods

- [`Theme$new()`](#method-Theme-new)

- [`Theme$print()`](#method-Theme-print)

- [`Theme$repr()`](#method-Theme-repr)

- [`Theme$get_layer_name()`](#method-Theme-get_layer_name)

- [`Theme$get_layer_pane()`](#method-Theme-get_layer_pane)

- [`Theme$get_layer_index()`](#method-Theme-get_layer_index)

- [`Theme$get_feature_id()`](#method-Theme-get_feature_id)

- [`Theme$get_feature_name()`](#method-Theme-get_feature_name)

- [`Theme$get_feature_current()`](#method-Theme-get_feature_current)

- [`Theme$get_feature_limit()`](#method-Theme-get_feature_limit)

- [`Theme$get_feature_total()`](#method-Theme-get_feature_total)

- [`Theme$get_visible()`](#method-Theme-get_visible)

- [`Theme$get_feature_visible()`](#method-Theme-get_feature_visible)

- [`Theme$get_feature_invisible()`](#method-Theme-get_feature_invisible)

- [`Theme$get_feature_loaded()`](#method-Theme-get_feature_loaded)

- [`Theme$get_feature_hidden()`](#method-Theme-get_feature_hidden)

- [`Theme$get_feature_downloadable()`](#method-Theme-get_feature_downloadable)

- [`Theme$get_feature_status()`](#method-Theme-get_feature_status)

- [`Theme$get_feature_goal()`](#method-Theme-get_feature_goal)

- [`Theme$get_feature_order()`](#method-Theme-get_feature_order)

- [`Theme$set_feature_order()`](#method-Theme-set_feature_order)

- [`Theme$get_setting()`](#method-Theme-get_setting)

- [`Theme$set_visible()`](#method-Theme-set_visible)

- [`Theme$set_feature_visible()`](#method-Theme-set_feature_visible)

- [`Theme$set_feature_status()`](#method-Theme-set_feature_status)

- [`Theme$set_feature_goal()`](#method-Theme-set_feature_goal)

- [`Theme$set_feature_current()`](#method-Theme-set_feature_current)

- [`Theme$set_setting()`](#method-Theme-set_setting)

- [`Theme$get_solution_settings_widget_data()`](#method-Theme-get_solution_settings_widget_data)

- [`Theme$get_map_manager_widget_data()`](#method-Theme-get_map_manager_widget_data)

- [`Theme$render_on_map()`](#method-Theme-render_on_map)

- [`Theme$update_on_map()`](#method-Theme-update_on_map)

- [`Theme$export()`](#method-Theme-export)

- [`Theme$clone()`](#method-Theme-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a Theme object.

#### Usage

    Theme$new(id, name, feature, feature_order)

#### Arguments

- `id`:

  `character` value.

- `name`:

  `character` value.

- `feature`:

  `list` of
  [Feature](https://ncc-cnc.github.io/wheretowork/reference/Feature.md)
  objects.

- `feature_order`:

  `numeric` vector.

#### Returns

A new Theme object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    Theme$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    Theme$repr(...)

#### Arguments

- `...`:

  not used.

#### Returns

`character` value.

------------------------------------------------------------------------

### Method `get_layer_name()`

Get layer names.

#### Usage

    Theme$get_layer_name()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_layer_pane()`

Get layer map pane classes.

#### Usage

    Theme$get_layer_pane()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_layer_index()`

Get layer index values.

#### Usage

    Theme$get_layer_index()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_feature_id()`

Get feature identifiers.

#### Usage

    Theme$get_feature_id()

#### Returns

`character` vector with identifier(s).

------------------------------------------------------------------------

### Method `get_feature_name()`

Get feature names.

#### Usage

    Theme$get_feature_name()

#### Returns

`character` vector with identifier(s).

------------------------------------------------------------------------

### Method `get_feature_current()`

Get feature current.

#### Usage

    Theme$get_feature_current()

#### Returns

`numeric` vector with value(s).

------------------------------------------------------------------------

### Method `get_feature_limit()`

Get feature limit.

#### Usage

    Theme$get_feature_limit()

#### Returns

`numeric` vector with value(s).

------------------------------------------------------------------------

### Method `get_feature_total()`

Get feature current.

#### Usage

    Theme$get_feature_total()

#### Returns

`numeric` vector with value(s).

------------------------------------------------------------------------

### Method `get_visible()`

Get visible value for all features.

#### Usage

    Theme$get_visible()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_feature_visible()`

Get feature visible values.

#### Usage

    Theme$get_feature_visible()

#### Returns

`logical` vector with status value(s).

------------------------------------------------------------------------

### Method `get_feature_invisible()`

Get feature invisible values.

#### Usage

    Theme$get_feature_invisible()

#### Returns

`numeric` vector with date/time value(s).

------------------------------------------------------------------------

### Method `get_feature_loaded()`

Get feature loaded values.

#### Usage

    Theme$get_feature_loaded()

#### Returns

`logical` vector with loaded value(s).

------------------------------------------------------------------------

### Method `get_feature_hidden()`

Get feature hidden values.

#### Usage

    Theme$get_feature_hidden()

#### Returns

`logical` vector with status value(s).

------------------------------------------------------------------------

### Method `get_feature_downloadable()`

Get feature downloadable values.

#### Usage

    Theme$get_feature_downloadable()

#### Returns

`logical` vector with status value(s).

------------------------------------------------------------------------

### Method `get_feature_status()`

Get feature status values.

#### Usage

    Theme$get_feature_status()

#### Returns

`logical` vector with status value(s).

------------------------------------------------------------------------

### Method `get_feature_goal()`

Get feature goal values.

#### Usage

    Theme$get_feature_goal()

#### Returns

`numeric` vector with goal value(s).

------------------------------------------------------------------------

### Method `get_feature_order()`

Set relative order for displaying features on a map.

#### Usage

    Theme$get_feature_order()

------------------------------------------------------------------------

### Method `set_feature_order()`

Set relative order for displaying features on a map.

#### Usage

    Theme$set_feature_order(value)

#### Arguments

- `value`:

  `numeric` vector of new orders.

------------------------------------------------------------------------

### Method `get_setting()`

Get setting.

#### Usage

    Theme$get_setting(name)

#### Arguments

- `name`:

  `character` setting name. Available options are `"feature_status"`,
  `"feature_goal"`, `"feature_visible"`, `"visible"`, or
  `"feature_order"`.

#### Returns

Value.

------------------------------------------------------------------------

### Method `set_visible()`

Set visible value for all features.

#### Usage

    Theme$set_visible(value)

#### Arguments

- `value`:

  `logical` value.

------------------------------------------------------------------------

### Method `set_feature_visible()`

Set feature visible values.

#### Usage

    Theme$set_feature_visible(value)

#### Arguments

- `value`:

  `logical` vector containing a value for each feature. A `list` of
  `logical` values can also be supplied.

------------------------------------------------------------------------

### Method `set_feature_status()`

Set feature status values.

#### Usage

    Theme$set_feature_status(value)

#### Arguments

- `value`:

  `logical` vector containing a value for each feature. A `list` of
  `logical` values can also be supplied.

------------------------------------------------------------------------

### Method `set_feature_goal()`

Set feature goal values.

#### Usage

    Theme$set_feature_goal(value)

#### Arguments

- `value`:

  `numeric` vector containing a value for each feature. A `list` of
  `numeric` values can also be supplied.

------------------------------------------------------------------------

### Method `set_feature_current()`

Set feature current values.

#### Usage

    Theme$set_feature_current(value)

#### Arguments

- `value`:

  `numeric` vector containing a value for each feature. A `list` of
  `numeric` values can also be supplied.

------------------------------------------------------------------------

### Method `set_setting()`

Set setting.

#### Usage

    Theme$set_setting(name, value)

#### Arguments

- `name`:

  `character` setting name. Available options are `"feature_status"`,
  `"feature_goal"`, `"feature_visible"`, `"visible"`, `"feature_order"`,
  or `"feature_current"`.

- `value`:

  vector containing a value for each feature.

------------------------------------------------------------------------

### Method `get_solution_settings_widget_data()`

Get data for displaying the theme in a
[`solutionSettings()`](https://ncc-cnc.github.io/wheretowork/reference/solutionSettings-widget.md)
widget.

#### Usage

    Theme$get_solution_settings_widget_data()

#### Returns

`list` with widget data.

------------------------------------------------------------------------

### Method `get_map_manager_widget_data()`

Get data for displaying the theme in a
[`mapManager()`](https://ncc-cnc.github.io/wheretowork/reference/mapManager-widget.md)
widget.

#### Usage

    Theme$get_map_manager_widget_data()

#### Returns

`list` with widget data.

------------------------------------------------------------------------

### Method `render_on_map()`

Render on map.

#### Usage

    Theme$render_on_map(x, zindex)

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

    Theme$update_on_map(x, zindex)

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

### Method `export()`

Export settings

#### Usage

    Theme$export()

#### Returns

`list` object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Theme$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
