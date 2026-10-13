# Map manager class

Map manager class

Map manager class

## Details

Definition for the `MapManager` class.

The argument to `value` should be a `list` with the following elements:

- id:

  `character` (optional) name of layer.

- setting:

  `character` name of setting. Available options are: `"order"`,
  `"feature_order"` and `"visible"`. Note that the `"id"` element is
  required for `"feature_order"` and `"visible"` settings.

&nbsp;

- id:

  `character` (optional) identifier for layer.

- setting:

  `character` name of setting. Available options are: `"order"`,
  "remove"`, `"visible"`, `"feature_order"`, `"feature_visible"`. Note that the `"id"`element is required for`"remove"`, `"visible"`, `"feature_order"`, `"feature_visible"` settings.} \item{value}{`numeric`or`logical\`
  value for new setting.

## Public fields

- `layers`:

  `list` of
  [Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md),
  [Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md),
  [Include](https://ncc-cnc.github.io/wheretowork/reference/Include.md),
  [Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md),
  [Solution](https://ncc-cnc.github.io/wheretowork/reference/Solution.md)
  objects.

- `ids`:

  `character` vector of identifiers for the layers.

- `order`:

  `numeric` vector indicating the order each layer should appear on them
  map. A value of 1 indicates that a layer should appear beneath every
  other layer.

## Methods

### Public methods

- [`MapManager$new()`](#method-MapManager-new)

- [`MapManager$print()`](#method-MapManager-print)

- [`MapManager$repr()`](#method-MapManager-repr)

- [`MapManager$get_order()`](#method-MapManager-get_order)

- [`MapManager$get_layer()`](#method-MapManager-get_layer)

- [`MapManager$get_layer_names()`](#method-MapManager-get_layer_names)

- [`MapManager$get_layer_indices()`](#method-MapManager-get_layer_indices)

- [`MapManager$get_layer_ids()`](#method-MapManager-get_layer_ids)

- [`MapManager$get_layer_panes()`](#method-MapManager-get_layer_panes)

- [`MapManager$get_layer_visible()`](#method-MapManager-get_layer_visible)

- [`MapManager$get_layer_invisible()`](#method-MapManager-get_layer_invisible)

- [`MapManager$get_layer_loaded()`](#method-MapManager-get_layer_loaded)

- [`MapManager$get_setting()`](#method-MapManager-get_setting)

- [`MapManager$set_order()`](#method-MapManager-set_order)

- [`MapManager$set_visible()`](#method-MapManager-set_visible)

- [`MapManager$set_loaded()`](#method-MapManager-set_loaded)

- [`MapManager$set_setting()`](#method-MapManager-set_setting)

- [`MapManager$add_layer()`](#method-MapManager-add_layer)

- [`MapManager$drop_layer()`](#method-MapManager-drop_layer)

- [`MapManager$get_widget_data()`](#method-MapManager-get_widget_data)

- [`MapManager$get_group_layer_ids()`](#method-MapManager-get_group_layer_ids)

- [`MapManager$get_layer_classes()`](#method-MapManager-get_layer_classes)

- [`MapManager$get_lazyload()`](#method-MapManager-get_lazyload)

- [`MapManager$initialize_map()`](#method-MapManager-initialize_map)

- [`MapManager$update_map()`](#method-MapManager-update_map)

- [`MapManager$delete_sinlge_map_pane()`](#method-MapManager-delete_sinlge_map_pane)

- [`MapManager$delete_all_map_panes()`](#method-MapManager-delete_all_map_panes)

- [`MapManager$clone()`](#method-MapManager-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a `MapManager` object.

#### Usage

    MapManager$new(layers, order)

#### Arguments

- `layers`:

  `list` of
  [Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md),
  [Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md),
  [Include](https://ncc-cnc.github.io/wheretowork/reference/Include.md),
  [Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md),
  [Solution](https://ncc-cnc.github.io/wheretowork/reference/Solution.md)
  objects.

- `order`:

  `numeric` vector.

#### Returns

A new `MapManager` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    MapManager$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    MapManager$repr()

#### Returns

`character` value.

------------------------------------------------------------------------

### Method `get_order()`

Get information on the plot order of each layer.

#### Usage

    MapManager$get_order()

------------------------------------------------------------------------

### Method `get_layer()`

Get a layer.

#### Usage

    MapManager$get_layer(value)

#### Arguments

- `value`:

  `character` layer identifier.

#### Returns

[Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md)
object.

------------------------------------------------------------------------

### Method `get_layer_names()`

Get layer names.

#### Usage

    MapManager$get_layer_names(download_only)

#### Arguments

- `download_only`:

  Get only layer names that are flagged as downloadable. Available
  options are `TRUE` or `FALSE`.

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_layer_indices()`

Get layer index values.

#### Usage

    MapManager$get_layer_indices(download_only)

#### Arguments

- `download_only`:

  Get only layer indices that are flagged as downloadable. Available
  options are `TRUE` or `FALSE`.

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_layer_ids()`

Get layer ids values.

#### Usage

    MapManager$get_layer_ids()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_layer_panes()`

Get panes.

#### Usage

    MapManager$get_layer_panes()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_layer_visible()`

Get layer visible values.

#### Usage

    MapManager$get_layer_visible()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_layer_invisible()`

Get layer invisible values.

#### Usage

    MapManager$get_layer_invisible()

#### Returns

`date/time` vector.

------------------------------------------------------------------------

### Method `get_layer_loaded()`

Get layer loaded values.

#### Usage

    MapManager$get_layer_loaded()

#### Returns

`logical` vector.

------------------------------------------------------------------------

### Method `get_setting()`

Get a setting for the object.

#### Usage

    MapManager$get_setting(value)

#### Arguments

- `value`:

  `list` with setting information (see Details section)

------------------------------------------------------------------------

### Method `set_order()`

Set information on the plot order of each layer.

#### Usage

    MapManager$set_order(value)

#### Arguments

- `value`:

  `logical` vector indicating if each layer is visible or not.

------------------------------------------------------------------------

### Method `set_visible()`

Set visibility for all layers.

#### Usage

    MapManager$set_visible(value)

#### Arguments

- `value`:

  `logical` vector indicating if layers should be visible or not.

------------------------------------------------------------------------

### Method `set_loaded()`

Set loaded for all layers.

#### Usage

    MapManager$set_loaded(value)

#### Arguments

- `value`:

  `logical` vector indicating if layers are loaded or not.

------------------------------------------------------------------------

### Method `set_setting()`

Set a setting for the object.

#### Usage

    MapManager$set_setting(value)

#### Arguments

- `value`:

  `list` with new setting information (see Details section)

------------------------------------------------------------------------

### Method `add_layer()`

Add a new layer.

#### Usage

    MapManager$add_layer(value, map)

#### Arguments

- `value`:

  `Layer` object.

- `map`:

  [`leaflet::leafletProxy()`](https://rstudio.github.io/leaflet/reference/leafletProxy.html)
  object.

------------------------------------------------------------------------

### Method `drop_layer()`

Remove a layer.

#### Usage

    MapManager$drop_layer(value, map)

#### Arguments

- `value`:

  `character` layer identifier.

- `map`:

  [`leaflet::leafletProxy()`](https://rstudio.github.io/leaflet/reference/leafletProxy.html)
  object.

------------------------------------------------------------------------

### Method `get_widget_data()`

Get data for displaying the theme in a
[`mapManager()`](https://ncc-cnc.github.io/wheretowork/reference/mapManager-widget.md)
widget.

#### Usage

    MapManager$get_widget_data()

#### Returns

`list` with widget data.

------------------------------------------------------------------------

### Method `get_group_layer_ids()`

Get group layer ids.

#### Usage

    MapManager$get_group_layer_ids()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_layer_classes()`

Get group layer ids.

#### Usage

    MapManager$get_layer_classes()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_lazyload()`

Get data frame of map manager layers.

#### Usage

    MapManager$get_lazyload()

#### Returns

`data.frame` object.

------------------------------------------------------------------------

### Method `initialize_map()`

Initial map by adding data to it.

#### Usage

    MapManager$initialize_map(map)

#### Arguments

- `map`:

  [leaflet::leafletProxy](https://rstudio.github.io/leaflet/reference/leafletProxy.html)
  object.

------------------------------------------------------------------------

### Method `update_map()`

Update map.

#### Usage

    MapManager$update_map(map)

#### Arguments

- `map`:

  [`leaflet::leafletProxy()`](https://rstudio.github.io/leaflet/reference/leafletProxy.html)
  object.

------------------------------------------------------------------------

### Method `delete_sinlge_map_pane()`

Delete single map pane

#### Usage

    MapManager$delete_sinlge_map_pane(map)

#### Arguments

- `map`:

  [`leaflet::leafletProxy()`](https://rstudio.github.io/leaflet/reference/leafletProxy.html)
  object.

------------------------------------------------------------------------

### Method `delete_all_map_panes()`

Delete all map panes.

#### Usage

    MapManager$delete_all_map_panes(map)

#### Arguments

- `map`:

  [`leaflet::leafletProxy()`](https://rstudio.github.io/leaflet/reference/leafletProxy.html)
  object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MapManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
