# Feature class

Feature class

Feature class

## Details

Definition for the Feature class.

## See also

[`new_feature()`](https://ncc-cnc.github.io/wheretowork/reference/new_feature.md),
[`new_variable()`](https://ncc-cnc.github.io/wheretowork/reference/new_variable.md).

## Public fields

- `id`:

  `character` unique identifier.

- `name`:

  `character` name.

- `variable`:

  [Variable](https://ncc-cnc.github.io/wheretowork/reference/Variable.md)
  object.

- `pane`:

  `character` name.

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

  `numeric` value.

- `goal`:

  `numeric` value.

- `min_goal`:

  `numeric` minimum goal value.

- `max_goal`:

  `numeric` maximum goal value.

- `step_goal`:

  `numeric` step goal value.

- `limit_goal`:

  `numeric` limit goal value.

## Methods

### Public methods

- [`Feature$new()`](#method-Feature-new)

- [`Feature$print()`](#method-Feature-print)

- [`Feature$repr()`](#method-Feature-repr)

- [`Feature$get_layer_name()`](#method-Feature-get_layer_name)

- [`Feature$get_layer_index()`](#method-Feature-get_layer_index)

- [`Feature$get_hidden()`](#method-Feature-get_hidden)

- [`Feature$get_downloadable()`](#method-Feature-get_downloadable)

- [`Feature$get_visible()`](#method-Feature-get_visible)

- [`Feature$get_invisible()`](#method-Feature-get_invisible)

- [`Feature$get_loaded()`](#method-Feature-get_loaded)

- [`Feature$get_current()`](#method-Feature-get_current)

- [`Feature$get_status()`](#method-Feature-get_status)

- [`Feature$get_goal()`](#method-Feature-get_goal)

- [`Feature$get_data()`](#method-Feature-get_data)

- [`Feature$set_new_pane()`](#method-Feature-set_new_pane)

- [`Feature$set_visible()`](#method-Feature-set_visible)

- [`Feature$set_invisible()`](#method-Feature-set_invisible)

- [`Feature$set_loaded()`](#method-Feature-set_loaded)

- [`Feature$set_status()`](#method-Feature-set_status)

- [`Feature$set_goal()`](#method-Feature-set_goal)

- [`Feature$set_current()`](#method-Feature-set_current)

- [`Feature$export()`](#method-Feature-export)

- [`Feature$clone()`](#method-Feature-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a Feature object.

#### Usage

    Feature$new(
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
      goal,
      limit_goal,
      min_goal,
      max_goal,
      step_goal
    )

#### Arguments

- `id`:

  `character` value.

- `name`:

  `character` value.

- `variable`:

  [Variable](https://ncc-cnc.github.io/wheretowork/reference/Variable.md).

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

  `numeric` value.

- `goal`:

  `numeric` value.

- `limit_goal`:

  `numeric` value.

- `min_goal`:

  `numeric` value.

- `max_goal`:

  `numeric` value.

- `step_goal`:

  `numeric` value.

#### Returns

A new Feature object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    Feature$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    Feature$repr(start = "[", end = "]")

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

    Feature$get_layer_name()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_layer_index()`

Get layer index values.

#### Usage

    Feature$get_layer_index()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_hidden()`

Get hidden.

#### Usage

    Feature$get_hidden()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_downloadable()`

Get downloadable.

#### Usage

    Feature$get_downloadable()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_visible()`

Get visible.

#### Usage

    Feature$get_visible()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_invisible()`

Get invisible.

#### Usage

    Feature$get_invisible()

#### Returns

`numeric` date/time value.

------------------------------------------------------------------------

### Method `get_loaded()`

Get loaded.

#### Usage

    Feature$get_loaded()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_current()`

Get current (proportion) coverage.

#### Usage

    Feature$get_current()

#### Returns

`numeric` value.

------------------------------------------------------------------------

### Method `get_status()`

Get status.

#### Usage

    Feature$get_status()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_goal()`

Get goal (proportion) coverage.

#### Usage

    Feature$get_goal()

#### Returns

`numeric` value.

------------------------------------------------------------------------

### Method `get_data()`

Get the data.

#### Usage

    Feature$get_data()

#### Returns

[`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)
or
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object.

------------------------------------------------------------------------

### Method `set_new_pane()`

Set new pane.

#### Usage

    Feature$set_new_pane(id, index)

#### Arguments

- `id`:

  `character` unique identifier.

- `index`:

  `character` variable index.

#### Returns

`character` value.

------------------------------------------------------------------------

### Method `set_visible()`

Set visible.

#### Usage

    Feature$set_visible(value)

#### Arguments

- `value`:

  `logical` new value.

------------------------------------------------------------------------

### Method `set_invisible()`

Set invisible.

#### Usage

    Feature$set_invisible(value)

#### Arguments

- `value`:

  `numeric` date/time value.

------------------------------------------------------------------------

### Method `set_loaded()`

Set loaded.

#### Usage

    Feature$set_loaded(value)

#### Arguments

- `value`:

  `logical` new value.

------------------------------------------------------------------------

### Method `set_status()`

Set status.

#### Usage

    Feature$set_status(value)

#### Arguments

- `value`:

  `logical` new value.

------------------------------------------------------------------------

### Method `set_goal()`

Set goal.

#### Usage

    Feature$set_goal(value)

#### Arguments

- `value`:

  `numeric` new value.

------------------------------------------------------------------------

### Method `set_current()`

Set current.

#### Usage

    Feature$set_current(value)

#### Arguments

- `value`:

  `numeric` new value.

------------------------------------------------------------------------

### Method `export()`

Export settings

#### Usage

    Feature$export()

#### Returns

`list` object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Feature$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
