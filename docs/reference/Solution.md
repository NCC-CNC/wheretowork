# Solution class

Solution class

Solution class

## Details

Definition for the Solution class.

## See also

[`new_solution()`](https://ncc-cnc.github.io/wheretowork/reference/new_solution.md).

## Public fields

- `id`:

  `character` identifier.

- `name`:

  `character` name of solution.

- `visible`:

  `logical` value.

- `invisible`:

  `numeric` date/time value.

- `loaded`:

  `logical` value.

- `variable`:

  [Variable](https://ncc-cnc.github.io/wheretowork/reference/Variable.md)
  object.

- `pane`:

  `character` name.

- `parameters`:

  `list` of
  [Parameter](https://ncc-cnc.github.io/wheretowork/reference/Parameter.md)
  objects

- `statistics`:

  `list` of
  [Statistic](https://ncc-cnc.github.io/wheretowork/reference/Statistic.md)
  objects

- `theme_results`:

  `list` of
  [ThemeResults](https://ncc-cnc.github.io/wheretowork/reference/ThemeResults.md)
  objects.

- `weight_results`:

  `list` of
  [WeightResults](https://ncc-cnc.github.io/wheretowork/reference/WeightResults.md)
  objects.

- `include_results`:

  `list` of
  [IncludeResults](https://ncc-cnc.github.io/wheretowork/reference/IncludeResults.md)
  objects.

- `exclude_results`:

  `list` of
  [ExcludeResults](https://ncc-cnc.github.io/wheretowork/reference/ExcludeResults.md)
  objects.

- `hidden`:

  `logical` value.

- `downloadable`:

  `logical` value.

## Methods

### Public methods

- [`Solution$new()`](#method-Solution-new)

- [`Solution$repr()`](#method-Solution-repr)

- [`Solution$print()`](#method-Solution-print)

- [`Solution$get_layer_name()`](#method-Solution-get_layer_name)

- [`Solution$get_layer_index()`](#method-Solution-get_layer_index)

- [`Solution$get_layer_pane()`](#method-Solution-get_layer_pane)

- [`Solution$get_id()`](#method-Solution-get_id)

- [`Solution$get_visible()`](#method-Solution-get_visible)

- [`Solution$get_invisible()`](#method-Solution-get_invisible)

- [`Solution$get_loaded()`](#method-Solution-get_loaded)

- [`Solution$get_hidden()`](#method-Solution-get_hidden)

- [`Solution$get_downloadable()`](#method-Solution-get_downloadable)

- [`Solution$get_setting()`](#method-Solution-get_setting)

- [`Solution$set_new_pane()`](#method-Solution-set_new_pane)

- [`Solution$set_visible()`](#method-Solution-set_visible)

- [`Solution$set_invisible()`](#method-Solution-set_invisible)

- [`Solution$set_loaded()`](#method-Solution-set_loaded)

- [`Solution$get_summary_results_data()`](#method-Solution-get_summary_results_data)

- [`Solution$get_theme_results_data()`](#method-Solution-get_theme_results_data)

- [`Solution$get_weight_results_data()`](#method-Solution-get_weight_results_data)

- [`Solution$get_include_results_data()`](#method-Solution-get_include_results_data)

- [`Solution$get_exclude_results_data()`](#method-Solution-get_exclude_results_data)

- [`Solution$render_summary_results()`](#method-Solution-render_summary_results)

- [`Solution$render_theme_results()`](#method-Solution-render_theme_results)

- [`Solution$render_weight_results()`](#method-Solution-render_weight_results)

- [`Solution$render_include_results()`](#method-Solution-render_include_results)

- [`Solution$render_exclude_results()`](#method-Solution-render_exclude_results)

- [`Solution$set_setting()`](#method-Solution-set_setting)

- [`Solution$get_solution_results_widget_data()`](#method-Solution-get_solution_results_widget_data)

- [`Solution$get_map_manager_widget_data()`](#method-Solution-get_map_manager_widget_data)

- [`Solution$render_on_map()`](#method-Solution-render_on_map)

- [`Solution$update_on_map()`](#method-Solution-update_on_map)

- [`Solution$clone()`](#method-Solution-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a Solution object.

#### Usage

    Solution$new(
      id,
      name,
      variable,
      pane,
      visible,
      invisible,
      loaded,
      statistics,
      parameters,
      theme_results,
      weight_results,
      include_results,
      exclude_results,
      hidden,
      downloadable
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

- `statistics`:

  `list` of
  [Statistic](https://ncc-cnc.github.io/wheretowork/reference/Statistic.md)
  objects.

- `parameters`:

  `list` of
  [Statistic](https://ncc-cnc.github.io/wheretowork/reference/Statistic.md)
  objects.

- `theme_results`:

  `list` of
  [ThemeResults](https://ncc-cnc.github.io/wheretowork/reference/ThemeResults.md)
  objects.

- `weight_results`:

  `list` of
  [WeightResults](https://ncc-cnc.github.io/wheretowork/reference/WeightResults.md)
  objects.

- `include_results`:

  `list` of
  [IncludeResults](https://ncc-cnc.github.io/wheretowork/reference/IncludeResults.md)
  objects.

- `exclude_results`:

  `list` of
  [ExcludeResults](https://ncc-cnc.github.io/wheretowork/reference/ExcludeResults.md)
  objects.

- `hidden`:

  `logical` value.

- `downloadable`:

  `logical` value.

#### Returns

A new Solution object.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    Solution$repr(start = "[", end = "]")

#### Arguments

- `start`:

  `character` symbol used to start the setting list. Defaults to `"["`.

- `end`:

  `character` symbol used to start the setting list. Defaults to `"]"`.

#### Returns

`character` value.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    Solution$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `get_layer_name()`

Get layer names.

#### Usage

    Solution$get_layer_name()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_layer_index()`

Get layer index values.

#### Usage

    Solution$get_layer_index()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_layer_pane()`

Get layer pane class.

#### Usage

    Solution$get_layer_pane()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_id()`

Get solution identifier.

#### Usage

    Solution$get_id()

#### Returns

`character` vector.

------------------------------------------------------------------------

### Method `get_visible()`

Get visible.

#### Usage

    Solution$get_visible()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_invisible()`

Get invisible.

#### Usage

    Solution$get_invisible()

#### Returns

`numeric` date/time value.

------------------------------------------------------------------------

### Method `get_loaded()`

Get loaded.

#### Usage

    Solution$get_loaded()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_hidden()`

Get hidden.

#### Usage

    Solution$get_hidden()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_downloadable()`

Get downloadable.

#### Usage

    Solution$get_downloadable()

#### Returns

`logical` value.

------------------------------------------------------------------------

### Method `get_setting()`

Get setting.

#### Usage

    Solution$get_setting(name)

#### Arguments

- `name`:

  `character` setting name. Available options are `"visible"`.

#### Returns

Value.

------------------------------------------------------------------------

### Method `set_new_pane()`

Set new pane.

#### Usage

    Solution$set_new_pane(id, index)

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

    Solution$set_visible(value)

#### Arguments

- `value`:

  `logical` new value.

------------------------------------------------------------------------

### Method `set_invisible()`

Set invisible.

#### Usage

    Solution$set_invisible(value)

#### Arguments

- `value`:

  `numeric` date/time value.

------------------------------------------------------------------------

### Method `set_loaded()`

Set loaded.

#### Usage

    Solution$set_loaded(value)

#### Arguments

- `value`:

  `logical` new value.

------------------------------------------------------------------------

### Method `get_summary_results_data()`

Get summary results.

#### Usage

    Solution$get_summary_results_data()

#### Returns

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
object.

------------------------------------------------------------------------

### Method `get_theme_results_data()`

Get theme results.

#### Usage

    Solution$get_theme_results_data()

#### Returns

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
object.

------------------------------------------------------------------------

### Method `get_weight_results_data()`

Get weight results.

#### Usage

    Solution$get_weight_results_data()

#### Returns

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
object.

------------------------------------------------------------------------

### Method `get_include_results_data()`

Get include results.

#### Usage

    Solution$get_include_results_data()

#### Returns

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
object.

------------------------------------------------------------------------

### Method `get_exclude_results_data()`

Get exclude results.

#### Usage

    Solution$get_exclude_results_data()

#### Returns

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
object.

------------------------------------------------------------------------

### Method `render_summary_results()`

Render summary results.

#### Usage

    Solution$render_summary_results()

#### Returns

[`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html) object.

------------------------------------------------------------------------

### Method `render_theme_results()`

Render theme results.

#### Usage

    Solution$render_theme_results()

#### Returns

[`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html) object.

------------------------------------------------------------------------

### Method `render_weight_results()`

Render weight results.

#### Usage

    Solution$render_weight_results()

#### Returns

[`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html) object.

------------------------------------------------------------------------

### Method `render_include_results()`

Render include results.

#### Usage

    Solution$render_include_results()

#### Returns

[`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html) object.

------------------------------------------------------------------------

### Method `render_exclude_results()`

Render exclude results.

#### Usage

    Solution$render_exclude_results()

#### Returns

[`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html) object.

------------------------------------------------------------------------

### Method `set_setting()`

Set setting.

#### Usage

    Solution$set_setting(name, value)

#### Arguments

- `name`:

  `character` setting name. Available options are \`"visible"“.

- `value`:

  `ANY` new value.

------------------------------------------------------------------------

### Method `get_solution_results_widget_data()`

Get data for displaying the theme in a
[`solutionResults()`](https://ncc-cnc.github.io/wheretowork/reference/solutionResults-widget.md)
widget.

#### Usage

    Solution$get_solution_results_widget_data()

#### Returns

`list` with widget data.

------------------------------------------------------------------------

### Method `get_map_manager_widget_data()`

Get data for displaying the theme in a
[`mapManager()`](https://ncc-cnc.github.io/wheretowork/reference/mapManager-widget.md)
widget.

#### Usage

    Solution$get_map_manager_widget_data()

#### Returns

`list` with widget data.

------------------------------------------------------------------------

### Method `render_on_map()`

Render on map.

#### Usage

    Solution$render_on_map(x, zindex)

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

    Solution$update_on_map(x, zindex)

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

    Solution$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
