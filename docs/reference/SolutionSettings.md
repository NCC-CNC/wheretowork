# Solution settings class

Solution settings class

Solution settings class

## Details

Definition for the `SolutionSettings` class.

`user_settings` stores user uploaded .yaml file used to repopulate
solution settings from a previous optimization run. The user uploaded
.yaml file must completely match the current project.

The argument to `value` should be a `list` with the following elements:

- id:

  `character` identifier for theme, weight, include, or exclude.

- setting:

  `character` name of parameter. Available options are: `"status"`,
  `"factor"`, `"value"`, or `"goal"`.

- type:

  `character` indicating the type of setting. Available options are:
  `"theme"`, `"weight"`, `"include"`, `"exclude"` `"parameter"`.

The argument to `value` should be a `list` with the following elements:

- id:

  `character` identifier for theme, weight, include, exclude or
  parameter.

- setting:

  `character` name of parameter. Available options are: `"status"`,
  `"factor"`, `"value"`, `"goal"`, or `"fileinput"`

- value:

  `numeric`, `logical`, or `character` value for new setting.

- type:

  `character` indicating the type of setting. Available options are:
  `"theme"`, `"weight"`, `"include"`, `"exclude"`, `"parameter"`.

The argument to `value` should be a `list` of
[Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md),
[Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md),
[Include](https://ncc-cnc.github.io/wheretowork/reference/Include.md),
[Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md)
and
[Parameter](https://ncc-cnc.github.io/wheretowork/reference/Parameter.md)
objects.

## Public fields

- `theme_ids`:

  `character` vector of identifiers for the themes.

- `weight_ids`:

  `character` vector of identifiers for the weights.

- `include_ids`:

  `character` vector of identifiers for the includes.

- `exclude_ids`:

  `character` vector of identifiers for the excludes.

- `parameter_ids`:

  `character` vector of identifiers for the parameters.

- `themes`:

  `list` of
  [Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md)
  objects.

- `weights`:

  `list` of
  [Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md)
  objects.

- `includes`:

  `list` of
  [Include](https://ncc-cnc.github.io/wheretowork/reference/Include.md)
  objects.

- `excludes`:

  `list` of
  [Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md)
  objects.

- `parameters`:

  `list` of
  [Parameter](https://ncc-cnc.github.io/wheretowork/reference/Parameter.md)
  objects.

- `user_settings`:

  `list` of
  [Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md),
  [Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md),
  [Include](https://ncc-cnc.github.io/wheretowork/reference/Include.md),
  [Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md)
  and
  [Parameter](https://ncc-cnc.github.io/wheretowork/reference/Parameter.md)
  objects.

## Methods

### Public methods

- [`SolutionSettings$new()`](#method-SolutionSettings-new)

- [`SolutionSettings$print()`](#method-SolutionSettings-print)

- [`SolutionSettings$repr()`](#method-SolutionSettings-repr)

- [`SolutionSettings$get_theme()`](#method-SolutionSettings-get_theme)

- [`SolutionSettings$get_weight()`](#method-SolutionSettings-get_weight)

- [`SolutionSettings$get_include()`](#method-SolutionSettings-get_include)

- [`SolutionSettings$get_exclude()`](#method-SolutionSettings-get_exclude)

- [`SolutionSettings$get_parameter()`](#method-SolutionSettings-get_parameter)

- [`SolutionSettings$get_setting()`](#method-SolutionSettings-get_setting)

- [`SolutionSettings$set_setting()`](#method-SolutionSettings-set_setting)

- [`SolutionSettings$get_feature_names()`](#method-SolutionSettings-get_feature_names)

- [`SolutionSettings$update_ss()`](#method-SolutionSettings-update_ss)

- [`SolutionSettings$get_widget_data()`](#method-SolutionSettings-get_widget_data)

- [`SolutionSettings$get_theme_settings()`](#method-SolutionSettings-get_theme_settings)

- [`SolutionSettings$get_weight_settings()`](#method-SolutionSettings-get_weight_settings)

- [`SolutionSettings$get_include_settings()`](#method-SolutionSettings-get_include_settings)

- [`SolutionSettings$get_exclude_settings()`](#method-SolutionSettings-get_exclude_settings)

- [`SolutionSettings$get_parameter_settings()`](#method-SolutionSettings-get_parameter_settings)

- [`SolutionSettings$get_theme_data()`](#method-SolutionSettings-get_theme_data)

- [`SolutionSettings$get_weight_data()`](#method-SolutionSettings-get_weight_data)

- [`SolutionSettings$get_include_data()`](#method-SolutionSettings-get_include_data)

- [`SolutionSettings$get_exclude_data()`](#method-SolutionSettings-get_exclude_data)

- [`SolutionSettings$get_overlap()`](#method-SolutionSettings-get_overlap)

- [`SolutionSettings$set_overlap()`](#method-SolutionSettings-set_overlap)

- [`SolutionSettings$update_current_held()`](#method-SolutionSettings-update_current_held)

- [`SolutionSettings$clone()`](#method-SolutionSettings-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a `SolutionSettings` object.

#### Usage

    SolutionSettings$new(themes, weights, includes, excludes, parameters)

#### Arguments

- `themes`:

  `list` of
  [Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md)
  objects.

- `weights`:

  `list` of
  [Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md)
  objects.

- `includes`:

  `list` of
  [Include](https://ncc-cnc.github.io/wheretowork/reference/Include.md)
  objects.

- `excludes`:

  `list` of
  [Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md)
  objects.

- `parameters`:

  `list` of
  [Parameter](https://ncc-cnc.github.io/wheretowork/reference/Parameter.md)
  objects.

- `user_settings`:

  `list` of `list` of
  [Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md),
  [Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md),
  [Include](https://ncc-cnc.github.io/wheretowork/reference/Include.md),
  [Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md)
  and
  [Parameter](https://ncc-cnc.github.io/wheretowork/reference/Parameter.md)
  objects (see Details section).

#### Returns

A new `SolutionSettings` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    SolutionSettings$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    SolutionSettings$repr()

#### Returns

`character` value.

------------------------------------------------------------------------

### Method `get_theme()`

Get a theme.

#### Usage

    SolutionSettings$get_theme(value)

#### Arguments

- `value`:

  `character` theme identifier.

#### Returns

[Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md)
object.

------------------------------------------------------------------------

### Method `get_weight()`

Get a weight.

#### Usage

    SolutionSettings$get_weight(value)

#### Arguments

- `value`:

  `character` weight identifier.

#### Returns

[Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md)
object.

------------------------------------------------------------------------

### Method `get_include()`

Get an include.

#### Usage

    SolutionSettings$get_include(value)

#### Arguments

- `value`:

  `character` include identifier.

#### Returns

[Include](https://ncc-cnc.github.io/wheretowork/reference/Include.md)
object.

------------------------------------------------------------------------

### Method `get_exclude()`

Get an exclude.

#### Usage

    SolutionSettings$get_exclude(value)

#### Arguments

- `value`:

  `character` exclude identifier.

#### Returns

[Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md)
object.

------------------------------------------------------------------------

### Method `get_parameter()`

Get an parameter.

#### Usage

    SolutionSettings$get_parameter(value)

#### Arguments

- `value`:

  `character` weight identifier.

#### Returns

[Parameter](https://ncc-cnc.github.io/wheretowork/reference/Parameter.md)
object.

------------------------------------------------------------------------

### Method `get_setting()`

Get a setting for a weight, theme, include, exclude, or parameter.

#### Usage

    SolutionSettings$get_setting(value)

#### Arguments

- `value`:

  `list` with new parameter information (see Details section)

------------------------------------------------------------------------

### Method `set_setting()`

Set a setting for theme, weight, include, exclude and parameters.

#### Usage

    SolutionSettings$set_setting(value)

#### Arguments

- `value`:

  `list` with new setting information (see Details section)

------------------------------------------------------------------------

### Method `get_feature_names()`

get character vector of all feature names from a list of themes.

#### Usage

    SolutionSettings$get_feature_names(x)

#### Arguments

- `x`:

  `list` of
  [Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md)
  objects.

#### Returns

`vector` of feature names.

------------------------------------------------------------------------

### Method `update_ss()`

update settings for theme, weight, include, exclude and parameters from
user uploaded configuration file.

#### Usage

    SolutionSettings$update_ss(value)

#### Arguments

- `value`:

  `list` with new setting information (see Details section)

------------------------------------------------------------------------

### Method `get_widget_data()`

Get data for displaying the theme in a
[`solutionSettings()`](https://ncc-cnc.github.io/wheretowork/reference/solutionSettings-widget.md)
widget.

#### Usage

    SolutionSettings$get_widget_data()

#### Returns

`list` with widget data.

------------------------------------------------------------------------

### Method `get_theme_settings()`

Get theme settings for generating a prioritization.

#### Usage

    SolutionSettings$get_theme_settings()

#### Returns

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with data.

------------------------------------------------------------------------

### Method `get_weight_settings()`

Get weight settings for generating a prioritization.

#### Usage

    SolutionSettings$get_weight_settings()

#### Returns

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with data.

------------------------------------------------------------------------

### Method `get_include_settings()`

Get include settings for generating a prioritization.

#### Usage

    SolutionSettings$get_include_settings()

#### Returns

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with data.

------------------------------------------------------------------------

### Method `get_exclude_settings()`

Get exclude settings for generating a prioritization.

#### Usage

    SolutionSettings$get_exclude_settings()

#### Returns

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with data.

------------------------------------------------------------------------

### Method `get_parameter_settings()`

Get parameter settings for generating a prioritization.

#### Usage

    SolutionSettings$get_parameter_settings()

#### Returns

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with data.

------------------------------------------------------------------------

### Method `get_theme_data()`

Get theme matrix data.

#### Usage

    SolutionSettings$get_theme_data()

#### Returns

[`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
with data.

------------------------------------------------------------------------

### Method `get_weight_data()`

Get weight matrix data.

#### Usage

    SolutionSettings$get_weight_data()

#### Returns

[`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
with data.

------------------------------------------------------------------------

### Method `get_include_data()`

Get includes matrix data.

#### Usage

    SolutionSettings$get_include_data()

#### Returns

[`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
with data.

------------------------------------------------------------------------

### Method `get_exclude_data()`

Get exclude matrix data.

#### Usage

    SolutionSettings$get_exclude_data()

#### Returns

[`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
with data.

------------------------------------------------------------------------

### Method `get_overlap()`

Get list of include and exclude names that overlap.

#### Usage

    SolutionSettings$get_overlap()

#### Returns

`list` with exclude and include names.

------------------------------------------------------------------------

### Method `set_overlap()`

Get list of include and exclude names that overlap.

#### Usage

    SolutionSettings$set_overlap()

#### Returns

`list` with exclude and include names.

------------------------------------------------------------------------

### Method `update_current_held()`

Update the current amount held for each themes and weights automatically
based on the include and exclude statuses.

#### Usage

    SolutionSettings$update_current_held(
      theme_data = self$get_theme_data(),
      weight_data = self$get_weight_data(),
      include_data = self$get_include_data(),
      exclude_data = self$get_exclude_data()
    )

#### Arguments

- `theme_data`:

  `[Matrix::sparseMatrix()] with theme data. Defaults to `self\$get_theme_data()\`.

- `weight_data`:

  `[Matrix::sparseMatrix()] with weight data. Defaults to `self\$get_weight_data()\`.

- `include_data`:

  `[Matrix::sparseMatrix()] with include data. Defaults to `self\$get_include_data()\`.

- `exclude_data`:

  `[Matrix::sparseMatrix()] with exclude data. Defaults to `self\$get_exclude_data()\`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SolutionSettings$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
