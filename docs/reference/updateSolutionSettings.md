# Update solution settings widget

Change the settings for solution settings widget on the client.
Specifically, change the settings for a theme or weight depicted in a
solution settings widget.

## Usage

``` r
updateSolutionSettings(
  session = shiny::getDefaultReactiveDomain(),
  inputId,
  value
)
```

## Arguments

- session:

  The `session` object passed to function given to `shinyServer` Default
  is
  [`shiny::getDefaultReactiveDomain()`](https://rdrr.io/pkg/shiny/man/domains.html).

- inputId:

  `character` The identifier of the input object.

- value:

  `list` Object containing the new settings (see Details).

## Details

### Overview

The argument to `value` should be `list` object containing the
information necessary to update a theme or weight. Broadly speaking, it
should contain the following elements:

- id:

  `character` value with the identifier for the theme or weight.

- setting:

  `character` value with the name of the setting to update.

- value:

  `ANY` new value for the setting.

- type:

  `character` value indicating if the setting is a `theme` or `weight`.

Note that the `value` element in the `list` object should have a class
(i.e. `numeric`,
`logical, or `character`) that is relevant to the setting that should be updated. For example, if the `setting`element is equal to`name`, then the `value`element should contain a`character\`
value. For reference, we provide examples detailing all the various
settings that can be updated below.

### Themes

Here we detail all possible settings that can be updated for themes.

**Update the name.** This controls the bold text shown in the header of
the theme.

- id:

  `"THEMEID"`

- setting:

  `"name"`

- value:

  `"SPECIES"`

- type:

  `"theme"`

**Update the status.** This controls the large switch shown in the
header of theme. Note that the `value` element must be a `logical`
(`TRUE` or `FALSE`) value.

- id:

  `"THEMEID"`

- setting:

  `"status"`

- value:

  `TRUE`

- type:

  `"theme"`

**Update the view.** This controls whether the "group" or "single" tab
is active for a theme. Note that the `value` element must be a
`character` value equal to `"group"` or `"single"`.

- id:

  `"THEMEID"`

- setting:

  `"view"`

- value:

  `"group"`

- type:

  `"theme"`

**Update the group goal.** This controls the slider present under the
"group" tab.

- id:

  `"THEMEID"`

- setting:

  `"group_goal"`

- value:

  `0.7`

- type:

  `"theme"`

**Update the feature status.** This controls the switches present under
the "single" tab. Note that the `value` element must have a value for
each feature within the theme (the example below assumes the theme has
three features).

- id:

  `"THEMEID"`

- setting:

  `"feature_status"`

- value:

  `c(TRUE, FALSE, TRUE)`

- type:

  `"theme"`

**Update the feature current values.** This controls the bars indicating
the amount currently held for each feature under both the "single" and
"group" tabs. Note that the `value` element must have a value for each
feature within the theme (the example below assumes the theme has three
features).

- id:

  `"THEMEID"`

- setting:

  `"feature_current"`

- value:

  `c(0.3, 0.1, 0.7)`

- type:

  `"theme"`

**Update the feature goals.** This controls the sliders present under
the "single" tab. Note that the `value` element must have a value for
each feature within the theme (the example below assumes the theme has
three features).

- id:

  `"THEMEID"`

- setting:

  `"feature_goal"`

- value:

  `c(0.3, 0.1, 0.7)`

- type:

  `"theme"`

### Weights

Here we detail all possible settings that can be updated for weights.

**Update the name.** This controls the bold text shown in the header of
the weight.

- id:

  `"WEIGHTID"`

- setting:

  `"name"`

- value:

  `"SPECIES"`

- type:

  `"weight"`

**Update the status.** This controls the large switch shown in the
header of weight. Note that the `value` element must be a `logical`
(`TRUE` or `FALSE`) value.

- id:

  `"WEIGHTID"`

- setting:

  `"status"`

- value:

  `TRUE`

- type:

  `"weight"`

**Update the factor.** This controls the slider shown for the weight.

- id:

  `"WEIGHTID"`

- setting:

  `"factor"`

- value:

  `0.1`

- type:

  `"weight"`

### Includes

Here we detail all possible settings that can be updated for includes.

**Update the name.** This controls the bold text shown in the header of
the include.

- id:

  `"INCLUDEID"`

- setting:

  `"name"`

- value:

  `"PA"`

- type:

  `"include"`

**Update the status.** This controls the large switch shown in the
header of include. Note that the `value` element must be a `logical`
(`TRUE` or `FALSE`) value.

- id:

  `"INCLUDEID"`

- setting:

  `"status"`

- value:

  `TRUE`

- type:

  `"include"`

### Excludes

Here we detail all possible settings that can be updated for excludes.

**Update the name.** This controls the bold text shown in the header of
the exclude.

- id:

  `"EXCLUDEID"`

- setting:

  `"name"`

- value:

  `"XX"`

- type:

  `"exclude"`

**Update the status.** This controls the large switch shown in the
header of exclude. Note that the `value` element must be a `logical`
(`TRUE` or `FALSE`) value.

- id:

  `"EXCLUDEID"`

- setting:

  `"status"`

- value:

  `TRUE`

- type:

  `"exclude"`

## See also

[`solutionSettings()`](https://ncc-cnc.github.io/wheretowork/reference/solutionSettings-widget.md).
