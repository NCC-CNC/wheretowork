# Update a layer in a map manager widget

Change the layer for a map manager widget on the client.

## Usage

``` r
updateMapManagerLayer(
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
information necessary to update a layer. Broadly speaking, it should
contain the following elements:

- id:

  `character` value with the identifier for the layer.

- setting:

  `character` value with the name of the setting to update.

- value:

  `ANY` new value for the setting.

Note that the `value` element in the `list` object should have a class
(i.e. `numeric`, `logical`) that is relevant to the setting that should
be updated. For example, if the `setting` element is equal to `name`,
then the `value` element should contain a `character` value. For
reference, we provide examples detailing all the various settings that
can be updated below.

**Update layer name.** This controls the bold text shown in the header
of the layer.

- id:

  `"LAYERID"`

- setting:

  `"name"`

- value:

  `"SPECIES"`

**Update layer visibility.** This controls the visible checkbox for a
layer. Note that the `value` element must be a `logical` (`TRUE` or
`FALSE`) value

- id:

  `"LAYERID"`

- setting:

  `"visible"`

- value:

  `TRUE`

**Update the visibility of features within a layer.** This controls the
visible checkbox for each feature within a theme. Note that the `value`
element must have a value for each feature within the theme (the example
below assumes the theme has three features).

- id:

  `"LAYERID"`

- setting:

  `"feature_visible"`

- value:

  `c(TRUE, FALSE, TRUE)`

**Update the order of features within a layer.** This controls the
relative order of the features within a theme. Note that the `value`
element must have a value for each dataset within a layer (e.g. the
example below assumes a theme has three features).

- id:

  `"LAYERID"`

- setting:

  `"feature_order"`

- value:

  `c(TRUE, FALSE, TRUE)`

## See also

[`mapManager()`](https://ncc-cnc.github.io/wheretowork/reference/mapManager-widget.md).
