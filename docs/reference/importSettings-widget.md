# Import settings

Constructs a widget for specifying settings for importing data. This
widget is designed to be used within a Shiny web application.

## Usage

``` r
importSettings(
  buttonId,
  x = NULL,
  width = NULL,
  height = NULL,
  elementId = NULL
)
```

## Arguments

- buttonId:

  `character` containing the identifier for the button HTML element to
  accompany this widget.

- x:

  `character` vector containing the field/layer names for the dataset. A
  `NULL` value may also be specified to initialize the widget without
  any field/layer names.

- width:

  `character` width of the displayed widget. Defaults to `NULL` such
  that the widget is automatically sized.

- height:

  `character` width of the displayed widget. Defaults to `NULL` such
  that the widget is automatically sized.

- elementId:

  `character` HTML identifier for the widget. Defaults to `NULL`.

## Server value

A `list` object containing a `list` element for each field/layer name
containing:

- name:

  `character` name of the field/layer.

- import:

  `logical` value indicating if the field/layer should be used.

- type:

  `character` value. Available options are `"Theme"`, `"Weight"`, or
  `"Include"`.

- nonce:

  `numeric` random value used to ensure that updates trigger when the
  button is clicked.

## Examples

``` r
if (FALSE) { # \dontrun{
# run Shiny app to demo the sidebar pane
if (interactive()) {
  runExample("importSettings")
}
} # }
```
