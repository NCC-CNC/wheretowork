# Solution settings widget

Constructs a widget for managing the settings for generating solutions.

## Usage

``` r
solutionSettings(x, width = NULL, height = NULL, elementId = NULL)
```

## Arguments

- x:

  [SolutionSettings](https://ncc-cnc.github.io/wheretowork/reference/SolutionSettings.md)
  object.

- width:

  `character` width of the displayed widget. Defaults to `NULL` such
  that the widget is automatically sized.

- height:

  `character` width of the displayed widget. Defaults to `NULL` such
  that the widget is automatically sized.

- elementId:

  `character` HTML identifier for the widget. Defaults to `NULL`.

## Server value

The widget sends a `list` with the following values to the server:

- id:

  `character` identifier for the theme or weight.

- setting:

  `character` name of the updated setting. Available options include:
  `"status"`, `"factor"`, or `"goal"`.

- value:

  new `numeric` or `logical` values.

- type:

  `character` indicating if the updated setting corresponds to a `theme`
  or `weight`.

The widget also contains a text box. The server value for this text box
is a `character` string. It can be queried using `id_name` where `id` is
the argument to `elementId`.

The widget also contains two buttons. The server values for these
buttons is an `integer` indicating the number of times they has been
clicked. They can be queried using `id_start_button` and
`id_stop_button` where `id` is the argument to `elementId`.

The widget also contains a color picker. The server value for this
button is an `character` indicating the current color. It can be queried
using `id_color` where `id` is the argument to `elementId`.

## Examples

``` r
if (FALSE) { # \dontrun{
# run Shiny app to demo the sidebar pane
if (interactive()) {
  runExample("solutionSettings")
}
} # }
```
