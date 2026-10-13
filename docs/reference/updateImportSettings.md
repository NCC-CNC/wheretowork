# Update import settings widget

Change the field/layers displayed in the widget.

## Usage

``` r
updateImportSettings(
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

  `character` Vector containing the new field/layers.

## See also

[`importSettings()`](https://ncc-cnc.github.io/wheretowork/reference/importSettings-widget.md).
