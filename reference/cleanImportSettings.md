# Clean import settings widget

Remove all field/layers displayed in the widget.

## Usage

``` r
cleanImportSettings(session = shiny::getDefaultReactiveDomain(), inputId)
```

## Arguments

- session:

  The `session` object passed to function given to `shinyServer` Default
  is
  [`shiny::getDefaultReactiveDomain()`](https://rdrr.io/pkg/shiny/man/domains.html).

- inputId:

  `character` The identifier of the input object.

## See also

[`importSettings()`](https://ncc-cnc.github.io/wheretowork/reference/importSettings-widget.md).
