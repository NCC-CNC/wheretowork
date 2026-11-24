# Drop a layer from a map manager widget

Drop a layer from a map manager widget on the client.

## Usage

``` r
dropMapManagerLayer(
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

  `character` layer identifier.

## See also

[`mapManager()`](https://ncc-cnc.github.io/wheretowork/reference/mapManager-widget.md).
