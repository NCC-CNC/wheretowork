# Update map manager widget ordering

Change the order of the layers for a map manager widget on the client.

## Usage

``` r
updateMapManagerOrder(
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

  `numeric` Vector containing the new ordering.

## See also

[`mapManager()`](https://ncc-cnc.github.io/wheretowork/reference/mapManager-widget.md).
