# Add a layer in a map manager widget

Add a new layer to a map manager widget on the client.

## Usage

``` r
addMapManagerLayer(session = shiny::getDefaultReactiveDomain(), inputId, value)
```

## Arguments

- session:

  The `session` object passed to function given to `shinyServer` Default
  is
  [`shiny::getDefaultReactiveDomain()`](https://rdrr.io/pkg/shiny/man/domains.html).

- inputId:

  `character` The identifier of the input object.

- value:

  [Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md),
  [Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md),
  or
  [Solution](https://ncc-cnc.github.io/wheretowork/reference/Solution.md)
  object.

## See also

[`mapManager()`](https://ncc-cnc.github.io/wheretowork/reference/mapManager-widget.md).
