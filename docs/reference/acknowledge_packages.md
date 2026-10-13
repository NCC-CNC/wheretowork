# Acknowledge R packages.

This function creates a HTML element for acknowledging R packages.

## Usage

``` r
acknowledge_packages(x, prefix = "We thank the ", suffix = " R packages.")
```

## Arguments

- x:

  `character` names of package.

- prefix:

  `character` prefix text. Defaults to `"We thank: "`.

- suffix:

  `character` suffix text. Defaults to `" R packages."`.

## Value

`shiny.tag` object.
