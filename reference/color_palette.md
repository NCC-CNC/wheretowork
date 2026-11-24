# Color palette

Generate colors using the name of a color palette.

## Usage

``` r
color_palette(x, n = NULL)
```

## Arguments

- x:

  `character` name of the color palette (see Details for available
  options).

- n:

  `integer` number of colors required. A `NULL` argument can also be
  used to obtain a default number of colors. If a `NULL` argument is
  supplied and the palette has a limited number of colors, then the
  maximum number of colors is returned. Otherwise, if a `NULL` argument
  is supplied and the palette does not have a limited number of colors,
  then a 20 colors are returned. The default argument is `NULL`.

## Details

This function can be used to generate colors using palettes provided by
the RColorBrewer and viridisLite packages. If colors are from multiple
palettes, they can be supplied as a single `character` object delimited
by semicolon (`";"`) characters.

Specifically, available palettes include:

- `"viridis"` (see
  [`viridisLite::viridis()`](https://sjmgarnier.github.io/viridisLite/reference/viridis.html))

- `"magma"` (see
  [`viridisLite::magma()`](https://sjmgarnier.github.io/viridisLite/reference/viridis.html))

- `"plasma"` (see
  [`viridisLite::plasma()`](https://sjmgarnier.github.io/viridisLite/reference/viridis.html))

- `"inferno"` (see
  [`viridisLite::inferno()`](https://sjmgarnier.github.io/viridisLite/reference/viridis.html))

- `"cividis"` (see
  [`viridisLite::cividis()`](https://sjmgarnier.github.io/viridisLite/reference/viridis.html))

- `"mako"` (see
  [`viridisLite::mako()`](https://sjmgarnier.github.io/viridisLite/reference/viridis.html))

- `"rocket"` (see
  [`viridisLite::rocket()`](https://sjmgarnier.github.io/viridisLite/reference/viridis.html))

- `"turbo"` (see
  [`viridisLite::turbo()`](https://sjmgarnier.github.io/viridisLite/reference/viridis.html))

- `"BrBG`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"PiYG`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"PRGn`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"PuOr`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"RdBu`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"RdGy`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"RdYlBu`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"RdYlGn`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"Spectral`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"Accent`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"Dark2`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"Paired`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"Pastel1`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"Pastel2`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"Set1`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"Set2`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"Set3`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"Blues`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"BuGn`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"BuPu`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"GnBu`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"Greens`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"Greys`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"Oranges`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"OrRd`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"PuBu`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"PuBuGn`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"PuRd`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"Purples`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"RdPu`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"Reds`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"YlGn`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"YlGnBu`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"YlOrBr`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"YlOrRd`" (see
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html))

- `"random"` (the above palettes are randomly used to generate colors)

## Examples

``` r
# obtain 5 colors from the Greens palette
color_palette("Greens", 5)
#> [1] "#EDF8E9" "#BAE4B3" "#74C476" "#31A354" "#006D2C"

# obtain all colors from the Greens palette
color_palette("Greens", NULL)
#> [1] "#F7FCF5" "#E5F5E0" "#C7E9C0" "#A1D99B" "#74C476" "#41AB5D" "#238B45"
#> [8] "#006D2C" "#00441B"

# obtain 15 colors using the Set1 and Pastel1 palettes
color_palette("Set1;Pastel1", 15)
#>  [1] "#E41A1C" "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00" "#FFFF33" "#A65628"
#>  [8] "#F781BF" "#999999" "#FBB4AE" "#B3CDE3" "#CCEBC5" "#DECBE4" "#FED9A6"
#> [15] "#FFFFCC"

# obtain all colors from the plasma palette
color_palette("plasma", NULL)
#> [1] "#0D0887FF" "#7E03A8FF" "#CC4678FF" "#F89441FF" "#F0F921FF"

# obtain all colors from a random palette
color_palette("random", NULL)
#>  [1] "#543005" "#8C510A" "#BF812D" "#DFC27D" "#F6E8C3" "#F5F5F5" "#C7EAE5"
#>  [8] "#80CDC1" "#35978F" "#01665E" "#003C30"

# obtain 5 colors from random palette(s)
color_palette("random", 5)
#> [1] "#FEF0D9" "#FDCC8A" "#FC8D59" "#E34A33" "#B30000"
```
