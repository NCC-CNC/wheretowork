# Read spatial data

Import spatial data from disk.

## Usage

``` r
read_spatial_data(x)
```

## Arguments

- x:

  `character` file path.

## Value

A [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html) or
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object.

## Details

This function imports data in vector format as a
[`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html)
object. It also imports data in raster format as a
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object. Note that is will only import the first band in a multi-band
raster dataset.

## Examples

``` r
# read raster data
f1 <- system.file("external/rlogo.grd", package = "raster")
read_spatial_data(f1)
#> class       : SpatRaster
#> size        : 77, 101, 3  (nrow, ncol, nlyr)
#> resolution  : 1, 1  (x, y)
#> extent      : 0, 101, 0, 77  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs
#> source      : rlogo.grd
#> colors rgb  : 1, 2, 3
#> names       : red, green, blue
#> min values  :   0,     0,    0
#> max values  : 255,   255,  255

# read vector data
f2 <- system.file("shape/nc.shp", package = "sf")
read_spatial_data(f2)
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
#> # A tibble: 100 × 15
#>     AREA PERIMETER CNTY_ CNTY_ID NAME  FIPS  FIPSNO CRESS_ID BIR74 SID74 NWBIR74
#>    <dbl>     <dbl> <dbl>   <dbl> <chr> <chr>  <dbl>    <int> <dbl> <dbl>   <dbl>
#>  1 0.114      1.44  1825    1825 Ashe  37009  37009        5  1091     1      10
#>  2 0.061      1.23  1827    1827 Alle… 37005  37005        3   487     0      10
#>  3 0.143      1.63  1828    1828 Surry 37171  37171       86  3188     5     208
#>  4 0.07       2.97  1831    1831 Curr… 37053  37053       27   508     1     123
#>  5 0.153      2.21  1832    1832 Nort… 37131  37131       66  1421     9    1066
#>  6 0.097      1.67  1833    1833 Hert… 37091  37091       46  1452     7     954
#>  7 0.062      1.55  1834    1834 Camd… 37029  37029       15   286     0     115
#>  8 0.091      1.28  1835    1835 Gates 37073  37073       37   420     0     254
#>  9 0.118      1.42  1836    1836 Warr… 37185  37185       93   968     4     748
#> 10 0.124      1.43  1837    1837 Stok… 37169  37169       85  1612     1     160
#> # ℹ 90 more rows
#> # ℹ 4 more variables: BIR79 <dbl>, SID79 <dbl>, NWBIR79 <dbl>,
#> #   geometry <MULTIPOLYGON [°]>
```
