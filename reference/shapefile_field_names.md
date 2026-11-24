# Shapefile field names

Quickly extract the field names in a shapefile.

## Usage

``` r
shapefile_field_names(x, inherits = NULL)
```

## Arguments

- x:

  `character` path.

- inherits:

  `character` class to return only column names that contain a certain
  type of data. Defaults to `NULL` such that all column names are
  returned.

## Value

`character` vector of field names.

## Examples

``` r
shapefile_field_names(system.file("shape/nc.shp", package = "sf"))
#>  [1] "AREA"      "PERIMETER" "CNTY_"     "CNTY_ID"   "NAME"      "FIPS"     
#>  [7] "FIPSNO"    "CRESS_ID"  "BIR74"     "SID74"     "NWBIR74"   "BIR79"    
#> [13] "SID79"     "NWBIR79"  
```
