# Write Excel workbook

Save a table to disk as an Excel Spreadsheet.

## Usage

``` r
write_excel_workbook(x, path)
```

## Arguments

- x:

  `data.frame` object (or `list` of `data.frame` objects) to save to
  disk.

- path:

  `character` file path to save data.

## Value

Invisible `TRUE` indicating success.

## Details

This function is similar to the
[openxlsx::write.xlsx](https://rdrr.io/pkg/openxlsx/man/write.xlsx.html)
function. The main difference is that it formats cells columns
containing `numeric` values (and `numeric`-like values) using the
`"NUMBER"` format, and the remaining cells as `"TEXT"` format.

## Examples

``` r
# create file path to save data
f <- tempfile(fileext = ".xlsx")

# load example dataset to save to disk
data(iris)
d <- iris

# save dataset to disk
write_excel_workbook(d, f)
```
