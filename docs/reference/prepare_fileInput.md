# Prepare file input

Prepare files uploaded using
[`shiny::fileInput()`](https://rdrr.io/pkg/shiny/man/fileInput.html).

## Usage

``` r
prepare_fileInput(x)
```

## Arguments

- x:

  `data.frame` object.

## Value

`character` file paths.

## Details

This function renames files to their original file names after they have
been uploaded.
