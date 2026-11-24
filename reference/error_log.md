# Error log

Create an error log based on a `try-error` object.

## Usage

``` r
error_log(x)
```

## Arguments

- x:

  `try-error` object.

## Value

`character` value.

## Details

This function checks `attr(x, "log")` for a list of `SimpleError`
objects that are used to construct the log. If the argument to `x` does
not contain this attribute, then the text `"No error log available."` is
returned.
