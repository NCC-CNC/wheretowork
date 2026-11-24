# New provenance from source

Create a new
[Provenance](https://ncc-cnc.github.io/wheretowork/reference/Provenance.md)
object from a recognized source.

## Usage

``` r
new_provenance_from_source(source)
```

## Arguments

- source:

  `character` provenance type. Available options include `"national"`,
  `"regional"`, or `"missing"`.

## Value

A
[Provenance](https://ncc-cnc.github.io/wheretowork/reference/Provenance.md)
object.

## Examples

``` r
# create a provenance
x <- new_provenance_from_source("missing")

# print object
print(x)
#> missing
```
