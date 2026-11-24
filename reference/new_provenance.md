# New Provenance

Create a new
[Provenance](https://ncc-cnc.github.io/wheretowork/reference/Provenance.md)
object.

## Usage

``` r
new_provenance(name, icon, color, description)
```

## Arguments

- name:

  `character` name of provenance source.

- icon:

  `character` name of icon to depict the soruce.

- color:

  `character` value containing the color for the icon.

- description:

  `character` value containing a description of the provenance.

## Value

A
[Provenance](https://ncc-cnc.github.io/wheretowork/reference/Provenance.md)
object.

## Examples

``` r
# create a provenance
x <- new_provenance(
  name = "Global", icon = "globe", color = "#123456",
  description = "Data is global."
)

# print object
print(x)
#> Global
```
