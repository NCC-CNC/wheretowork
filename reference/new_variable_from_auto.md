# New variable from automatic calculations

Create a new
[Variable](https://ncc-cnc.github.io/wheretowork/reference/Variable.md)
object by automatically calculating all metadata from the underlying
data. This function is useful when pre-calculated metadata are not
available. Note this function will take longer to create variables than
other functions because it requires performing geospatial operations.

## Usage

``` r
new_variable_from_auto(
  dataset,
  index,
  units = "",
  type = "auto",
  colors = "random",
  provenance = "missing",
  labels = "missing",
  hidden = FALSE
)
```

## Arguments

- dataset:

  `Dataset` file path for the dataset.

- index:

  `character` or `integer` indicating the field/layer with the data.

- units:

  `character` units for the values in the underlying data.

- type:

  `character` indicating if the data contain continuous (`"continuous"`)
  or categorical (`"categorical"`) numerical values. Defaults to
  `"auto"` such that data are automatically identified.

- colors:

  `character` object containing the colors for visualization (see
  Details for more information). Defaults to `"random"` such that colors
  are randomly generated.

- provenance:

  `character` value indicating the type of provenance. The argument must
  be a valid type (see
  [`new_provenance_from_source()`](https://ncc-cnc.github.io/wheretowork/reference/new_provenance_from_source.md)).
  Defaults to `"missing"`.

- labels:

  `character` object containing manual legend labels. Defaults to
  `"missing"`.

- hidden:

  `logical` indicating if variable will be hidden from map. Defaults to
  `"FALSE"`. If hidden, a
  [NullLegend](https://ncc-cnc.github.io/wheretowork/reference/NullLegend.md)
  is used

## Value

A
[Variable](https://ncc-cnc.github.io/wheretowork/reference/Variable.md)
object.

## Details

The argument to `colors` can be a vector of different colors (in
hexadecimal format, e.g.
`"#112233"), or a single `character`containing the name of a color palette that is used to generate a vector of different colors (see [color_palette()] for more information). The color palette name`"random"\`
is also available, such that colors are generated using a randomly
selected palette.

## Examples

``` r
# find data file paths
f1 <- system.file(
  "extdata", "projects", "sim_raster", "sim_raster_spatial.tif",
  package = "wheretowork"
)
f2 <- system.file(
  "extdata",  "projects", "sim_raster", "sim_raster_attribute.csv.gz",
  package = "wheretowork"
)
f3 <- system.file(
  "extdata",  "projects", "sim_raster", "sim_raster_boundary.csv.gz",
  package = "wheretowork"
)

# create new dataset
d <- new_dataset(f1, f2, f3)

# create new variable
v <- new_variable_from_auto(d, index = 1)

# print object
print(v)
#> Variable
#>   dataset: .../sim_raster_spatial.tif
#>   index:   Brown_necked_Parrot_Brown_necked_
#>   total:   483
#>   units:   
#>   provenance:   missing
```
