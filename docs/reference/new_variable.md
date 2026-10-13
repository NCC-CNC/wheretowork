# New variable

Create a new
[Variable](https://ncc-cnc.github.io/wheretowork/reference/Variable.md)
object.

## Usage

``` r
new_variable(
  dataset,
  index,
  units,
  total,
  legend,
  provenance = new_provenance_from_source("missing")
)
```

## Arguments

- dataset:

  `Dataset` file path for the dataset.

- index:

  `character` or `integer` indicating the field/layer with the data.

- units:

  `character` units for the values in the underlying data.

- total:

  `numeric` total amount of all values in the underlying data.

- legend:

  `Legend` object.

- provenance:

  [Provenance](https://ncc-cnc.github.io/wheretowork/reference/Provenance.md)
  object. Defaults to `new_provenance_from_source("missing")`.

## Value

A
[Variable](https://ncc-cnc.github.io/wheretowork/reference/Variable.md)
object.

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
v <- new_variable(d,
  index = 1, total = 12, units = "ha",
  legend = new_continuous_legend(1, 100, c("#000000", "#AAAAAA"))
)

# print object
print(v)
#> Variable
#>   dataset: .../sim_raster_spatial.tif
#>   index:   Brown_necked_Parrot_Brown_necked_
#>   total:   12
#>   units:   ha
#>   provenance:   missing
```
