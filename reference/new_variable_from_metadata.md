# New variable from metadata

Create a new
[Variable](https://ncc-cnc.github.io/wheretowork/reference/Variable.md)
object using metadata. This function is useful when pre-calculated are
available, so that previously calculated metadata can be used.

## Usage

``` r
new_variable_from_metadata(dataset, metadata)
```

## Arguments

- dataset:

  `Dataset` file path for the dataset.

- metadata:

  `list` object (see Details for more information).

## Details

The argument to `metadata` should contain the following elements:

- "index":

  `character` or `integer` indicating the field/layer with the data
  within the dataset that has the data.

- "units":

  `character` units for the values in the underlying data.

- "type":

  `character` indicating if the data contain continuous (`"continuous"`)
  or categorical (`"categorical"`) values.

- "provenance":

  `character` indicating the data source. (see
  [`new_provenance_from_source()`](https://ncc-cnc.github.io/wheretowork/reference/new_provenance_from_source.md)).

- "colors":

  `character` vector containing colors for visualization.

- "total":

  `numeric` sum of all values in dataset.

- "min_value":

  `numeric` minimum value in dataset. Required only for continuous data.

- "max_value":

  `numeric` maximum value in dataset. Required only for continuous data.

- "values":

  `numeric` vector of unique value in dataset. Required only for
  categorical data.

- "labels":

  `character` vector of unique labels for manual legend. Required only
  for categorical data, manual legend.

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
v <- new_variable_from_metadata(
  d, list(
    index = 1, units = "ha", type = "continuous",
    colors = c("#000000", "#AAAAAA"), total = 12,
    min_value = 1, max_value = 3, provenance = "missing", 
    labels = "missing"
  )
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
