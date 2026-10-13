# Dataset class

Dataset class

Dataset class

## Details

Definition for the Dataset class.

Note that this method has no effect if the dataset does not have file
path on disk.

## See also

[`new_dataset()`](https://ncc-cnc.github.io/wheretowork/reference/new_dataset.md).

## Public fields

- `id`:

  `character` identifier.

- `spatial_path`:

  `character` file path.

- `attribute_path`:

  `character` file path.

- `boundary_path`:

  `character` file path.

- `spatial_data`:

  `NULL`,
  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html), or
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object.

- `attribute_data`:

  `NULL`, or
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  object.

- `boundary_data`:

  `NULL`, or
  [`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  or `NA` object.

- `planning_unit_areas`:

  `NULL`, or `numeric` vector of cached planning unit areas, see
  `get_planning_unit_areas()`.

## Methods

### Public methods

- [`Dataset$new()`](#method-Dataset-new)

- [`Dataset$print()`](#method-Dataset-print)

- [`Dataset$repr()`](#method-Dataset-repr)

- [`Dataset$import()`](#method-Dataset-import)

- [`Dataset$write()`](#method-Dataset-write)

- [`Dataset$clean()`](#method-Dataset-clean)

- [`Dataset$get_spatial_data()`](#method-Dataset-get_spatial_data)

- [`Dataset$get_attribute_data()`](#method-Dataset-get_attribute_data)

- [`Dataset$get_boundary_data()`](#method-Dataset-get_boundary_data)

- [`Dataset$get_crs()`](#method-Dataset-get_crs)

- [`Dataset$get_bbox()`](#method-Dataset-get_bbox)

- [`Dataset$get_planning_unit_indices()`](#method-Dataset-get_planning_unit_indices)

- [`Dataset$get_names()`](#method-Dataset-get_names)

- [`Dataset$get_planning_unit_areas()`](#method-Dataset-get_planning_unit_areas)

- [`Dataset$get_index()`](#method-Dataset-get_index)

- [`Dataset$has_index()`](#method-Dataset-has_index)

- [`Dataset$max_index()`](#method-Dataset-max_index)

- [`Dataset$add_index()`](#method-Dataset-add_index)

- [`Dataset$update_bm()`](#method-Dataset-update_bm)

- [`Dataset$clone()`](#method-Dataset-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a Dataset object.

#### Usage

    Dataset$new(
      id,
      spatial_path,
      attribute_path,
      boundary_path,
      spatial_data,
      attribute_data,
      boundary_data
    )

#### Arguments

- `id`:

  `character` value.

- `spatial_path`:

  `character` file path.

- `attribute_path`:

  `character` file path.

- `boundary_path`:

  `character` file path.

- `spatial_data`:

  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html), or
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object.

- `attribute_data`:

  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  object.

- `boundary_data`:

  [`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  object or [NA](https://rdrr.io/r/base/NA.html).

#### Returns

A new Dataset object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    Dataset$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    Dataset$repr()

#### Returns

`character` value.

------------------------------------------------------------------------

### Method `import()`

Import the data into memory.

#### Usage

    Dataset$import()

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write the data to disk.

#### Usage

    Dataset$write(spatial_path, attribute_path, boundary_path)

#### Arguments

- `spatial_path`:

  `character` file path.

- `attribute_path`:

  `character` file path.

- `boundary_path`:

  `character` file path.

------------------------------------------------------------------------

### Method `clean()`

Clean the dataset from memory.

#### Usage

    Dataset$clean()

------------------------------------------------------------------------

### Method `get_spatial_data()`

Get the spatial data.

#### Usage

    Dataset$get_spatial_data()

#### Returns

[`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)
or
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object.

------------------------------------------------------------------------

### Method `get_attribute_data()`

Get the attribute data.

#### Usage

    Dataset$get_attribute_data()

#### Returns

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
object.

------------------------------------------------------------------------

### Method `get_boundary_data()`

Get the spatial data.

#### Usage

    Dataset$get_boundary_data()

#### Returns

[`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
object.

------------------------------------------------------------------------

### Method `get_crs()`

Get the coordinate reference system.

#### Usage

    Dataset$get_crs()

#### Returns

[`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
object.

------------------------------------------------------------------------

### Method `get_bbox()`

Get the bounding box.

#### Usage

    Dataset$get_bbox(native = TRUE, expand = FALSE)

#### Arguments

- `native`:

  `logical` indicating if the bounding box should be in (`TRUE`) the
  native coordinate reference system or (`FALSE`) re-projected to
  longitude/latitude?

- `expand`:

  `FALSE` should the bounding box be expanded by 10%?

#### Returns

`list` object with `"xmin"`, `"xmax"`, `"ymin"`, and `"ymax"` elements.

------------------------------------------------------------------------

### Method `get_planning_unit_indices()`

Get planning unit indices.

#### Usage

    Dataset$get_planning_unit_indices()

#### Returns

`integer` vector of indices.

------------------------------------------------------------------------

### Method `get_names()`

Get attribute names.

#### Usage

    Dataset$get_names()

#### Returns

`character` vector of field/layer names.

------------------------------------------------------------------------

### Method `get_planning_unit_areas()`

Get area values. These are calculated once and then cached.

#### Usage

    Dataset$get_planning_unit_areas()

#### Returns

`numeric` vector of values.

------------------------------------------------------------------------

### Method `get_index()`

Get a data from the dataset at an index.

#### Usage

    Dataset$get_index(index)

#### Arguments

- `index`:

  `character` or `integer` indicating the field/layer with the data.

#### Returns

[`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)
or
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object.

------------------------------------------------------------------------

### Method `has_index()`

Check if the dataset has an index.

#### Usage

    Dataset$has_index(index)

#### Arguments

- `index`:

  `character` or `integer` indicating the field/layer with the data.

#### Returns

`logical` indicating if data is present or not.

------------------------------------------------------------------------

### Method `max_index()`

Maximum index.

#### Usage

    Dataset$max_index()

#### Returns

`integer` largest index.

------------------------------------------------------------------------

### Method `add_index()`

Add data at an index.

#### Usage

    Dataset$add_index(index, values)

#### Arguments

- `index`:

  `character` or `integer` indicating the field/layer with the data.

- `values`:

  `numeric` vector.

------------------------------------------------------------------------

### Method `update_bm()`

Updates old boundary matrix to new format.

#### Usage

    Dataset$update_bm()

#### Returns

[`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Dataset$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
