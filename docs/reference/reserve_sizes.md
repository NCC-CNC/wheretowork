# Reserve sizes

Calculate the size of individual reserves in a solution.

## Usage

``` r
reserve_sizes(x, areas, boundary_matrix)
```

## Arguments

- x:

  `numeric` vector containing the solution values for each planning
  unit.

- areas:

  `numeric` vector containing the area of each planning unit.

- boundary_matrix:

  [`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  object with boundary length data for the planning units. Or
  [`NA`](https://rdrr.io/r/base/NA.html) indicating that no boundary
  data is available (see Details for more information)

## Value

A `numeric` vector containing the total area of each reserve.

## Details

For spatial uploads (shapefile) with many planning units, building
boundary data can result in a std::bad_alloc error. To avoid this, the
user can skip generating a boundary matrix on the
`new_dataset_from_auto` method. For these scenarios, reserve sizes can
not be calculated when the `boundary_matrix` is set to `NA`.
