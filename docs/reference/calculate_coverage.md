# Calculate coverage

Calculate how well a solution covers data.

## Usage

``` r
calculate_coverage(x, data)
```

## Arguments

- x:

  `numeric` solution values.

- data:

  [`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  object.

## Value

`numeric` vector.

## Examples

``` r
# load dependency
library(Matrix)

# simulate solution values for 10 planning units
solution_values <- sample(c(0, 1), 10, replace = TRUE)

# simulate data for 5 features in those 10 planning units
feature_data <- as(matrix(runif(10 * 5), ncol = 10), "dgCMatrix")

# calculate coverage
calculate_coverage(solution_values, feature_data)
#> [1] 0.28590246 0.39446301 0.46700761 0.09504548 0.30568609
```
