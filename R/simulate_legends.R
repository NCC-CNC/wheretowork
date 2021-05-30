#' @include internal.R
NULL

#' Simulate continuous legend
#'
#' This function simulates a continuous legend ([ContinuousLegend]) object.
#' It is primarily used for testing the package.
#'
#' @return A [ContinuousLegend] object.
#'
#' @seealso [new_continuous_legend()].
#'
#' @examples
#' print(simulate_continuous_legend())
#'
#' @export
simulate_continuous_legend <- function() {
  # randomly pick colors for legend
  d <- RColorBrewer::brewer.pal.info
  d <- d[d$category == "seq", , drop = FALSE]
  col <- RColorBrewer::brewer.pal(n = 8, name = sample(rownames(d), 1))
  # return legend
  new_continuous_legend(
    min_value = stats::runif(1, 0, 50),
    max_value = stats::runif(1, 51, 100),
    colors = col)
}

#' Simulate categorical legend
#'
#' This function simulates a categorical legend ([CategoricalLegend]) object.
#' It is primarily used for testing the package.
#'
#' @return A [CategoricalLegend] object.
#'
#' @examples
#' print(simulate_continuous_legend())
#'
#' @seealso [new_categorical_legend()].
#'
#' @export
simulate_categorical_legend <- function() {
  # randomly pick number of values for legend
  n <- sample.int(8, 1)
  # randomly pick colors for legend
  d <- RColorBrewer::brewer.pal.info
  d <- d[d$category == "qual", , drop = FALSE]
  col <- suppressWarnings({
    RColorBrewer::brewer.pal(n = n, name = sample(rownames(d), 1))
  })
  # randomly sample numbers for legend
  values <- sample.int(100, n)
  # return legend
  new_categorical_legend(values = values, colors = col[seq_len(n)])
}
