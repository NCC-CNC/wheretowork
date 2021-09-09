#' @include internal.R
NULL

#' Simulate includes
#'
#' This function simulates [Include] objects.
#'
#' @inheritParams simulate_themes
#'
#' @param n `integer` number of objects to simulate.
#'
#' @return A `list` of simulated [Include] objects.
#'
#' @seealso [new_include].
#'
#' @examples
#' # find data file paths
#' f1 <- system.file(
#'   "extdata", "projects", "sim_raster", "sim_raster_spatial.tif",
#'   package = "wheretowork"
#' )
#' f2 <- system.file(
#'   "extdata",  "projects", "sim_raster", "sim_raster_attribute.csv.gz",
#'   package = "wheretowork"
#' )
#' f3 <- system.file(
#'   "extdata",  "projects", "sim_raster", "sim_raster_boundary.csv.gz",
#'   package = "wheretowork"
#' )
#'
#' # create new dataset
#' d <- new_dataset(f1, f2, f3)
#'
#' # simulate data
#' x <- simulate_includes(dataset = d, n = 2)
#'
#' # print results
#' print(x)
#' @export
simulate_includes <- function(dataset, n) {
  # assert arguments are valid
  assertthat::assert_that(
    ## data
    inherits(dataset, c("Dataset")),
    ## weights
    assertthat::is.count(n),
    assertthat::noNA(n)
  )

  # extract data
  data <- dataset$get_spatial_data()
  idx <- dataset$attribute_data[["_index"]]

  # set names
  ln <- example_include_names()
  ln <- ln[sample.int(nrow(ln)), , drop = FALSE]
  ln <- ln[seq_len(n), , drop = FALSE]

  # assert that there are sufficient example names
  assertthat::assert_that(
    n <= nrow(ln),
    msg = "insufficient example names for this many objects"
  )

  # set index names
  ln_index <- make_valid_names(ln[[1]])

  # simulate underlying data values
  ld <- simulate_binary_spatial_data(data, n)
  for (i in seq_along(ln_index)) {
    dataset$add_index(ln_index[[i]], ld[[i]][idx])
  }

  # generate weights
  out <- lapply(seq_len(n), function(i) {
    new_include(
      name = ln[[1]][i],
      variable =
        new_variable(
          dataset = dataset,
          index = ln_index[[i]],
          units = "",
          total = sum(ld[[i]][idx]),
          legend = simulate_include_legend(),
          provenance = new_provenance_from_source(
            sample(c("regional", "national"), 1)
          )
        )
    )
  })

  # return results
  out
}
