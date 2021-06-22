#' @include internal.R
NULL

#' Simulate weights
#'
#' This function simulates [Weight] objects.
#'
#' @inheritParams simulate_themes
#'
#' @param n `integer` number of weights to simulate.
#'
#' @return A `list` of simulated [Weight] objects.
#'
#' @seealso [new_weight].
#'
#' @examples
#' # import data
#' f <- system.file("extdata", "sim_raster_data.tif", package = "locationmisc")
#' d <- new_dataset(raster::raster(f))
#'
#' # simulate data
#' x <- simulate_weights(dataset = d, n = 2)
#'
#' # print results
#' print(x)
#'
#' @export
simulate_weights <- function(
  dataset, n) {
  # assert arguments are valid
  assertthat::assert_that(
    ## data
    inherits(dataset, c("Dataset")),
    ## weights
    assertthat::is.count(n),
    assertthat::noNA(n))

  # extract data
  data <- dataset$get_data()

  # set weight names
  wn <- example_weight_names()
  wn <- wn[sample.int(nrow(wn)), , drop = FALSE]
  wn <- wn[seq_len(n), , drop = FALSE]

  # assert that there are sufficient example names
  assertthat::assert_that(
    n <= nrow(wn),
    msg = "insufficient example names for this many weights"
  )

  # set index names
  wn_index <- make_valid_names(wn[[1]])

  # simulate underlying data values
  wd <- simulate_proportion_spatial_data(data, n)
  names(wd)[seq_len(n)] <- wn_index
  if (inherits(data, "sf")) {
    dataset$data <- cbind(dataset$data, sf::st_drop_geometry(wd))
  } else {
    dataset$data <- raster::stack(dataset$data, wd)
  }

  # generate weights
  w <- lapply(seq_len(n), function(i) {
    new_weight(
      name = wn[[1]][i],
      variable =
        new_variable_from_auto(
          dataset = dataset, index = wn_index[[i]], units = "ha"
        )
    )
  })

  # return results
  w
}
