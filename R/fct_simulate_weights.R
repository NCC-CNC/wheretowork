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
#' @param continuous `logical` should the data be continuous? 
#' Defaults to `TRUE`.
#' 
#' @return A `list` of simulated [Weight] objects.
#'
#' @seealso [new_weight].
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
#' x <- simulate_weights(dataset = d, n = 2)
#' # print results
#' print(x)
#' 
#' 
#' @export
simulate_weights <- function(dataset, n, continuous = TRUE) {
  
  # assert arguments are valid
  assertthat::assert_that(
    ## data
    inherits(dataset, c("Dataset")),
    ## weights
    assertthat::is.count(n),
    assertthat::noNA(n),
    assertthat::is.flag(continuous),
    assertthat::noNA(continuous)
  )

  # extract data
  data <- dataset$get_spatial_data()

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
  if (continuous) {
    wd <- simulate_proportion_spatial_data(data, n)
  } else {
    wd <- simulate_categorical_spatial_data(data, n)
  }  
  names(wd)[seq_len(n)] <- wn_index
  if (inherits(data, "sf")) {
    for (i in seq_along(wn_index)) {
      dataset$add_index(wn_index[[i]], wd[[wn_index[[i]]]])
    }
  } else {
    idx <- dataset$attribute_data[["_index"]]
    for (i in seq_along(wn_index)) {
      dataset$add_index(wn_index[[i]], unlist(wd[[wn_index[[i]]]][idx])) # unlist data.frame
    }
  }

  # generate weights
  w <- lapply(seq_len(n), function(i) {
    if (continuous) {
      v <- new_variable_from_auto(dataset = dataset, index = wn_index[[i]], 
        units = "ha",provenance = sample(c("regional", "national"), 1)
      )
    } else {
      d <- dataset$get_attribute_data()[[wn_index[[i]]]]
      u <-  sort(c(stats::na.omit(unique(d))))
      cp <- color_palette(x = "random", n = length(u))
      v <- new_variable(dataset = dataset, index = wn_index[[i]], 
        units = "ha", legend = new_manual_legend(u, cp, paste("value:", u)), 
        total = sum(d, na.rm = TRUE),  
        provenance = new_provenance_from_source("national")) 
    }
    
    new_weight(
      name = wn[[1]][i],
      current = round(stats::runif(1, 0.1, 0.6), 2),
      variable = v
    )
  })

  # return results
  w
}
