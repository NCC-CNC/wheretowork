#' @include internal.R import_spatial_data.R
NULL

#' Simulate proportion spatial data
#'
#' Simulate spatially auto-correlated proportion values for a spatial dataset.
#'
#' @param x [sf::st_sf()] or [raster::raster()] object.
#'
#' @param n `integer` number of layers/fields to simulate.
#'   Defaults to 1.
#'
#' @return [sf::st_sf()] or [raster::raster()] object with values.
#'
#' @noRd
simulate_proportion_spatial_data <- function(x, n = 1) {
  simulate_spatial_data(
    x = x,
    n = n,
    model = RandomFields::RMgauss(),
    transform = stats::plogis
  )
}

#' Simulate continuous spatial data
#'
#' Simulate spatially auto-correlated continuous values for a spatial dataset.
#'
#' @inheritParams simulate_proportion_spatial_data
#'
#' @inherit simulate_proportion_spatial_data return
#'
#' @noRd
simulate_continuous_spatial_data <- function(x, n = 1) {
  simulate_spatial_data(
    x = x,
    n = n,
    model = RandomFields::RMexp(),
    transform =
      function(x) {
        x <- abs(x)
        mult <- sample(seq(1, 100, 10), 1)
        mult * scales::rescale(abs(x), to = c(0, max(abs(x))))
      }
  )
}

#' Simulate categorical spatial data
#'
#' Simulate spatially auto-correlated categorical values for a spatial dataset.
#'
#' @inheritParams simulate_proportion_spatial_data
#'
#' @inherit simulate_proportion_spatial_data return
#'
#' @noRd
simulate_categorical_spatial_data <- function(x, n = 1) {
  simulate_spatial_data(
    x = x,
    n = n,
    model = RandomFields::RMgauss(),
    transform = function(x) {
      n <- sample.int(9, 1) + 1
      as.integer(cut(x, n))
    }
  )
}

#' Simulate spatial data
#'
#' Simulate spatial data.
#'
#' @param x [sf::st_sf()] or [raster::raster()] object.
#'
#' @inheritParams simulate_random_field
#'
#' @noRd
simulate_spatial_data <- function(x, n, model, transform) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(x, c("sf", "Raster")),
    assertthat::is.count(n),
    assertthat::noNA(n))
  # extract centroids
  if (inherits(x, "Raster")) {
    coords <- methods::as(x, "SpatialPoints")@coords
  } else {
    coords <- suppressWarnings(sf::as_Spatial(sf::st_centroid(x))@coords)
  }
  # simulate values
  mtx <- simulate_random_field(model = model, n = n, coords = coords, transform)
  # convert to spatial format
  if (inherits(x, "Raster")) {
    out <- raster::stack(lapply(seq_len(ncol(mtx)), function(i) {
        r <- x
        r[raster::Which(!is.na(r))] <- mtx[, i]
        r
    }))
    names(out) <- colnames(mtx)
  } else {
    out <- cbind(x, as.data.frame(mtx))
    out <- out[, colnames(mtx)]
  }
  out
}

#' Simulate random field
#'
#' Simulate a random field.
#'
#' @param model [RandomFields::RMmodel] object.
#'
#' @param coords `matrix` with spatial coordinates.
#'
#' @param n `integer` number of dimensions to simulate.
#'
#' @param transform `function` post-processing function.
#'
#' @return A `matrix` object.
#'
#' @noRd
simulate_random_field <- function(model, n, coords, transform) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(model, "RMmodel"),
    is.matrix(coords),
    ncol(coords) == 2,
    assertthat::is.count(n),
    assertthat::noNA(n),
    is.function(transform))
  # simulate values
  mtx <- RandomFields::RFsimulate(
    model = model,
    x = coords[, 1], y = coords[, 2], n = n, spConform = FALSE)
  if (!inherits(mtx, "matrix")) {
    mtx <- matrix(mtx, ncol = 1)
  }
  mtx <- apply(mtx, 2, transform)
  # assign column names
  colnames(mtx) <- paste0("V", seq_len(n))
  # return result
  mtx
}
