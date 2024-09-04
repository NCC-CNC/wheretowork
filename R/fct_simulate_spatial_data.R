#' @include internal.R
NULL

#' Simulate proportion spatial data
#'
#' Simulate spatially auto-correlated proportion values for a spatial dataset.
#'
#' @param x [sf::st_sf()] or [terra::rast()] object.
#'
#' @param n `integer` number of layers/fields to simulate.
#'   Defaults to 1.
#'
#' @return [sf::st_sf()] or [terra::rast()] object with values.
#'
#' @noRd
simulate_proportion_spatial_data <- function(x, n = 1) {
  simulate_data(
    x = x,
    n = n,
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
  simulate_data(
    x = x,
    n = n,
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
  simulate_data(
    x = x,
    n = n,
    transform = function(x) {
      n <- sample.int(9, 1) + 1
      as.integer(cut(x, n))
    }
  )
}

#' Simulate binary spatial data
#'
#' Simulate spatially auto-correlated binary values for a spatial dataset.
#'
#' @inheritParams simulate_proportion_spatial_data
#'
#' @inherit simulate_proportion_spatial_data return
#'
#' @noRd
simulate_binary_spatial_data <- function(x, n = 1) {
  simulate_data(
    x = x,
    n = n,
    transform = function(x) {
      round(x > 0)
    }
  )
}

#' Simulate data
#'
#' Simulate spatially auto-correlated data using Gaussian random fields.
#'
#' @param x [terra::rast()] or [sf::st_sf()] 
#' object to use as a template.
#'
#' @param n `integer` number of layers to simulate.
#'   Defaults to 1.
#'
#' @param scale `numeric` parameter to control level of spatial
#'   auto-correlation in the simulated data.
#'   Defaults to 0.5.
#'
#' @param intensity `numeric` average value of simulated data.
#'   Defaults to 0.
#'
#' @param sd `numeric` standard deviation of simulated data.
#'   Defaults to 1.
#'
#' @param transform `function` transform values output from the simulation.
#'   Defaults to the [identity()] function such that values remain the same
#'   following transformation.
#'
#' @family simulations
#'
#' @return A [terra::rast()] or [sf::st_sf()] object.
#'
#' @examples
#' \dontrun{
#' # create raster
#' r <- terra::rast(
#'   ncols = 10, nrows = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1, vals = 1
#' )
#'
#' # simulate data using a Gaussian field
#' x <- simulate_data(r, n = 1, scale = 0.2)
#'
#' # plot simulated data
#' plot(x, main = "simulated data", axes = FALSE)
#' }
#' @export
simulate_data <- function(x, n, scale, intensity, sd, transform) {
  assertthat::assert_that(
    inherits(x, c("SpatRaster", "sf"))
  )  
  UseMethod("simulate_data")
}

#' @rdname simulate_data
#' @method simulate_data sf
#' @export
simulate_data.sf <- function(
    x, n = 1, 
    scale = 0.5, 
    intensity = 0,
    sd = 1, 
    transform = identity
 ) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, "sf"),
    assertthat::is.count(n),
    assertthat::is.number(scale),
    assertthat::noNA(scale),
    assertthat::is.number(intensity),
    assertthat::noNA(intensity),
    assertthat::is.number(sd),
    assertthat::noNA(sd),
    is.function(transform)
  )  

  # rasterize data
  r <- blank_raster_from_dim(x, nrow = 10, ncol = 10)
  r <- simulate_data.SpatRaster(
    r, n = n, 
    scale = scale, 
    intensity = intensity,
    sd = sd, 
    transform = transform
  )
  # extract raster to sf polygons
  ex_sf <- terra::extract(r, terra::vect(x), fun = "mean", na.rm = TRUE, ID = FALSE)
  x <- dplyr::bind_cols(x, ex_sf)
  x <- x[, names(ex_sf), drop = FALSE] # drop _index field

  # return sf with extracted values
  x
}

#' @rdname simulate_data
#' @method simulate_data SpatRaster
#' @export
simulate_data.SpatRaster <- function(
    x, 
    n = 1, 
    scale = 0.5, 
    intensity = 0,
    sd = 1, 
    transform = identity
  ) {
  
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    assertthat::is.count(n),
    terra::global(x, "notNA")[[1]][[1]] > 0,
    assertthat::is.number(scale),
    assertthat::noNA(scale),
    assertthat::is.number(intensity),
    assertthat::noNA(intensity),
    assertthat::is.number(sd),
    assertthat::noNA(sd),
    is.function(transform)
  )
  
  # create object for simulation
  obj <- fields::circulantEmbeddingSetup(
    grid = list(
      x = seq(0, 5, length.out = terra::nrow(x)),
      y = seq(0, 5, length.out = terra::ncol(x))
    ),
    Covariance = "Exponential",
    aRange = scale
  )
  
  # generate populate rasters with values
  r <- terra::rast(lapply(seq_len(n), function(i) {
    ## populate with simulated values
    v <- c(t(fields::circulantEmbedding(obj)))
    v <- transform(v + stats::rnorm(length(v), mean = intensity, sd = sd))
    r <- terra::setValues(x[[1]], v[seq_len(terra::ncell(x))])
    ## apply mask for consistency
    r <- terra::mask(r, x[[1]])
    ## return result
    r
  }))
  ## assign column names
  names(r) <- paste0("V", seq_len(n))
  # return result
  r
}
