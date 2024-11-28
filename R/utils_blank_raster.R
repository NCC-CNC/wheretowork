#' @include internal.R
NULL

#' SpatRaster from sf dimensions
#'
#' @param x `sf` object.
#'
#' @param nrow `numeric` value defining the number of SpatRaster rows
#' 
#' @param ncol `numeric` value defining the number of SpatRaster columns
#'
#' @examples
#' # import vector
#' v <- import_simple_vector_data()
#'
#' # generate SpatRaster 
#' r <- blank_raster_from_dim(v, nrow = 10, ncol = 10)
#' 
#' # print results
#' print(x)
#' 
#' @noRd
blank_raster_from_dim <- function(x, nrow, ncol) {
  assertthat::assert_that(
    inherits(x, "sf"), 
    assertthat::is.count(nrow),
    assertthat::is.count(ncol)
  )
  # determine number of rows and columns
  y <- terra::vect(x)
  xres <- ceiling((terra::xmax(y) - terra::xmin(y)) / ncol)
  yres <- ceiling((terra::ymax(y) - terra::ymin(y)) / nrow)
  # return result
  blank_raster_from_res(x, c(xres, yres))
}

#' SpatRaster from sf dimensions with resolution
#'
#' @param x `sf` object.
#'
#' @param res `numeric` value defining the SpatRaster resolution
#'
#' @examples
#' # import vector
#' v <- import_simple_vector_data()
#'
#' # generate SpatRaster 
#' r <- blank_raster_from_res(v, res = 5000)
#' 
#' # print results
#' print(x)
#' 
#' @noRd
blank_raster_from_res <- function(x, res) {
  assertthat::assert_that(
    inherits(x, "sf"), 
    is.numeric(res),
    all(is.finite(res)), length(res) %in% c(1, 2)
  )
  # initialize resolution inputs
  if (length(res) == 1) {
    res <- c(res, res)
  }
  
  y <- terra::vect(x)
  
  # extract coordinates
  if ((terra::xmax(y) - terra::xmin(y)) <= res[1]) {
    xpos <- c(terra::xmin(y), res[1])
  } else {
    xpos <- seq(
      terra::xmin(y),
      terra::xmax(y) + (res[1] * (((terra::xmax(y) - terra::xmin(y)) %% res[1]) != 0)),
      res[1])
    }
  if ((terra::ymax(y) - terra::ymin(y)) <= res[2]) {
    ypos <- c(terra::ymin(y), res[2])
  } else {
    ypos <- seq(
      terra::ymin(y),
      terra::ymax(y) + (res[2] * (((terra::ymax(y) - terra::ymin(y)) %% res[2]) != 0)),
      res[2])
  }
  
  # generate raster from sf
  rast <- terra::rast(
    xmin = min(xpos),
    xmax = max(xpos),
    ymin = min(ypos),
    ymax = max(ypos),
    nrows = length(ypos) - 1,
    ncols = length(xpos) - 1,
    crs = terra::crs(y)
  )
  # return raster
  terra::setValues(rast, 1)
}
