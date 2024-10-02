#' Write a Raster object to disk
#'
#' Save a [terra::rast()] object to disk
#'
#' @param x [terra::rast()] object.
#'
#' @param filename `character` file path.
#'
#' @param ... arguments passed to [terra::writeRaster()].
#'
#' @details This file saves a SpatRaster object to disk along.
#'
#' @return None.
#'
#' @export
writeNamedRaster <- function(x, filename, ...) {
  args <- list(...)
  terra::writeRaster(x, filename, ...)
}
