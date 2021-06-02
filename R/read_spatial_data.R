#' @include internal.R
NULL

#' Read spatial data
#'
#' Import spatial data from disk.
#'
#' @param x `character` file path.
#'
#' @details
#' This function imports data in vector format as a [sf::st_sf()] object.
#' It also imports data in raster format as a [raster::stack()] object.
#'
#' @return A [sf::st_sf()] or [raster::stack()] object.
#'
#' @examples
#' # read raster data
#' f1 <- system.file("external/rlogo.grd", package = "raster")
#' read_spatial_data(f1)
#'
#' # read vector data
#' f2 <- system.file("shape/nc.shp", package = "sf")
#' read_spatial_data(f2)
#'
#' @export
read_spatial_data <- function(x) {
  # assert argument is valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x))
  # deduce format automatically
  rast_ext <- c("grd", "asc", "sdat", "rst", "nc", "tif", "envi", "bil", "img")
  if (tools::file_ext(x) %in% rast_ext) {
    out <- raster::stack(x)
  } else {
    suppressMessages({
      out <- sf::read_sf(x)
    })
  }
  out
}
