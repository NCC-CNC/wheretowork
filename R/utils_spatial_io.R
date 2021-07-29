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
#' It also imports data in raster format as a [raster::raster()] object.
#' Note that is will only import the first band in a multi-band raster dataset.
#'
#' @return A [sf::st_sf()] or [raster::raster()] object.
#'
#' @examples
#' # read raster data
#' f1 <- system.file("external/rlogo.grd", package = "raster")
#' read_spatial_data(f1)
#'
#' # read vector data
#' f2 <- system.file("shape/nc.shp", package = "sf")
#' read_spatial_data(f2)
#' @export
read_spatial_data <- function(x) {
  # assert argument is valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x)
  )
  # deduce format automatically
  rast_ext <- c("grd", "asc", "sdat", "rst", "nc", "tif", "envi", "bil", "img")
  if (tools::file_ext(x) %in% rast_ext) {
    out <- raster::raster(x)
  } else {
    suppressMessages({
      out <- sf::read_sf(x)
      if ("geom" %in% names(out)) {
        names(out)[names(out) == "geom"] <- "geometry"
        attr(out, "sf_column") <- "geometry"
      }
      attr(out, "agr") <- NULL
    })
  }
  # return result
  out
}

#' Write spatial data
#'
#' Write spatial data to disk.
#'
#' @param x [sf::st_sf()] or [raster::stack()] object.
#'
#' @param dir `character` directory to save data.
#'
#' @param name `character` name of the spatial data file.
#'
#' @return Invisible `TRUE` indicating success.
#'
#' @examples
#' # read and write raster data
#' f1 <- system.file("external/rlogo.grd", package = "raster")
#' d1 <- read_spatial_data(f1)
#' write_spatial_data(d1, tempdir(), "rlogo")
#'
#' # read and write vector data
#' f2 <- system.file("shape/nc.shp", package = "sf")
#' read_spatial_data(f2)
#' write_spatial_data(d1, tempdir(), "nc")
#'
#' @export
write_spatial_data <- function(x, dir, name) {
  assertthat::assert_that(
    inherits(x, c("sf", "Raster")),
    assertthat::is.string(dir),
    assertthat::noNA(dir),
    assertthat::is.string(name),
    assertthat::noNA(name)
  )
  # save data to disk
  if (inherits(x, "sf")) {
    ## save vector data in ESRI Shapefile format
    sf::write_sf(x, file.path(dir, paste0(name, ".shp")))
  } else {
    ## save raster data in GeoTIFF format
    suppressWarnings({
      writeNamedRaster(
        x = x, filename = file.path(dir, paste0(name, ".tif")),
        overwrite = TRUE, NAflag = -9999
      )
    })
  }
  # return success
  invisible(TRUE)
}
