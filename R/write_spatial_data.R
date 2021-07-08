#' @include internal.R
NULL

#' Write spatial data
#'
#' Write spatial data to disk as a zip archive.
#'
#' @param x [sf::st_sf()] or [raster::stack()] object.
#'
#' @param path `character` file path to save zip archive.
#'
#' @param name `character` name of the spatial data inside the zip archive.
#'  Defaults to the file name of `path`.
#'
#' @details
#' This function saves the spatial dataset as a zip archive.
#' Vector data are saved in ESRI Shapefile format and raster data
#' are saved as a GeoTIFF with layer names stored in a plain text file.
#'
#' @return Invisible `TRUE` indicating success.
#'
#' @export
write_spatial_data <- function(
  x, path, name = tools::file_path_sans_ext(basename(path))) {
  assertthat::assert_that(
    inherits(x, c("sf", "Raster")),
    assertthat::is.string(path),
    assertthat::noNA(path),
    endsWith(path, ".zip"),
    assertthat::is.string(name),
    assertthat::noNA(name))
  # create temporary directory
  td <- tempfile()
  dir.create(td, showWarnings = TRUE, recursive = TRUE)
  # save data to disk
  if (inherits(x, "sf")) {
    ## save vector data in ESRI Shapefile format
    sf::write_sf(x, file.path(td, paste0(name, ".shp")))
  } else {
    ## save raster data in GeoTIFF format
    suppressWarnings({
      writeNamedRaster(
        x = x, filename = file.path(td, paste0(name, ".tif")),
        overwrite = TRUE, NAflag = -9999)
    })
  }
  # add files to zip archive
  withr::with_dir(td, utils::zip(path, dir(td), flags = "-qq"))
  # return success
  file.exists(path)
}
