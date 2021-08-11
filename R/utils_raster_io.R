#' Write a Raster object to disk
#'
#' Save a [raster::raster()] object to disk, along with a text file containing
#' the names of each layer in the object.
#'
#' @param x [raster::raster()] object.
#'
#' @param filename `character` file path.
#'
#' @param ... arguments passed to [raster::writeRaster()].
#'
#' @details This file saves a raster object to disk along with
#'   a text file containing the layer names of the raster.
#'
#' @return None.
#'
#' @export
writeNamedRaster <- function(x, filename, ...) {
  args <- list(...)
  if ((!file.exists(raster::extension(filename, "txt"))) ||
    isTRUE(args$overwrite)) {
    writeLines(names(x), raster::extension(filename, "txt"))
  }
  raster::writeRaster(x, filename, ...)
}

#' Read a Raster object from disk
#'
#' Read a [raster::raster()] object from disk, along with a text file containing
#' the names of each layer in the object.
#'
#' @param x [raster::raster()] object.
#'
#' @param ... arguments passed to [raster::stack].
#'
#' @details This file reads a raster object to disk along with
#'   a text file containing the layer names of the raster.
#'
#' @return A raster object.
#'
#' @export
readNamedRaster <- function(x, ...) {
  assertthat::assert_that(file.exists(raster::extension(x, "txt")),
    msg = "x is not a named raster"
  )
  n <- readLines(raster::extension(x, "txt"))
  x <- suppressWarnings(raster::stack(x, ...))
  names(x) <- n
  if (raster::nlayers(x) == 1) {
    x <- x[[1]]
  }
  x
}
