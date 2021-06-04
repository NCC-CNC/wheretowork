#' Write a \code{Raster} object to disk
#'
#' @inheritParams raster::writeRaster
#'
#' @param ... arguments passed to \code{\link[raster]{writeRaster}}.
#'
#' @details This file saves a \code{\link{Raster}} object to disk along with
#'   a text file containing the layer names of the raster.
#'
#' @return result from  \code{\link[raster]{writeRaster}}.
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

#' Read a \code{Raster} object from disk
#'
#' @inheritParams raster::stack
#'
#' @details This file reads a \code{\link{Raster}} object to disk along with
#'   a text file containing the layer names of the raster.
#'
#' @return result from \code{\link[raster]{stack}} with names.
#'
#' @export
readNamedRaster <- function(x, ...) {
  assertthat::assert_that(file.exists(raster::extension(x, "txt")),
                          msg = "x is not a named raster")
  n <- readLines(raster::extension(x, "txt"))
  x <- suppressWarnings(raster::stack(x, ...))
  names(x) <- n
  if (raster::nlayers(x) == 1)
    x <- x[[1]]
  x
}
