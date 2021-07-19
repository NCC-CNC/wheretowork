#' @include internal.R
NULL

#' Import realistic spatial data
#'
#' Import a realistic spatial vector dataset.
#'
#' @return A [sf::st_sf()] object.
#'
#' @noRd
import_realistic_vector_data <- function() {
  # unzip data
  data_dir <- tempfile()
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(
    system.file(
      "extdata", "data", "ECODISTRICT_V2_2_SHP.zip",
      package = "wheretowork"
    ),
    exdir = data_dir)
  # import data
  x <- sf::read_sf(file.path(data_dir, "ECODISTRICT_V2_2_SHP.shp"))
  # clean data
  x <- sf::st_make_valid(x)
  x$id <- seq_len(nrow(x))
  x <- x[, "id"]
  x <- sf::st_transform(x, 4326)
  # overwrite attributes
  attr(x, "agr") <- NULL
  # return result
  x
}

#' Import simple vector data
#'
#' Import a simple spatial vector dataset.
#'
#' @return A [sf::st_sf()] object.
#'
#' @noRd
import_simple_vector_data <- function() {
  x <-
    sf::st_as_sf(raster::rasterToPolygons(import_simple_raster_data()))
  x <- sf::st_sf(
    tibble::tibble(id = seq_len(nrow(x)), geometry = sf::st_geometry(x)))
  attr(x, "agr") <- NULL
  sf::st_transform(x, 4326)
}

#' Import realistic raster data
#'
#' Import a realistic spatial raster dataset.
#'
#' @return A [raster::raster()] object.
#'
#' @noRd
import_realistic_raster_data <- function() {
  x <- sf::st_transform(import_realistic_vector_data(), 3857)
  suppressWarnings({
    fasterize::fasterize(
      sf = x,
      raster = raster::raster(x, res = 50000)
    )
  })
}

#' Import simple raster data
#'
#' Import a simple spatial raster dataset.
#'
#' @return A [raster::raster()] object.
#'
#' @noRd
import_simple_raster_data <- function() {
  suppressWarnings({
    x <- sf::st_transform(import_realistic_vector_data(), 3857)
    fasterize::fasterize(
      sf = x,
      raster = raster::raster(x, res = 500000)
    )
  })
}
