#' @include internal.R
NULL

#' Simulate realistic spatial data
#'
#' Simulate a realistic spatial vector dataset.
#'
#' @return A [sf::st_sf()] object.
#'
#' @export
simulate_realistic_vector_data <- function() {
  # unzip data
  data_dir <- tempfile()
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(
    system.file(
      "extdata", "ECODISTRICT_V2_2_SHP.zip", package = "locationmisc"),
    exdir = data_dir)

  # import data
  x <- sf::read_sf(file.path(data_dir, "ECODISTRICT_V2_2_SHP.shp"))

  # clean data
  x <- sf::st_make_valid(x)
  x$id <- seq_len(nrow(x))
  x <- x[, "id"]
  x <- sf::st_transform(x, 3857)

  # return result
  x
}

#' Simulate realistic raster data
#'
#' Simulate a realistic spatial raster dataset.
#'
#' @return A [raster::raster()] object.
#'
#' @export
simulate_realistic_raster_data <- function() {
  x <- simulate_realistic_vector_data()
  suppressWarnings({
    fasterize::fasterize(sf = x, raster::raster(x, res = 50000))
  })
}

#' Simulate simple vector data
#'
#' Simulate a simple spatial vector dataset.
#'
#' @return A [sf::st_sf()] object.
#'
#' @export
simulate_simple_vector_data <- function() {
  x <- sf::st_as_sf(raster::rasterToPolygons(simple_spatial_raster_data()))
  x$id <- seq_len(nrow(x))
  x[, c("id")]
}

#' Simulate simple raster data
#'
#' Simulate a simple spatial raster dataset.
#'
#' @return A [raster::raster()] object.
#'
#' @export
simulate_simple_raster_data <- function() {
  suppressWarnings({
    x <- realistic_spatial_vector_data()
    fasterize::fasterize(sf = x, raster::raster(x, res = 500000))
  })
}
