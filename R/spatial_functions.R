#' @include internal.R
NULL

#' Shapefile field names
#'
#' Quickly extract the field names in a shapefile.
#'
#' @param x `character` path.
#'
#' @return `character` vector of field names.
#'
#' @examples
#' shapefile_field_names(system.file("shape/nc.shp", package = "sf")
#'
#' @export
shapefile_field_names <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(x),
    endsWith(x, ".shp"))
  # find layer name
  l <- sf::st_layers(x)$name[[1]]
  # prepare query
  qu <- paste0("SELECT * FROM \"", l, "\" WHERE FID <= 2")
  # import shapefile
  s <- sf::read_sf(dsn = x, layer = l, query = qu)
  # return field names (excluding geometry column)
  setdiff(names(s), attr(s, "sf_column"))
}
