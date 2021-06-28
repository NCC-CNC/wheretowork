#' @include internal.R
NULL

#' Add map components
#'
#' Add components to a Leaflet map.
#'
#' @param x [leaflet::leaflet()] or [leaflet::leafletProxy] object.
#'
#' @param `list` of bounding box information.
#'  Defaults to `NULL` such that no bounding box information is used.
#' @return Updated version of argument to `x`.
#'
#' @export
add_leaflet_map_components <- function(x, bbox = NULL) {
  # assert arguments are valid
  print(class(x))
  assertthat::assert_that(
    inherits(x, c("leaflet", "leaflet_proxy")),
    is.null(bbox) || is.list(bbox))
  # set bounding box to entire world if missing bboxox information
  if (is.null(bbox)) {
    bbox <- list(xmin = -180, xmax = 180, ymin = -90, ymax = 90)
  }
  # prepare JS code for button
  fly_to_sites_js <- paste0(
    "function(btn, map){ map.flyToBounds([",
    "[", bbox$ymin, ", ", bbox$xmin, "],",
    "[", bbox$ymax, ", ", bbox$xmax, "]]);}")
  zoom_in_js <- paste0(
    "function(btn, map){",
    "map.setZoom(Math.min(map.getZoom() + 1, map.getMaxZoom()));",
    "}")
  zoom_out_js <- paste0(
    "function(btn, map){",
    "map.setZoom(Math.max(map.getZoom() - 1, map.getMinZoom()));",
    "}")
  # update map
  x %>%
  leaflet::addProviderTiles(
    leaflet::providers$Esri.WorldImagery) %>%
  leaflet::addEasyButton(
    leaflet::easyButton(
      icon = shiny::icon("plus"),
      title = "Zoom in",
      position = "topright",
      onClick = htmlwidgets::JS(zoom_in_js))) %>%
  leaflet::addEasyButton(
    leaflet::easyButton(
      icon = shiny::icon("minus"),
      title = "Zoom out",
      position = "topright",
      onClick = htmlwidgets::JS(zoom_out_js))) %>%
  leaflet::addEasyButton(
    leaflet::easyButton(
      icon = shiny::icon("home"),
      title = "Zoom to data",
      position = "topright",
      onClick = htmlwidgets::JS(fly_to_sites_js))) %>%
  leaflet::addScaleBar(
    position = "bottomright") %>%
  leaflet::addMiniMap(position = "bottomright")
}
