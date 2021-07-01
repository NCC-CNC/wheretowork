#' @include internal.R
NULL

#' Leaflet map
#'
#' Create a leaflet map with customized default settings.
#'
#' @return [leaflet::leaflet()] object.
#'
#' @export
leaflet_map <- function() {
  # prepare JS code for button
  home_js <- paste0(
    "function(btn, map) {",
    "Shiny.setInputValue(\"home_button\", Math.random());",
    "}")
  # create map
  leaflet::leaflet() %>%
  leaflet::addProviderTiles(
    leaflet::providers$Esri.WorldImagery) %>%
  leaflet::flyToBounds(
    -165, -30, 165, 60) %>%
  leaflet::addEasyButton(
    leaflet::easyButton(
      icon = shiny::icon("home"),
      position = "topleft",
      onClick = htmlwidgets::JS(home_js))) %>%
  leaflet.extras2::addHistory(
    options = leaflet.extras2::historyOptions(position = "topleft")) %>%
  leaflet::addScaleBar(
    position = "bottomleft") %>%
  leaflet::addMiniMap(
    position = "bottomleft",
    width = 174, height = 150)
}
