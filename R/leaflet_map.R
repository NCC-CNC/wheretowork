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
  # prepare JS code for buttons
  home_js <- paste0(
    "function(btn, map) {",
    "Shiny.setInputValue(\"home_button\", Math.random());",
    "}")
  hide_js <- paste0(
    "function(btn, map) {",
    "Shiny.setInputValue(\"hide_button\", Math.random());",
    "}")
  print_js <- paste0(
    "function(btn, map) {",
    "Shiny.setInputValue(\"print_button\", Math.random());",
    "}")
  # create map
  map <-
    ## initialize leaflet map
    leaflet::leaflet() %>%
    ## add basemap
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldImagery) %>%
    ## specify default view window
    leaflet::flyToBounds(
      -165, -30, 165, 60) %>%
    ## add home button
    leaflet::addEasyButton(
      leaflet::easyButton(
        title = "Zoom to home",
        icon = shiny::icon("home"),
        position = "topleft",
        onClick = htmlwidgets::JS(home_js)
      )
    ) %>%
    ## add hide layers button
    leaflet::addEasyButton(
      leaflet::easyButton(
        title = "Hide layers",
        icon = shiny::icon("eye-slash"),
        position = "topleft",
        onClick = htmlwidgets::JS(hide_js)
      )
    ) %>%
    ## add screenshot button
    leaflet.extras2::addEasyprint(
      options = leaflet.extras2::easyprintOptions(
        exportOnly = TRUE,
        hidden = TRUE
      )
    ) %>%
    leaflet::addEasyButton(
      leaflet::easyButton(
        title = "Take screenshot",
        icon = shiny::icon("print"),
        position = "topleft",
        onClick = htmlwidgets::JS(print_js)
      )
    ) %>%
    ## add history buttons
    leaflet.extras2::addHistory(
      options = leaflet.extras2::historyOptions(position = "topleft")) %>%
    ## add scale bar
    leaflet::addScaleBar(
      position = "bottomleft") %>%
    ## add minimap
    leaflet::addMiniMap(
      position = "bottomleft",
      width = 174, height = 150)
  # remove outdated font awesome dependency
  idx <- vapply(
    map$dependencies, FUN.VALUE = logical(1),
    function(x) x$name == "fontawesome" && x$version == "4.7.0")
  map$dependencies <- map$dependencies[which(!idx)]
  # return result
  map
}
