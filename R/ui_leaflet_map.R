#' @include internal.R
NULL

#' Leaflet map
#'
#' Create a leaflet map with customized default settings.
#'
#' @param sidebar_ids `character` vector containing HTML identifiers for
#' the left and right sidebars.
#'
#' @return [leaflet::leaflet()] object.
#'
#' @export
leaflet_map <- function(sidebar_ids) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(sidebar_ids),
    length(sidebar_ids) == 2,
    assertthat::noNA(sidebar_ids)
  )

  # prepare JS code for buttons
  home_js <- paste0(
    "function(btn, map) {",
    "Shiny.setInputValue(\"home_button\", Math.random());",
    "}"
  )
  hide_js <- paste0(
    "function(btn, map) {",
    "Shiny.setInputValue(\"hide_button\", Math.random());",
    "}"
  )
  show_js <- paste0(
    "function(btn, map) {",
    "Shiny.setInputValue(\"show_button\", Math.random());",
    "}"
  )
  print_js <- paste0(
    "function(btn, map) {",
    "Shiny.setInputValue(\"print_button\", Math.random());",
    "}"
  )
  help_js <- paste0(
    "function(btn, map) {",
    "Shiny.setInputValue(\"help_button\", Math.random());",
    "}"
  )

  # create map
  map <-
    ## initialize leaflet map
    leaflet::leaflet() %>%
    ## add basemaps
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.Positron,
      group = "Gray scale"
    ) %>%
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldImagery,
      group = "Satellite"
    ) %>%
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldStreetMap,
      group = "Street view"
    ) %>%
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.DarkMatter,
      group = "Monochrome"
    ) %>%
    ## specify default view window
    leaflet::flyToBounds(
      -165, -30, 165, 60
    ) %>%
    ## set map bounds
    leaflet::setMaxBounds(
      -200, -100, 200, 100
    ) %>%
    ## add home button
    leaflet::addEasyButton(
      leaflet::easyButton(
        title = "Zoom to home",
        icon = shiny::icon("home"),
        position = "topleft",
        onClick = htmlwidgets::JS(home_js)
      )
    ) %>%
    ## show/hide layers buttons
    leaflet::addEasyButtonBar(
      leaflet::easyButton(
        id = "hideAllLayers",
        title = "Hide all layers",
        icon = shiny::icon("eye-slash"),
        position = "topleft",
        onClick = htmlwidgets::JS(hide_js)
      ),
      leaflet::easyButton(
        id = "showAllLayers",
        title = "Show all layers",
        icon = shiny::icon("eye"),
        position = "topleft",
        onClick = htmlwidgets::JS(show_js)
      )
    ) %>%
    ## add help button
    leaflet::addEasyButton(
      leaflet::easyButton(
        title = "Help",
        icon = shiny::icon("question"),
        position = "topleft",
        onClick = htmlwidgets::JS(help_js)
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
    ## add basemap controls
    leaflet::addLayersControl(
      baseGroups = c("Gray scale", "Satellite", "Street view", "Monochrome"),
      options = leaflet::layersControlOptions(collapsed = TRUE),
      position = "topleft"
    ) %>%
    ## add history buttons
    leaflet.extras2::addHistory(
      options = leaflet.extras2::historyOptions(position = "topleft")
    ) %>%
    ## add scale bar
    leaflet::addScaleBar(
      position = "bottomleft"
    )

  # add sidebars
  map <-
    map %>%
    leaflet.extras2::addSidebar(
      id =  sidebar_ids[[1]],
      options = list(position = "left", fit = FALSE)
    ) %>%
    leaflet.extras2::addSidebar(
      id = sidebar_ids[[2]],
      options = list(position = "right", fit = FALSE)
    )

  # remove outdated font awesome dependency
  idx <- vapply(
    map$dependencies,
    FUN.VALUE = logical(1),
    function(x) x$name == "fontawesome" && x$version == "4.7.0"
  )
  map$dependencies <- map$dependencies[which(!idx)]

  # add custom JS code to make Bootstrap tooltips
  map <- htmlwidgets::onRender(map, "
  function(el, x) {
     document
     .querySelectorAll('.leaflet-top *[title]:not([title=\"\"]')
     .forEach((x) => {
       $(x).tooltip({
         trigger: 'hover',
         placement: 'top',
         container: 'body',
         placement: 'bottom'
      });
     });
  }")

  # return result
  map
}
