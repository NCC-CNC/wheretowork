fillPage(
  htmltools::htmlDependency(
    name = "Styles",
    version = "1.0.0",
    src = system.file("styles", package = "locationmisc"),
    stylesheet = c("leaflet.css")
  ),
  tagList(
    sidebar_tabs(
      id = "sidebar",
      iconList = list(icon("bars")),
      mapManagerSidebarPane(
        id = "mapManagerPane"
      )
    ),
    leaflet::leafletOutput("map", width = "100%", height = "100%")
  )
)
