fillPage(
  tagList(
    sidebar_tabs(
      id = "sidebar",
      iconList = list(icon("rocket")),
      mapManagerPane(
        id = "mapManagerPane"
      )
    ),
    leafletOutput("map", width = "100%", height = "100%")
  )
)
