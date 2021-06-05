fillPage(
  tagList(
    sidebar_tabs(
      id = "sidebar",
      iconList = list(icon("rocket")),
      newSolutionSidebarPane(
        id = "newSolutionPane",
        solutionSettingsId = "newSolutionPane_settings"
      )
    ),
    leafletOutput("map", width = "100%", height = "100%")
  )
)
