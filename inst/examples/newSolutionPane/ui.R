tagList(
  # header
  tags$style(
    "body {padding: 0; margin: 0;}"
  ),

  # body
  tagList(
    sidebar_tabs(
      id = "sidebar",
      iconList = list(icon("rocket")),
      newSolutionSidebarPane(
        id = "newSolutionPane",
        solutionSettingsId = "newSolutionPane_settings",
        nameId = "newSolutionPane_name",
        buttonId = "newSolutionPane_button"
      )
    ),
    leafletOutput("map", width = "100vw", height = "100vh")
  )
)
