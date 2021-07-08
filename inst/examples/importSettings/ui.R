fluidPage(
  # header
  title = "importSettings demo",
  titlePanel("importSettings widget demo"),

  # manual styling for widget
  htmltools::tags$style(
    ".panel {flex: 1 1 33.33%; max-height: 400px}"
  ),
  htmltools::tags$style(
    ".importSettings {border: 3px solid red; !important}"
  ),

  # body
  htmltools::tags$div(style = "display: flex; justify-content: space-evenly",

  div(
    class = "panel",
    wellPanel(
      h3("Widget"),
      p("This panel contains a minimal version of the import settings widget. Note that it has minimal styling so that we make easily customize it within the Location App. The red outline shows its extent."),
      importSettingsOutput("widget", height = "60vh")
    )
  ),

  div(
    class = "panel",
    wellPanel(
      "Message from widget",
      br(),
      p("This tab shows the messages that the widget is sending to R to update R's internal representation of the solution settings."),
      verbatimTextOutput("message"),
    )
  )

))
