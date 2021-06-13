fluidPage(
  # header
  title = "solutionResults demo",
  titlePanel("solutionResults widget demo"),

  # manual styling for widget
  htmltools::tags$style(
    ".panel {flex: 1 1 33.33%; max-height: 400px}"
  ),
  htmltools::tags$style(
    "#widget {border: 3px solid red; !important}"
  ),

  # body
  htmltools::tags$div(style = "display: flex; justify-content: space-evenly",

  div(
    class = "panel",
    wellPanel(
      h3("Widget"),
      p("This panel contains a minimal version of the solution results widget. Note that it has minimal styling so that we make easily customize it within the Location App. The red outline shows its extent."),
      solutionResultsOutput("widget", height = "60vh")
    )
  ),

  div(
    class = "panel",
    wellPanel(
      h3("State"),
      p("This panel depicts R's internal representation of the solution settings widget."),
      verbatimTextOutput("show")
    )
  ),

  div(
    class = "panel",
    wellPanel(
      h3("Controls"),
        p("This panel contains controls to update the widget. These controls are used to randomly add and remove solutions from the object."),
      actionButton("add_solution_btn", label = "Add new solution"),
      actionButton("drop_solution_btn", label = "Drop solution")
    )
  )
))
