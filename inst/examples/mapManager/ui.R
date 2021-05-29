fluidPage(
  # header
  title = "mapManager demo",
  titlePanel("mapManager widget demo"),

  # manual styling for widget
  htmltools::tags$style(
    ".panel {flex: 1 1 33.33%; max-height: 400px}"
  ),
  htmltools::tags$style(
    ".mapManager {border: 3px solid red; !important}"
  ),

  # body
  htmltools::tags$div(style = "display: flex; justify-content: space-evenly",

  div(
    class = "panel",
    wellPanel(
      h3("Widget"),
      p("This panel contains a minimal version of the map manager widget. Note that it has minimal styling so that we make easily customize it within the Location App. The red outline shows its extent."),
      solutionSettingsOutput("widget", height = "60vh")
    )
  ),

  div(
    class = "panel",
    wellPanel(
      h3("State"),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "R's internal memory",
          br(),
          p("This tab depicts R's internal representation of the solution settings widget."),
          verbatimTextOutput("show")
        ),
        tabPanel(
          "Message from widget",
          br(),
          p("This tab shows the messages that the widget is sending to R to update R's internal representation of the solution settings."),
          verbatimTextOutput("message"),
        )
      )
    )
  ),

  div(
    class = "panel",
    wellPanel(
      h3("Controls"),
        p("This tab contains controls to manually update the appearance of the widget. Note that these controls (mostly) do not alter R's internal representation of solution settings. This is because they are designed to be used within an R Shiny session to update the widget automatically for the user."),
      tabsetPanel(
        type = "tabs",
        ## single theme
        tabPanel(
          "Species",
          br(),
          fluidRow(
            column(3,
              textInput("st_name_input", label = NULL),
            ),
            column(1,
              actionButton("st_name_button", label = "Update name")
            )
          ),
          fluidRow(
            column(3,
              checkboxInput("st_visible_input", label = NULL, TRUE),
            ),
            column(1,
              actionButton("st_visible_button", label = "Update visible")
            )
          ),
        ),

        ### multi theme
        tabPanel(
          "Ecoregions",
          br(),
          fluidRow(
            column(3,
              textInput(
                "mt_name_input", label = NULL
              ),
            ),
            column(1,
              actionButton("mt_name_button", label = "Update name")
            )
          ),
          fluidRow(
            column(3,
              checkboxInput("mt_visible_input", label = NULL, TRUE),
            ),
            column(1,
              actionButton("mt_visible_button", label = "Update visible")
            )
          ),
          wellPanel(
            fluidRow(
              column(3,
                checkboxInput(
                  "mt1_feature_visible_input", label = "forest", TRUE),
                checkboxInput(
                  "mt2_feature_visible_input", label = "shrubs", TRUE),
              ),
              column(1,
                actionButton(
                  "mt_feature_visible_button", label = "Update feature visible"
                )
              )
            )
          )
        ),

        ### weight
        tabPanel(
          "Human Footprint Index",
          br(),
          fluidRow(
            column(3,
              textInput("w_name_input", label = NULL),
            ),
            column(1,
              actionButton("w_name_button", label = "Update name")
            )
          ),
          fluidRow(
            column(3,
              checkboxInput("w_visible_input", label = NULL, TRUE),
            ),
            column(1,
              actionButton("w_visible_button", label = "Update visible")
            )
          )
        )
      )
    )
  )
))
