library(shiny)
library(locationmisc)

fluidPage(
  # header
  title = "solutionSettings demo",
  titlePanel("solutionSettings widget demo"),

  # manual styling for widget
  htmltools::tags$style(
    ".panel {flex: 1 1 33.33%; max-height: 80vh}"
  ),
  htmltools::tags$style(
    ".solution-settings {border: 3px solid red; !important}"
  ),

  # body
  htmltools::tags$div(style = "display: flex; justify-content: space-evenly",

  div(
    class = "panel",
    wellPanel(
      h3("widget"),
      p("This panel contains a minimal version of the solution settings widget. Note that it has minimal styling so that we make easily customize it later within the Location-App. The red outline shows the widget."),
      solutionSettingsOutput("widget")
    )
  ),

  div(
    class = "panel",
    wellPanel(
      h3("state"),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "R's internal memory",
          p("This tab depicts R's internal representation of the solution settings widget."),
          verbatimTextOutput("show")
        ),
        tabPanel(
          "Message from widget",
          p("This tab shows the messages that the widget is sending to R through the power of Shiny and JavaScript."),
          verbatimTextOutput("message"),
        )
      )
    )
  ),

  div(
    class = "panel",
    wellPanel(
      h3("controls"),
        p("This tab contains controls to manually update the appearance of the widget. For brevity, controls have not been included for the different Turnip features."),
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
              checkboxInput("st_status_input", label = NULL, TRUE),
            ),
            column(1,
              actionButton("st_status_button", label = "Update status")
            )
          ),
          fluidRow(
            column(3,
              numericInput(
                "st_goal_input", label = NULL,
                value = f1$get_goal(), min = 0, max = 1, step = 0.01)
            ),
            column(3,
              actionButton("st_goal_button", label = "Update goal")
            )
          )
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
              checkboxInput("mt_status_input", label = NULL, TRUE),
            ),
            column(1,
              actionButton("mt_status_button", label = "Update status")
            )
          ),
          fluidRow(
            column(3,
              radioButtons(
                "mt_view_input", NULL,
                c("group" = "group", "single" = "single")
              )
            ),
            column(1,
              actionButton("mt_view_button", label = "Update view")
            )
          ),
          fluidRow(
            column(3,
              numericInput(
                "mt_group_goal_input", label = NULL,
                value = t2$group_initial_goal, min = 0, max = 1, step = 0.01)
            ),
            column(1,
              actionButton("mt_group_goal_button", label = "Update group goal")
            )
          ),
          wellPanel(
            fluidRow(
              column(3,
                checkboxInput(
                  "mt1_feature_status_input", label = "forest", TRUE),
                checkboxInput(
                  "mt2_feature_status_input", label = "shrubs", TRUE),
              ),
              column(1,
                actionButton(
                  "mt_feature_status_button", label = "Update feature status"
                )
              )
            )
          ),
          wellPanel(
            fluidRow(
              column(3,
                numericInput(
                  "mt1_feature_goal_input", label = "forest",
                  value = f2$get_goal(), min = 0, max = 1, step = 0.01),
                numericInput(
                  "mt2_feature_goal_input", label = "shrubs",
                  value = f3$get_goal(), min = 0, max = 1, step = 0.01),
              ),
              column(1,
                actionButton(
                  "mt_feature_goal_button", label = "Update feature goals"
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
              checkboxInput("w_status_input", label = NULL, TRUE),
            ),
            column(1,
              actionButton("w_status_button", label = "Update status")
            )
          ),
          fluidRow(
            column(3,
              numericInput(
                "w_factor_input", label = NULL,
                value = w$get_factor(), min = 0, max = 100, step = 0.01),
            ),
            column(1,
              actionButton("w_factor_button", label = "Update factor")
            )
          )
        )
      )
    )
  )
))
