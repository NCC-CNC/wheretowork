library(shiny)
library(locationmisc)

fluidPage(
  # header
  title = "solutionSettings demo",
  htmltools::tags$style(".well {flex-basis: 30%; max-height: 80vh}"),

  # body
  titlePanel("locationmisc demo"),
  htmltools::tags$div(style = "display: flex;",

  wellPanel(
    h3("widget"),
    solutionSettingsOutput("widget")
  ),

  wellPanel(
    h3("state"),
    h5("message from widget"),
    verbatimTextOutput("message"),
    br(),
    h5("R class"),
    verbatimTextOutput("show")
  ),

  wellPanel(
    h3("controls"),
    tabsetPanel(
      type = "tabs",
      ## single theme
      tabPanel(
        "Species",
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
        fluidRow(
          h5("Goals"),
          column(3,
            numericInput(
              "mt1_goal_input", label = "forest",
              value = f2$get_goal(), min = 0, max = 1, step = 0.01),
            numericInput(
              "mt2_goal_input", label = "shrubs",
              value = f3$get_goal(), min = 0, max = 1, step = 0.01),
          ),
          column(1,
            actionButton(
              "mt_goal_button", label = "Update"
            )
          )
        ),
        fluidRow(
          h5("Statuses"),
          column(3,
            checkboxInput(
              "mt1_goal_input", label = "forest", TRUE),
            checkboxInput(
              "mt2_goal_input", label = "shrubs", TRUE),
          ),
          column(1,
            actionButton(
              "mt_status_button", label = "Update"
            )
          )
        ),
        fluidRow(
          h5("Name"),
          column(3,
            textInput(
              "mt_name_input", label = NULL
            ),
          ),
          column(1,
            actionButton("mt_name_button", label = "Update")
          )
        )
      ),

      ### weight
      tabPanel(
        "Human Footprint Index",
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
              "w_goal_input", label = NULL,
              value = w$get_factor(), min = 0, max = 100, step = 0.01),
          ),
          column(1,
            actionButton("w_goal_button", label = "Update factor")
          )
        )
      )
    )
  )
))
