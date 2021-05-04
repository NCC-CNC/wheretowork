library(shiny)
library(locationmisc)

fluidPage(
  title = "locationmisc demo",
  htmltools::tags$style(".well {flex-basis: 30%; max-height: 80vh}"),
  titlePanel("locationmisc demo"),

  tabsetPanel(
    type = "tabs",

    # weight widget demo
    tabPanel("weight factor",
      tags$div(style = "display:flex;",
        wellPanel(
          h3("widget"),
          weightFactorOutput("weightFactor_widget")
        ),

        wellPanel(
          h3("state"),
          verbatimTextOutput("weightFactor_show_name"),
          verbatimTextOutput("weightFactor_show_status"),
          verbatimTextOutput("weightFactor_show_factor"),
        ),

        wellPanel(
          h3("controls"),
          ### name
          fluidRow(
            column(3,
              textInput(
                "weightFactor_control_name_input", label = NULL),
            ),
            column(3,
              actionButton(
                "weightFactor_control_name_button", label = "Update name")
            )
          ),
          ### status
          fluidRow(
            column(3,
              checkboxInput(
              "weightFactor_control_status_input", label = NULL, TRUE)
            ),
            column(3,
              actionButton(
                "weightFactor_control_status_button", label = "Update status")
            )
          ),
          ### factor
          fluidRow(
            column(3,
              numericInput(
                "weightFactor_control_factor_input", label = NULL,
                value = 0.3, min = 0, max = 1, step = 5),
            ),
            column(3,
              actionButton(
                "weightFactor_control_factor_button", label = "Update factor")
            )
          )
        )
      )
    ),

    # singleThemeGoal widget demo
    tabPanel("theme goal (single feature)",
      tags$div(style = "display:flex;",
        wellPanel(
          h3("widget"),
          singleThemeGoalOutput("singleThemeGoal_widget")
        ),

        wellPanel(
          h3("state"),
          verbatimTextOutput("singleThemeGoal_show_status"),
          verbatimTextOutput("singleThemeGoal_show_goal")
        ),

        wellPanel(
          h3("controls"),
          ### name
          fluidRow(
            column(3,
              textInput(
                "singleThemeGoal_control_name_input", label = NULL),
            ),
            column(3,
              actionButton(
                "singleThemeGoal_control_name_button", label = "Update name")
            )
          ),
          ### status
          fluidRow(
            column(3,
              checkboxInput(
              "singleThemeGoal_control_status_input", label = NULL, TRUE)
            ),
            column(3,
              actionButton(
                "singleThemeGoal_control_status_button", label = "Update status")
            )
          ),
          ### goal
          fluidRow(
            column(3,
              numericInput(
                "singleThemeGoal_control_goal_input", label = NULL,
                value = 0.3, min = 0, max = 1, step = 5),
            ),
            column(3,
              actionButton(
                "singleThemeGoal_control_goal_button", label = "Update goal")
            ),
            column(4,
              p("Note that minimum goal limit is 0.1")
            )
          )
        )
      )
    ),

    # multiThemeGoal widget demo
    tabPanel("theme goal (multiple features)",
      tags$div(style = "display:flex;",
        wellPanel(
          h3("widget"),
          multiThemeGoalOutput("multiThemeGoal_widget"),
        ),

        wellPanel(
          h3("state"),
          verbatimTextOutput("multiThemeGoal_show_status"),
          verbatimTextOutput("multiThemeGoal_show_goal"),
        ),

        wellPanel(
          h4("theme"),
          ### name
          fluidRow(
            column(3,
              textInput(
                "multiThemeGoal_control_name_input", label = NULL),
            ),
            column(3,
              actionButton(
                "multiThemeGoal_control_name_button", label = "Update name")
            )
          ),
          ### status
          fluidRow(
            column(3,
              checkboxInput(
                "multiThemeGoal_control_status_input", label = NULL, TRUE)
            ),
            column(3,
              actionButton(
                "multiThemeGoal_control_status_button", label = "Update status")
            )
          ),
          ### view
          fluidRow(
            column(3,
              radioButtons("multiThemeGoal_control_view_input",
                "View:", inline = TRUE,
                 c("group" = "group", "single" = "single"))
              ),
            column(3,
              actionButton(
                "multiThemeGoal_control_view_button", label = "Update view")
            )
          ),
          ### group view
          h4("Group view"),
          fluidRow(
            column(3,
              numericInput(
                "multiThemeGoal_control_group_goal_input", label = NULL,
                value = 0.3, min = 0, max = 1, step = 5),
            ),
            column(3,
              actionButton(
                "multiThemeGoal_control_group_goal_button",
                label = "Update goal")
            ),
            column(4,
              p("Note that minimum goal limit is 0.1")
            )
          ),
          ## single view
          h4("Single view"),
          fluidRow(
            h5("Goals"),
            column(5,
              numericInput(
                "multiThemeGoal_control_F1_goal_input", label = "forest",
                value = 0.3, min = 0, max = 1, step = 5),
              numericInput(
                "multiThemeGoal_control_F2_goal_input", label = "shrubland",
                value = 0.2, min = 0, max = 1, step = 5),
              numericInput(
                "multiThemeGoal_control_F3_goal_input", label = "grassland",
                value = 0.4, min = 0, max = 1, step = 5)
            ),
            column(1,
              actionButton(
                "multiThemeGoal_control_feature_goal_button",
                label = "Update goals"
              )
            )
          ),
          fluidRow(
            h5("Status"),
            column(5,
              checkboxInput(
                "multiThemeGoal_control_F1_status_input",
                label = "forest", TRUE),
              checkboxInput(
                "multiThemeGoal_control_F2_status_input",
                label = "shrubland", TRUE),
              checkboxInput(
                "multiThemeGoal_control_F3_status_input",
                label = "grassland", TRUE)
            ),
            column(1,
              actionButton(
                "multiThemeGoal_control_feature_status_button",
                label = "Update statuses"
              )
            )
          )
        )
      )
    ),

    # multiThemeGoal2 widget demo
    tabPanel("theme goal (many features)",
      tags$div(style = "display:flex;",
        wellPanel(
          h3("widget"),
          multiThemeGoalOutput("multiThemeGoal2_widget"),
        )
      )
    )

  )
)
