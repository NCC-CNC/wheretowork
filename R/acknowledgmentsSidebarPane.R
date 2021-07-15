#' @include internal.R solutionSettings.R
NULL

#' Acknowledgments sidebar pane
#'
#' Constructs a sidebar pane for displaying acknowledgements.
#'
#' @inheritParams solutionResultsSidebarPane
#'
#' @inherit solutionResultsSidebarPane details return
#'
#' @examples
#' \dontrun{
#' # run Shiny app to demo the sidebar pane
#' if (interactive()) {
#'   runExample("allSidebars")
#' }
#' }
#'
#' @export
acknowledgmentsSidebarPane <- function(id) {
  # assert arguments are valid
  assertthat::assert_that(
    ### id
    assertthat::is.string(id),
    assertthat::noNA(id))

  # create sidebar widget
  ## create sidebar
  x <-
    leaflet.extras2::sidebar_pane(
      title = "Acknowledgments",
      id = id,
      icon = NULL,
      htmltools::tags$div(
        class = "sidebar-pane-content",
        htmltools::tags$div(
          class = "sidebar-pane-inner",
        htmltools::tags$div(
          class = "generic-container",
            ## NCC acknowledgments
            htmltools::tags$p(
              class = "text-justify",
              "INSERT NCC ACKNOWLEDGMENTS HERE",
              .noWS = c("after-begin", "before-end")
            ),
            ## RCS acknowledgments
            htmltools::tags$p(
              class = "text-justify",
              "This work was supported by the ",
              htmltools::tags$a(
                href = "https://carleton.ca/rcs/our-team/",
                target = "_blank",
                "Research Software Development Team",
                .noWS = "outside"
              ),
              " from ",
              htmltools::tags$a(
                href = "https://carleton.ca/rcs/",
                target = "_blank",
                "Research Computing Services",
                .noWS = "outside"
              ),
              " at Carleton University. Developers ",
              htmltools::tags$a(
                href = "https://github.com/HanqingZhou",
                target = "_blank",
                "Kevin Zhou",
                .noWS = "outside"
              ),
              ", ",
              htmltools::tags$a(
                href = "https://github.com/tanvirislamcu",
                target = "_blank",
                "Tanvir Islam",
                .noWS = "outside"
              ),
              ", and Jazmin Romero supported this work by enhancing existing ",
              "widgets and implementing new widgets for the front-end of this ",
              "application. For more information about the work done by the ",
              " RCS Research Software Development Team, please click ",
              htmltools::tags$a(
                href = "https://carleton.ca/rcs/research-software-development/",
                target = "_blank",
                "here",
                .noWS = "outside"
              ),
              ". RCS can be contacted ",
              htmltools::tags$a(
                href = "https://carleton.ca/rcs/contact/",
                target = "_blank",
                "here",
                .noWS = "outside"
              ),
              ".",
              .noWS = c("after-begin", "before-end")
            ),
            ## main dependencies
            htmltools::tags$p(
              class = "text-justify",
              "This application was built using the ",
              htmltools::tags$a(
                href = "https://shiny.rstudio.com/",
                target = "_blank",
                "Shiny web application framework",
                .noWS = "outside"
              ),
              " for the ",
              htmltools::tags$a(
                href = "https://www.r-project.org/",
                target = "_blank",
                "R statistical computing environment",
                .noWS = "outside"
              ),
              ".  It uses the ",
              htmltools::tags$a(
                href = "https://prioritizr.net/",
                target = "_blank",
                "prioritizr R package",
                .noWS = "outside"
              ),
              " and the ",
              htmltools::tags$a(
                href = "https://www.coin-or.org/Cbc/",
                target = "_blank",
                "CBC (Coin-OR branch and cut) optimization solver",
                .noWS = "outside"
              ),
              " to generate solutions.",
              " It also uses the ",
              htmltools::tags$a(
                href = "https://datatables.net/",
                target = "_blank",
                "DataTables",
                .noWS = "outside"
              ),
              ", ",
              htmltools::tags$a(
                href = "https://d3js.org/",
                target = "_blank",
                "D3 data visualization",
                .noWS = "outside"
              ),
              ", ",
              htmltools::tags$a(
                href = "http://github.evanliu2968.com.cn/el-checkbox/",
                target = "_blank",
                "el checkbox",
                .noWS = "outside"
              ),
              ", ",
              htmltools::tags$a(
                href = "https://refreshless.com/nouislider/",
                target = "_blank",
                "noUiSlider",
                .noWS = "outside"
              ),
              ", ",
              htmltools::tags$a(
                href = "https://sortablejs.github.io/Sortable/",
                target = "_blank",
                "SortableJS",
                .noWS = "outside"
              ),
              ", and ",
              htmltools::tags$a(
                href = "https://refreshless.com/wnumb/",
                target = "_blank",
                "wNumb",
                .noWS = "outside"
              ),
              " JavaScript libraries.",
              .noWS = c("after-begin", "before-end"),
            ),
            ## additional dependencies
            acknowledge_packages(
              x = setdiff(
                rownames(installed.packages()),
                c("prioritizr", "shiny", "locationmisc")
              ),
              prefix = "It also uses the following R packages: ",
              suffix = "."
            )
          )
        )
      )
    )

  # attach dependencies
  htmltools::attachDependencies(x,
    htmltools::htmlDependency(
      name = "sidebar",
      version = "1.0.0",
      src = system.file(
        "htmlwidgets", package = "locationmisc"),
      stylesheet = "sidebarPane.css"
    )
  )
}
