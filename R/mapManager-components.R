#' Scaffold for the icon component of the map manager widget
#'
#' Create a HTML scaffold for an icon component of
#' the [mapManager()] widget.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
mm_icon_component_scaffold <- function() {
  htmltools::tags$div(class = "icon disable-if-inactive")
}

#' Scaffold for the sub-icon component of the map manager widget
#'
#' Create a HTML scaffold for a sub-icon component.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
mm_subicon_component_scaffold <- function() {
  htmltools::tags$div(class = "sub-icon disable-if-inactive")
}

#' Scaffold for the legend component of the map manager widget
#'
#' Create a HTML scaffold for a legend component.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
mm_legend_component_scaffold <- function() {
  htmltools::tags$div(class = "legend disable-if-inactive")
}

#' Scaffold for the header component of the map manager widget
#'
#' Create a HTML scaffold for a header component of
#' the [mapManager()] widget.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
mm_header_component_scaffold <- function(id = uuid::UUIDgenerate()) {
  htmltools::tags$div(
    class = "header",
    htmltools::tags$input(
      class = "view-checkbox",
      type = "checkbox"),
    htmltools::tags$input(
      class = "visible-checkbox",
      type = "checkbox"),
    htmltools::tags$label(
      class = "name-label disable-if-inactive"
    )
  )
}

#' Scaffold for the subheader component of the map manager widget
#'
#' Create a HTML scaffold for a subheader component of
#' the [mapManager()] widget.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
mm_subheader_component_scaffold <- function(id = uuid::UUIDgenerate()) {
  htmltools::tags$div(
    class = "sub-header",
    htmltools::tags$label(
      class = "el-switch el-switch-sm",
      htmltools::tags$input(
        type = "checkbox",
        class = "status-checkbox status",
        id = id
      ),
      htmltools::tags$span(
        class = "el-switch-style",
        `for` = id
      )
    ),
    htmltools::tags$label(
      class = "name-label disable-if-inactive"
    )
  )
}
