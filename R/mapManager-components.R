#' Scaffold for the icon component of the map manager widget
#'
#' Create a HTML scaffold for an icon component of
#' the [mapManager()] widget.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
mm_icon_component_scaffold <- function() {
  htmltools::tags$div(class = "icon")
}

#' Scaffold for the sub-icon component of the map manager widget
#'
#' Create a HTML scaffold for a sub-icon component.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
mm_subicon_component_scaffold <- function() {
  htmltools::tags$div(class = "sub-icon")
}

#' Scaffold for the legend component of the map manager widget
#'
#' Create a HTML scaffold for a legend component.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
mm_legend_component_scaffold <- function() {
  htmltools::tags$div(class = "legend")
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
    htmltools::tags$label(
      class = "view-container",
      htmltools::tags$input(
        class = "view-checkbox",
        type = "checkbox"),
      htmltools::tags$i(class = "far fa-plus-square checked"),
      htmltools::tags$i(class = "far fa-minus-square unchecked")
    ),
    htmltools::tags$label(
      class = "visible-container",
      htmltools::tags$input(
        class = "visible-checkbox",
        type = "checkbox"),
      htmltools::tags$i(class = "fa fa-eye checked"),
      htmltools::tags$i(class = "fa fa-eye-slash unchecked")
    ),
    htmltools::tags$label(
      class = "name-label"
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
      class = "view-container",
      htmltools::tags$input(
        class = "view-checkbox",
        type = "checkbox"),
      htmltools::tags$i(class = "far fa-plus-square checked"),
      htmltools::tags$i(class = "far fa-minus-square unchecked")
    ),
    htmltools::tags$label(
      class = "visible-container",
      htmltools::tags$input(
        class = "visible-checkbox",
        type = "checkbox"),
      htmltools::tags$i(class = "fa fa-eye checked"),
      htmltools::tags$i(class = "fa fa-eye-slash unchecked")
    ),
    htmltools::tags$label(
      class = "name-label"
    )
  )
}
