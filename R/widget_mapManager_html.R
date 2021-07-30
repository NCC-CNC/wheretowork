#' @include internal.R
NULL

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
#' @param id `character` HTML element identifier value.
#'   Defaults to random identifier.
#'
#' @param remove_button `logical` include a button to remove the layer?
#'  Defaults to `FALSE`.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
mm_header_component_scaffold <- function(id = uuid::UUIDgenerate(),
                                         type = "layer",
                                         remove_button = FALSE) {
  # create tooltip text for type
  if (identical(type, "theme")) {
    icon <- shiny::icon("star")
    remove_tooltip_text <- "Delete Theme"
    visible_tooltip_text <- "Show/hide Theme on map"
    icon_tooltip_text <- "This is a Theme"
  } else if (identical(type, "weight")) {
    icon <- shiny::icon("weight-hanging")
    remove_tooltip_text <- "Delete Weight"
    visible_tooltip_text <- "Show/hide Weight on map"
    icon_tooltip_text <- "This is a Weight"
  } else if (identical(type, "include")) {
    icon <- shiny::icon("lock")
    remove_tooltip_text <- "Delete Include"
    visible_tooltip_text <- "Show/hide Include on map"
    icon_tooltip_text <- "This is an Include"
  } else if (identical(type, "solution")) {
    icon <- shiny::icon("rocket")
    remove_tooltip_text <- "Delete solution"
    visible_tooltip_text <- "Show/hide solution on map"
    icon_tooltip_text <- "This is a solution"
  } else {
    icon <- shiny::icon("map-marker-alt")
    remove_tooltip_text <- "Delete layer"
    visible_tooltip_text <- "Show/hide layer on map"
    icon_tooltip_text <- "This layer is a miscellaneous layer"
  }

  # return scaffold
  htmltools::tags$div(
    class = "header",
    htmltools::tags$label(
      class = "view-container",
      `data-toggle` = "tooltip",
      `data-placement` = "top",
      `data-container` = "body",
      title = "Show/hide legend",
      htmltools::tags$input(
        class = "view-checkbox",
        type = "checkbox"
      ),
      htmltools::tags$i(class = "far fa-minus-square checked"),
      htmltools::tags$i(class = "far fa-plus-square unchecked")
    ),
    htmltools::tags$label(
      class = "visible-container",
      `data-toggle` = "tooltip",
      `data-placement` = "top",
      `data-container` = "body",
      title = visible_tooltip_text,
      htmltools::tags$input(
        class = "visible-checkbox",
        type = "checkbox"
      ),
      htmltools::tags$i(class = "fa fa-eye checked"),
      htmltools::tags$i(class = "fa fa-eye-slash unchecked")
    ),
    htmltools::tags$label(
      class = "name-label"
    ),
    htmltools::tags$div(
      class = "icon-container",
      `data-toggle` = "tooltip",
      `data-placement` = "top",
      `data-container` = "body",
      title = icon_tooltip_text,
      icon
    ),
    if (remove_button) {
      htmltools::tags$button(
        class = "remove-button",
        type = "button",
        `data-toggle` = "tooltip",
        `data-placement` = "top",
        `data-container` = "body",
        title = remove_tooltip_text,
        htmltools::tags$i(class = "fa fa-trash-alt")
      )
    } else {
      NULL
    }
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
  # create ids
  name_label_id <- uuid::UUIDgenerate()
  visible_checkbox_id <- uuid::UUIDgenerate()
  view_checkbox_id <- uuid::UUIDgenerate()

  htmltools::tags$div(
    class = "sub-header",
    htmltools::tags$label(
      class = "view-container",
      `data-toggle` = "tooltip",
      `data-placement` = "top",
      `data-container` = "body",
      title = "Show/hide legend",
      htmltools::tags$input(
        class = "view-checkbox",
        type = "checkbox"
      ),
      htmltools::tags$i(class = "far fa-minus-square checked"),
      htmltools::tags$i(class = "far fa-plus-square unchecked")
    ),
    htmltools::tags$label(
      class = "visible-container",
      `data-toggle` = "tooltip",
      `data-placement` = "top",
      `data-container` = "body",
      title = "Show/hide feature on map",
      htmltools::tags$input(
        class = "visible-checkbox",
        type = "checkbox"
      ),
      htmltools::tags$i(class = "fa fa-eye checked"),
      htmltools::tags$i(class = "fa fa-eye-slash unchecked")
    ),
    htmltools::tags$label(
      class = "name-label",
    )
  )
}
