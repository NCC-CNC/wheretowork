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
#' @param id `character` HTML element identifier value.
#'   Defaults to random identifier.
#'
#' @param remove_button `logical` include a button to remove the layer?
#'  Defaults to `FALSE`.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
mm_header_component_scaffold <- function(
  id = uuid::UUIDgenerate(), remove_button = FALSE) {
  # return scaffold
  htmltools::tags$div(
    class = "header",

    htmltools::tags$label(
      class = "view-container",
      `data-toggle` = "tooltip",
      `data-placement` = "top",
      `data-delay` = "{\"show\":500, \"hide\":100}",
      title = "Show/hide legend",
      htmltools::tags$input(
        class = "view-checkbox",
        type = "checkbox"),
      htmltools::tags$i(class = "far fa-minus-square checked"),
      htmltools::tags$i(class = "far fa-plus-square unchecked")
    ),

    htmltools::tags$label(
      class = "visible-container",
      `data-toggle` = "tooltip",
      `data-placement` = "top",
      `data-delay` = "{\"show\":500, \"hide\":100}",
      title = "Show/hide layer",
      htmltools::tags$input(
        class = "visible-checkbox",
        type = "checkbox"),
      htmltools::tags$i(class = "fa fa-eye checked"),
      htmltools::tags$i(class = "fa fa-eye-slash unchecked")
    ),
    htmltools::tags$label(
       class = "name-label",
      `data-toggle` = "tooltip",
      `data-placement` = "top",
      `data-delay` = "{\"show\":500, \"hide\":100}",
      title = "Name of the layer",
    ),
    if (remove_button) {
      htmltools::tags$button(
        class = "remove-button",
        type = "button",
        `data-toggle` = "tooltip",
        `data-placement` = "top",
        `data-delay` = "{\"show\":500, \"hide\":100}",
        title = "Remove layer",
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
      `data-delay` = "{\"show\":500, \"hide\":100}",
      title = "Show/hide legend",
      htmltools::tags$input(
        class = "view-checkbox",
        type = "checkbox"),
      htmltools::tags$i(class = "far fa-minus-square checked"),
      htmltools::tags$i(class = "far fa-plus-square unchecked")
    ),

    htmltools::tags$label(
      class = "visible-container",
      `data-toggle` = "tooltip",
      `data-placement` = "top",
      `data-delay` = "{\"show\":500, \"hide\":100}",
      title = "Show/hide feature",
      htmltools::tags$input(
        class = "visible-checkbox",
        type = "checkbox"),
      htmltools::tags$i(class = "fa fa-eye checked"),
      htmltools::tags$i(class = "fa fa-eye-slash unchecked")
    ),

    htmltools::tags$label(
      class = "name-label",
      `data-toggle` = "tooltip",
      `data-placement` = "top",
      `data-delay` = "{\"show\":500, \"hide\":100}",
      title = "Name of the feature",
    )
  )
}
