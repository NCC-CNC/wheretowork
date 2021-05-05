#' Scaffold for a goal component of a widget
#'
#' Create a HTML scaffold for a goal component.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
goal_component_scaffold <- function() {
  htmltools::tags$div(
    class = "goal",
    htmltools::tags$div(
      class = "info",
      htmltools::tags$div(
        class = "sub-info",
        htmltools::tags$div(
          class = "current-symbol"
        ),
        htmltools::tags$label(
          class = "current-label")
      ),
      htmltools::tags$div(
        class = "sub-info",
        htmltools::tags$div(
          class = "slider-symbol disable-if-inactive",
        ),
        htmltools::tags$label(
          class = "slider-label disable-if-inactive")
      ),
    ),
    slider_component_scaffold(bar = "current-bar")
  )
}

#' Scaffold for a group goal component of a widget
#'
#' Create a HTML scaffold for a goal component.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
group_goal_component_scaffold <- function() {
  htmltools::tags$div(
    class = "goal",
    htmltools::tags$div(
      class = "info",
      htmltools::tags$div(
        class = "sub-info",
        htmltools::tags$div(
          class = "current-symbol"
        ),
        htmltools::tags$label(
          class = "current-label")
      ),
      htmltools::tags$div(
        class = "sub-info",
        htmltools::tags$div(
          class = "slider-symbol disable-if-inactive",
        ),
        htmltools::tags$label(
          class = "slider-label disable-if-inactive")
      ),
    ),
    slider_component_scaffold(bar = c(
      "current-max-bar", "current-min-bar"))
  )
}

#' Scaffold for the slider component of a widget
#'
#' Create a HTML scaffold for a slider component.
#'
#' @param bar `character` names of classes for bars to include.
#'  Defaults to `NULL` such that no bars are included.
#'
#' @param current_min `logical` should the component provide a container for
#'  displaying a minimum bar behind the slider?
#'  Defaults to `FALSE`.
#'
#' @param current_max `logical` should the component provide a container for
#'  displaying a minimum bar behind the slider?
#'  Defaults to `FALSE`.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
slider_component_scaffold <- function(bar = NULL) {
  # assert arguments are valid
  if (!is.null(bar)) {
    assertthat::assert_that(
      is.character(bar),
      assertthat::noNA(bar))
  }
  # initialize slider
  out <- htmltools::tags$div(class = "slider")
  # add bars if needed
  if (!is.null(bar)) {
    # add bar cap
    out <-
      htmltools::tagAppendChild(out,
        htmltools::tags$div(class = "bar-cap"))
    # add bars
    bars <- do.call(
        htmltools::tagAppendChildren,
        append(
          list(htmltools::tags$div(class = "bars has-current-bar")),
          lapply(bar, function(x) htmltools::tags$div(class = x))))
    out <-
      htmltools::tagAppendChild(out, bars)

  }
  # add container for slider widget if needed
  out <- htmltools::tagAppendChild(out,
    htmltools::tags$div(
      class =
        paste("widget",
          ifelse(!is.null(bar), "has-current-bar", "no-current-bar")),
      htmltools::tags$div(class = "noUiSlider-widget disable-if-inactive")
    )
  )
  # return result
  out
}


#' Scaffold for the icon component of a widget
#'
#' Create a HTML scaffold for an icon component.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
icon_component_scaffold <- function() {
  htmltools::tags$div(class = "icon disable-if-inactive")
}

#' Scaffold for the sub-icon component of a widget
#'
#' Create a HTML scaffold for an sub-icon component.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
subicon_component_scaffold <- function() {
  htmltools::tags$div(class = "sub-icon disable-if-inactive")
}

#' Scaffold for the header component of a widget
#'
#' Create a HTML scaffold for a header component.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
header_component_scaffold <- function(id = uuid::UUIDgenerate()) {
  htmltools::tags$div(
    class = "header",
    htmltools::tags$label(
      class = "el-switch",
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

#' Scaffold for the subheader component of a widget
#'
#' Create a HTML scaffold for a subheader component.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
subheader_component_scaffold <- function(id = uuid::UUIDgenerate()) {
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
