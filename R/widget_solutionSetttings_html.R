#' @include internal.R
NULL

#' Scaffold for a goal component of the solution settings widget
#'
#' Create a HTML scaffold for a goal component of
#' the [solutionSettings()] widget.
#'
#' @inheritParams ss_slider_component_scaffold
#'
#' @return `shiny.tag` object.
#'
#' @noRd
ss_goal_component_scaffold <- function() {
  htmltools::tags$div(
    class = "goal",
    htmltools::tags$div(
      class = "status-info",
      htmltools::tags$div(
        class = "sub-status-info",
        htmltools::tags$div(
          class = "current-symbol"
        ),
        htmltools::tags$label(
          class = "current-label",
          `data-toggle` = "tooltip",
          `data-placement` = "top",
          `data-container` = "body",
          `data-trigger` = "hover",
          title = "Current coverage by Includes"
        )
      ),
      htmltools::tags$div(
        class = "sub-status-info",
        htmltools::tags$div(
          class = "slider-symbol disable-if-inactive",
        ),
        htmltools::tags$label(
          class = "slider-label disable-if-inactive",
          `data-toggle` = "tooltip",
          `data-placement` = "top",
          `data-container` = "body",
          `data-trigger` = "hover",
          title = "Goal for generating solutions"
        )
      )
    ),
    htmltools::tags$div(
      `data-toggle` = "tooltip",
      `data-placement` = "bottom",
      `data-container` = "body",
      `data-trigger` = "hover",
      title = "Set the goal",
      ss_slider_component_scaffold(bar = "current-bar")
    )
  )
}

#' Scaffold for a group goal component of the solution settings widget
#'
#' Create a HTML scaffold for a goal component of
#' the [solutionSettings()] widget.
#'
#' @inheritParams ss_slider_component_scaffold
#'
#' @return `shiny.tag` object.
#'
#' @noRd
ss_group_goal_component_scaffold <- function() {
  htmltools::tags$div(
    class = "goal",
    htmltools::tags$div(
      class = "status-info",
      htmltools::tags$div(
        class = "sub-status-info",
        htmltools::tags$div(
          class = "current-symbol"
        ),
        htmltools::tags$label(
          class = "current-label",
          `data-toggle` = "tooltip",
          `data-placement` = "top",
          `data-container` = "body",
          `data-trigger` = "hover",
          title = "Current coverage by Includes"
        )
      ),
      htmltools::tags$div(
        class = "sub-status-info",
        htmltools::tags$div(
          class = "slider-symbol disable-if-inactive",
        ),
        htmltools::tags$label(
          class = "slider-label disable-if-inactive",
          `data-toggle` = "tooltip",
          `data-placement` = "top",
          `data-container` = "body",
          `data-trigger` = "hover",
          title = "Goal for generating solutions"
        )
      ),
    ),
    htmltools::tags$div(
      `data-toggle` = "tooltip",
      `data-placement` = "bottom",
      `data-container` = "body",
      `data-trigger` = "hover",
      title = "Set goals for all features in the Theme",
      ss_slider_component_scaffold(
        bar = c("current-max-bar", "current-min-bar")
      )
    )
  )
}

#' Scaffold for the slider component of the solution settings widget
#'
#' Create a HTML scaffold for a slider component of the
#' the [solutionSettings()] widget.
#'
#' @param bar `character` names of classes for bars to include.
#'  Defaults to `NULL` such that no bars are included.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
ss_slider_component_scaffold <- function(bar = NULL) {
  # assert arguments are valid
  if (!is.null(bar)) {
    assertthat::assert_that(
      is.character(bar),
      assertthat::noNA(bar)
    )
  }

  # initialize slider
  out <- htmltools::tags$div(class = "slider")

  # add bars if needed
  if (!is.null(bar)) {
    # add bar cap
    out <-
      htmltools::tagAppendChild(
        out,
        htmltools::tags$div(class = "bar-cap")
      )
    # add bars
    bars <- do.call(
      htmltools::tagAppendChildren,
      append(
        list(htmltools::tags$div(class = "bars has-current-bar")),
        lapply(bar, function(x) htmltools::tags$div(class = x))
      )
    )
    out <-
      htmltools::tagAppendChild(out, bars)
  }
  # add container for slider widget if needed
  out <- htmltools::tagAppendChild(
    out,
    htmltools::tags$div(
      class =
        paste(
          "widget",
          ifelse(!is.null(bar), "has-current-bar", "no-current-bar")
        ),
      htmltools::tags$div(class = "noUiSlider-widget disable-if-inactive")
    )
  )
  # return result
  out
}

#' Scaffold for the header component of the solution settings widget
#'
#' Create a HTML scaffold for a header component of
#' the [solutionSettings()] widget.
#'
#' @inheritParams ss_slider_component_scaffold
#'
#' @param reset_button `logical` indicating if a reset button should be
#'   included. Defaults to `FALSE`.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
ss_header_component_scaffold <- function(
  type, reset_button = FALSE, id = uuid::UUIDgenerate()) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(type),
    assertthat::noNA(type),
    assertthat::is.flag(reset_button)
  )

  if (reset_button) {
    if (type == "theme") {
      reset_button_text <- "Reset to default goal"
    } else {
      reset_button_text <- "Reset to default"
    }
  }

  # HTML scaffold
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
        `data-toggle` = "tooltip",
        `data-placement` = "top",
        `data-container` = "body",
        `data-trigger` = "hover",
        title = paste(
          "Enable/disable the ",
          tools::toTitleCase(type),
          " when generating a solution"
        ),
        `for` = id
      )
    ),
    if (reset_button) {
      htmltools::tags$button(
        class = "reset-button disable-if-inactive",
        type = "button",
        `data-toggle` = "tooltip",
        `data-placement` = "top",
        `data-container` = "body",
        `data-trigger` = "hover",
        title = reset_button_text,
        htmltools::tags$i(class = "fa fa-redo")
      )
    },
    htmltools::tags$label(
      class = "name-label disable-if-inactive"
    ),
    htmltools::tags$div(
      class = "provenance-container"
    )
  )
}

ss_subheader_component_scaffold <- function(id = uuid::UUIDgenerate()) {
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
        `data-toggle` = "tooltip",
        `data-placement` = "top",
        `data-container` = "body",
        `data-trigger` = "hover",
        title = "Enable/disable the feature when generating a solution",
        `for` = id
      )
    ),
    htmltools::tags$label(
      class = "name-label disable-if-inactive"
    ),
    htmltools::tags$div(
      class = "provenance-container"
    )
  )
}
