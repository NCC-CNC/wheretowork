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
ss_goal_component_scaffold <- function(type) {
  assertthat::assert_that(
    assertthat::is.string(type),
    assertthat::noNA(type),
    type %in% c("weight", "theme"))
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
          `data-delay` = "{\"show\":500, \"hide\":100}",
          `data-container` = "body",
          title = "Current coverage by existing conservation areas")
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
          `data-delay` = "{\"show\":500, \"hide\":100}",
          `data-container` = "body",
          title = "Goal for generating solutions")
      ),
    ),
    ss_slider_component_scaffold(bar = "current-bar", type)
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
ss_group_goal_component_scaffold <- function(type) {
  assertthat::assert_that(
    assertthat::is.string(type),
    assertthat::noNA(type),
    type %in% c("weight", "theme"))
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
          `data-delay` = "{\"show\":500, \"hide\":100}",
          `data-container` = "body",
          title = "Current coverage by existing conservation areas")
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
          `data-delay` = "{\"show\":500, \"hide\":100}",
          `data-container` = "body",
          title = "Goal for generating solutions")
      ),
    ),
    ss_slider_component_scaffold(bar = c(
      "current-max-bar", "current-min-bar"), type)
  )
}

#' Scaffold for the slider component of the solution settings widget
#'
#' Create a HTML scaffold for a slider component of the
#' the [solutionSettings()] widget.
#'
#' @param type `character` name of parent widget.
#'
#' @param bar `character` names of classes for bars to include.
#'  Defaults to `NULL` such that no bars are included.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
ss_slider_component_scaffold <- function(type, bar = NULL) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(type),
    assertthat::noNA(type),
    type %in% c("weight", "theme", "parameter"))
  if (!is.null(bar)) {
    assertthat::assert_that(
      is.character(bar),
      assertthat::noNA(bar))
  }

  # initialize slider
  out <- htmltools::tags$div(
    class = "slider",
    `data-toggle` = "tooltip",
    `data-placement` = "bottom",
    `data-delay` = "{\"show\":500, \"hide\":100}",
    `data-container` = "body",
    title =
      switch(
        type,
        "weight" = "Set the factor",
        "theme" = "Set the goal",
        "parameter" = "Set the parameter value"
      )
    )

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

#' Scaffold for the header component of the solution settings widget
#'
#' Create a HTML scaffold for a header component of
#' the [solutionSettings()] widget.
#'
#' @inheritParams ss_slider_component_scaffold
#'
#' @return `shiny.tag` object.
#'
#' @noRd
ss_header_component_scaffold <- function(type, id = uuid::UUIDgenerate()) {
  #  assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(type),
    assertthat::noNA(type))

  # HTML scaffold
  htmltools::tags$div(
    class = "header",
    htmltools::tags$label(
      class = "el-switch",
      `data-toggle` = "tooltip",
      `data-placement` = "top",
      `data-delay` = "{\"show\":500, \"hide\":100}",
      `data-container` = "body",
      title =
        paste0("Enable/disable the ", type, " when generating a solution"),
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
      class = "name-label disable-if-inactive",
      `data-toggle` = "tooltip",
      `data-placement` = "top",
      `data-delay` = "{\"show\":500, \"hide\":100}",
      `data-container` = "body",
      title = paste0("Name of the ", type)
    )
  )
}

ss_subheader_component_scaffold<- function(id = uuid::UUIDgenerate()) {
  htmltools::tags$div(
    class = "sub-header",
    htmltools::tags$label(
      class = "el-switch el-switch-sm",
      `data-toggle` = "tooltip",
      `data-placement` = "top",
      `data-delay` = "{\"show\":500, \"hide\":100}",
      `data-container` = "body",
      title = "Enable/disable the feature when generating a solution",
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
      class = "name-label disable-if-inactive",
      `data-toggle` = "tooltip",
      `data-placement` = "top",
      `data-delay` = "{\"show\":500, \"hide\":100}",
      `data-container` = "body",
      title = "Name of the feature",
    )
  )
}
