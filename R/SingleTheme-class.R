#' @include internal.R Feature-class.R Theme-class.R
NULL

#' SingleTheme class
#'
#' Definition for the SingleTheme class.
#'
#' @seealso [new_single_theme()].
SingleTheme <- R6::R6Class(
  "SingleTheme",
  inherit = Theme,
  public = list(

    #' @description
    #' Create a SingleTheme object.
    #' @param id `character` value.
    #' @param name `character` value.
    #' @param feature `list` of a single [Feature] object.
    #' @param initial_status `logical` value.
    #' @param icon `shiny.tag` object.
    #' @param round `logical` value.
    #' @return A new SingleTheme object.
    initialize = function(
      id, name, feature, initial_status, round, icon) {
      ### assert that arguments are valid
      assertthat::assert_that(
        #### id
        assertthat::is.string(id),
        assertthat::is.noNA(id),
        #### name
        assertthat::is.string(name),
        assertthat::is.noNA(name),
        #### feature
        is.list(feature),
        length(feature) == 1,
        inherits(feature[[1]], "Feature"),
        #### initial_status
        assertthat::is.flag(initial_status),
        assertthat::is.noNA(initial_status),
        #### round
        assertthat::is.flag(round),
        assertthat::is.noNA(round),
        #### icon
        assertthat::is.string(icon),
        assertthat::is.noNA(icon))
      ## set fields
      self$id = id
      self$name = name
      self$feature = feature
      self$status = initial_status
      self$initial_status = initial_status
      self$round = round
      self$icon = icon
    },

    #' @description
    #' Get data for displaying the theme in a [newSolutionManager] widget.
    #' @return `list` with data for
    #'   displaying the object in a [newSolutionManager()] widget.
    get_new_solution_manager_data = function() {
      list(
        id = self$id,
        name = self$name,
        feature_name = self$feature[[1]]$name,
        feature_id = self$feature[[1]]$id,
        feature_total_amount = self$feature[[1]]$total,
        feature_current_held = self$feature[[1]]$current,
        feature_min_goal = self$feature[[1]]$min_goal,
        feature_max_goal = self$feature[[1]]$max_goal,
        feature_initial_goal = self$feature[[1]]$goal,
        feature_limit_goal = self$feature[[1]]$limit_goal,
        feature_step_goal = self$feature[[1]]$step_goal,
        feature_current_label = self$feature[[1]]$current_label,,
        feature_units = self$feature[[1]]$units,
        initial_status = self$status,
        round = self$round,
        icon = self$icon,
      )
    },

    #' @description
    #' Get data for displaying the theme in a [mapManager] widget.
    #' @return `list` with data for displaying the object in a [mapManager()]
    #'   widget.
    get_map_manager_data = function() {
      stop("TODO")
    }
  )
)

#' New single theme
#'
#' Create a new [SingleTheme] object.
#'
#' @param feature [Feature] object.
#'   For compatibility with [new_multi_theme()], a `list` object
#'   can also be supplied that contains a single [Feature] object.
#'
#' @inheritParams new_multi_theme
#'
#' @return A [SingleTheme] object.
#'
#' @examples
#' # create new layer
#' l <- new_layer(source = tempfile(), current = 0.1, total = 12, units = "ha")
#'
#' # create feature using the layer
#' f <- new_feature(name = "Intact Alvar Occurrence", layer = l)
#'
#' # create a theme using the single feature
#' st <- new_single_theme(name = "Inact Alvar", feature = f)
#'
#' # print object
#' print(st)
#'
#' @export
new_single_theme <- function(
  name,
  feature,
  initial_status = TRUE,
  round = TRUE,
  icon = "map-marked-alt",
  id = uuid::UUIDgenerate()) {
  # convert icon to shiny.tag if needed
  if (is.character(icon))
    icon <- shiny::icon(icon)
  # return new feature
  SingleTheme$new(
    id = id,
    feature = feature,
    name = name,
    initial_status = initial_status,
    round = round,
    icon = icon)
}
