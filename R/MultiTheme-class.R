#' @include internal.R Feature-class.R Theme-class.R
NULL

#' MultiTheme class
#'
#' Definition for the MultiTheme class.
#'
#' @seealso [new_multi_theme()].
MultiTheme <- R6::R6Class(
  "MultiTheme",
  inherit = Theme,
  public = list(

    #' @field group_min_goal `numeric` value.
    group_min_goal = NA_real_,

    #' @field group_max_goal `numeric` value.
    group_max_goal = NA_real_,

    #' @field group_initial_goal `numeric` value.
    group_initial_goal = NA_real_,

    #' @field group_limit_goal `numeric` value.
    group_limit_goal = NA_real_,

    #' @field group_step_goal `numeric` value.
    group_step_goal = NA_real_,

    #' @field group_current_label `character` value.
    group_current_label = NA_character_,

    #' @description
    #' Create a MultiTheme object.
    #' @param id `character` value.
    #' @param name `character` value.
    #' @param feature `list` of [Feature] objects.
    #' @param group_min_goal `numeric` value.
    #' @param group_max_goal `numeric` value.
    #' @param group_initial_goal `numeric` value.
    #' @param group_limit_goal `numeric` value.
    #' @param group_step_goal `numeric` value.
    #' @param group_current_label `character` value.
    #' @param initial_status `logical` value.
    #' @param round `logical` value.
    #' @param icon `shiny.tag` object.
    #' @return A new MultiTheme object.
    initialize = function(
      id, name, feature,
      group_min_goal, group_max_goal, group_initial_goal, group_limit_goal,
      group_step_goal, group_current_label,
      initial_status, round, icon) {
      ### assert that arguments are valid
      assertthat::assert_that(
        #### id
        assertthat::is.string(id),
        assertthat::noNA(id),
        #### name
        assertthat::is.string(name),
        assertthat::noNA(name),
        #### feature
        is.list(feature),
        all_list_elements_inherit(feature, "Feature"),
        #### group_min_goal
        assertthat::is.number(group_min_goal),
        assertthat::noNA(group_min_goal),
        #### group_max_goal
        assertthat::is.number(group_max_goal),
        assertthat::noNA(group_max_goal),
        #### group_initial_goal
        assertthat::is.number(group_initial_goal),
        assertthat::noNA(group_initial_goal),
        #### group_limit_goal
        assertthat::is.number(group_limit_goal),
        assertthat::noNA(group_limit_goal),
        #### group_step_goal
        assertthat::is.number(group_step_goal),
        assertthat::noNA(group_step_goal),
        #### group_current_label
        assertthat::is.string(group_current_label),
        assertthat::noNA(group_current_label),
        #### initial_status
        assertthat::is.flag(initial_status),
        assertthat::noNA(initial_status),
        #### round
        assertthat::is.flag(round),
        assertthat::noNA(round),
        #### icon
        inherits(icon, "shiny.tag"))
      ## assert all feature have ame units
      assertthat::assert_that(
        dplyr::n_distinct(
          vapply(
            feature, FUN.VALUE = character(1), function(x) x$layer$units)) == 1,
        msg = "argument to `feature` contains elements with different units")
      ## set fields
      self$id <- id
      self$name <- name
      self$feature <- feature
      self$group_min_goal <- group_min_goal
      self$group_max_goal <- group_max_goal
      self$group_initial_goal <- group_initial_goal
      self$group_limit_goal <- group_limit_goal
      self$group_step_goal <- group_step_goal
      self$group_current_label <- group_current_label
      self$initial_status <- initial_status
      self$round <- round
      self$icon <- icon
    },

    #' @description
    #' Get data for displaying the theme in a [solutionSettings()] widget.
    #' @return `list` with widget data.
    get_solution_settings_widget_data = function() {
      list(
        id = self$id,
        name = self$name,
        feature_name =
          vapply(self$feature, function(x) x$name, character(1)),
        feature_id =
          vapply(self$feature, function(x) x$id, character(1)),
        feature_total_amount =
          vapply(self$feature, function(x) x$layer$total, numeric(1)),
        feature_current_held =
          vapply(self$feature, function(x) x$layer$current, numeric(1)),
        group_min_goal = self$group_min_goal,
        group_max_goal = self$group_max_goal,
        group_initial_goal = self$group_initial_goal,
        group_limit_goal = self$group_limit_goal,
        group_step_goal = self$group_step_goal,
        group_current_label = self$group_current_label,
        feature_min_goal =
          vapply(self$feature, function(x) x$min_goal, numeric(1)),
        feature_max_goal =
          vapply(self$feature, function(x) x$max_goal, numeric(1)),
        feature_initial_goal =
          vapply(self$feature, function(x) x$goal, numeric(1)),
        feature_limit_goal =
          vapply(self$feature, function(x) x$limit_goal, numeric(1)),
        feature_step_goal =
          vapply(self$feature, function(x) x$step_goal, numeric(1)),
        feature_current_label =
          vapply(self$feature, function(x) x$current_label, character(1)),
        feature_initial_status =
          vapply(self$feature, function(x) x$status, logical(1)),
        feature_icon =
          vapply(
            self$feature, function(x) as.character(x$icon),
            character(1)),
        units = self$feature[[1]]$layer$units,
        initial_status = self$initial_status,
        round = self$round,
        icon = self$icon
      )
    },

    #' @description
    #' Get data for displaying the theme in a [mapManager()] widget.
    #' @return `list` with widget data.
    get_map_manager_widget_data = function() {
      stop("TODO")
    }
  )
)

#' New multi-theme
#'
#' Create a new [MultiTheme] object.
#'
#' @param name `character` Name to display.
#'
#' @param feature `list` of [Feature] objects.
#'
#' @param group_min_goal `numeric` The minimum goal (inclusive)
#'   shown for the features under the group view.
#'   Note that goal values are specified as proportions, such that a
#'   value of 0.1 corresponds to 10%.
#'   Defaults to 0 (i.e. 0%).
#'
#' @param group_max_goal `numeric` The maximum goal (inclusive)
#'   shown for the features under the group view.
#'   Note that goal values are specified as proportions, such that a
#'   value of 0.1 corresponds to 10%.
#'   Defaults to 1 (i.e. 100%).
#'
#' @param group_initial_goal `numeric` The initial goal
#'   for the features under the group view.
#'   Note that goal values are specified as proportions, such that a
#'   value of 0.1 corresponds to 10%.
#'   Defaults to 0.3 (i.e. 30%).
#'
#' @param group_limit_goal `numeric`  The minimum increment for setting goals
#'   for the features under the group view.
#'   Note that goal are specified as proportions, such that a
#'   value of 0.01 corresponds to 1%.
#'   Defaults to 0.01 (i.e. 1%).
#'
#' @param group_step_goal `numeric` The minimum increment for setting goals
#'   for the features under the group view.
#'   Note that goal are specified as proportions, such that a
#'   value of 0.01 corresponds to 1%.
#'   Defaults to 0.01 (i.e. 1%).
#'
#' @param group_current_label `character` The display label for the current
#'  level of representation for the features under the group view.
#'  Defaults to `Current`.
#'
#' @param initial_status `logical` The initial status.
#'   This is used to display information on whether the theme is
#'   selected (or not) for subsequent analysis.
#'   Defaults to `TRUE`
#'
#' @param round `logical` should all numbers be rounded to the nearest integer?
#'   Defaults to `TRUE`.
#'
#' @param icon `shiny.tag` Icon to display for the feature
#'  This icon should indicate the type of data that underpin the feature.
#'  Alternatively, the argument can be a `character` to automactially
#'  generate a `shiny.tag` icon (using [shiny::icon()]).
#'  Defaults to `"map-marked-alt"`.
#'
#' @param id `character` unique identifier.
#'   Defaults to a random identifier ([uuid::UUIDgenerate()]).
#'
#' @return A [MultiTheme] object.
#'
#' @examples
#' # create layers
#' l1 <- new_layer(source = tempfile(), current = 0.1, total = 12, units = "ha")
#' l2 <- new_layer(source = tempfile(), current = 0.2, total = 15, units = "ha")
#' l3 <- new_layer(source = tempfile(), current = 0.4, total = 20, units = "ha")
#'
#' # create features the layers
#' f1 <- new_feature(name = "Pangolin", layer = l1)
#' f2 <- new_feature(name = "Panda", layer = l2)
#' f3 <- new_feature(name = "Palms", layer = l3)
#'
#' # create a multi-theme using the features
#' mt <- new_multi_theme(
#'   name = "Endangered species", feature = list(f1, f2, f3))
#'
#' # print object
#' print(mt)
#'
#' @export
new_multi_theme <- function(
  name,
  feature,
  initial_status = TRUE,
  round = TRUE,
  icon = "map-marked-alt",
  id = uuid::UUIDgenerate(),
  group_min_goal = 0,
  group_max_goal = 1,
  group_initial_goal = 0.3,
  group_limit_goal = 0.1,
  group_step_goal = 0.01,
  group_current_label = "Current") {
  # convert icon to shiny.tag if needed
  if (is.character(icon))
    icon <- shiny::icon(icon)
  # return new feature
  MultiTheme$new(
    id = id,
    name = name,
    feature = feature,
    group_min_goal = group_min_goal,
    group_max_goal = group_max_goal,
    group_initial_goal = group_initial_goal,
    group_limit_goal = group_limit_goal,
    group_step_goal = group_step_goal,
    group_current_label = group_current_label,
    initial_status = initial_status,
    round = round,
    icon = icon)
}
