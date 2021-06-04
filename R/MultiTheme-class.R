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

    #' @field current_label `character` value.
    current_label = NA_character_,

    #' @field feature_order `numeric` value.
    feature_order = NA_real_,

    #' @description
    #' Create a MultiTheme object.
    #' @param id `character` value.
    #' @param name `character` value.
    #' @param feature `list` of [Feature] objects.
    #' @param mandatory `logical` value.
    #' @param current_label `character` value.
    #' @param feature_order `numeric` vector.
    #' @param round `logical` value.
    #' @param icon `shiny.tag` object.
    #' @return A new MultiTheme object.
    initialize = function(
      id, name, feature, mandatory,
      current_label, feature_order, round, icon) {
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
        #### mandatory
        assertthat::is.flag(mandatory),
        assertthat::noNA(mandatory),
        #### current_label
        assertthat::is.string(current_label),
        assertthat::noNA(current_label),
        #### feature_order
        is.numeric(feature_order),
        assertthat::noNA(feature_order),
        length(feature_order) == length(feature),
        identical(anyDuplicated(feature_order), 0L),
        #### round
        assertthat::is.flag(round),
        assertthat::noNA(round),
        #### icon
        inherits(icon, "shiny.tag"))
      ## assert all feature have ame units
      assertthat::assert_that(
        n_distinct(
          vapply(
            feature, FUN.VALUE = character(1),
            function(x) x$variable$units)) == 1,
        msg = "argument to `feature` contains elements with different units")
      ## set fields
      self$id <- id
      self$name <- name
      self$feature <- feature
      self$mandatory <- mandatory
      self$current_label <- current_label
      self$feature_order <- feature_order
      self$round <- round
      self$icon <- icon
    },

    #' @description
    #' Set relative order for displaying features on a map.
    get_feature_order = function() {
      self$feature_order
    },

    #' @description
    #' Set relative order for displaying features on a map.
    #' @param value `numeric` vector of new orders.
    set_feature_order = function(value) {
      if (is.list(value))
        value <- unlist(value, recursive = TRUE, use.names = TRUE)
      assertthat::assert_that(
        is.numeric(value),
        assertthat::noNA(value),
        length(value) == length(self$feature),
        identical(anyDuplicated(value), 0L))
      self$feature_order <- value
      invisible(self)
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
        feature_status =
          vapply(self$feature, function(x) x$status, logical(1)),
        feature_total_amount =
          vapply(self$feature, function(x) x$variable$total, numeric(1)),
        feature_current_held =
          vapply(self$feature, function(x) x$current, numeric(1)),
        feature_min_goal =
          vapply(self$feature, function(x) x$min_goal, numeric(1)),
        feature_max_goal =
          vapply(self$feature, function(x) x$max_goal, numeric(1)),
        feature_goal =
          vapply(self$feature, function(x) x$goal, numeric(1)),
        feature_limit_goal =
          vapply(self$feature, function(x) x$limit_goal, numeric(1)),
        feature_step_goal =
          vapply(self$feature, function(x) x$step_goal, numeric(1)),
        feature_current_label =
          vapply(self$feature, function(x) x$current_label, character(1)),
        feature_icon =
          vapply(
            self$feature, function(x) as.character(x$icon),
            character(1)),
        units = self$feature[[1]]$variable$units,
        mandatory = self$mandatory,
        current_label = self$current_label,
        round = self$round,
        icon = as.character(self$icon)
      )
    },

    #' @description
    #' Get data for displaying the theme in a [mapManager()] widget.
    #' @return `list` with widget data.
    get_map_manager_widget_data = function() {
      list(
        id = self$id,
        name = self$name,
        feature_name =
          vapply(self$feature, function(x) x$name, character(1)),
        feature_id =
          vapply(self$feature, function(x) x$id, character(1)),
        feature_visible =
          vapply(self$feature, function(x) x$visible, logical(1)),
        feature_legend =
          lapply(self$feature, function(x) x$variable$legend$get_widget_data()),
        units = self$feature[[1]]$variable$units,
        type = "theme"
      )
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
#' @param mandatory `logical` Is the theme mandatory for generating solutions?
#'   Defaults to `FALSE`.
#'
#' @param current_label `character` The display label for the current
#'  level of representation for the features under the group view.
#'  Defaults to `Current`.
#'
#' @param feature_order `numeric` Relative order for displaying each feature
#'  on a map. Defaults to a reverse sequence of integer values.
#'
#' @param round `logical` should all numbers be rounded to the nearest integer?
#'   Defaults to `TRUE`.
#'
#' @param icon `shiny.tag` Icon to display for the feature
#'  This icon should indicate the type of data that underpin the feature.
#'  Alternatively, the argument can be a `character` to automatically
#'  generate a `shiny.tag` icon (using [shiny::icon()]).
#'  Defaults to `"map-marked-alt"`.
#'
#' @param id `character` unique identifier.
#'   Defaults to a random identifier ([uuid::UUIDgenerate()]).
#'
#' @return A [MultiTheme] object.
#'
#' @examples
#' # create datasets
#' l1 <- new_dataset(
#'  source = tempfile(), total = 12, units = "ha",
#'  legend = new_continuous_legend(1, 100, c("#000000", "#1b9e77")))
#' l2 <- new_dataset(
#'  source = tempfile(), total = 14, units = "ha",
#'  legend = new_continuous_legend(1, 100, c("#000000", "#d95f02")))
#' l3 <- new_dataset(
#'  source = tempfile(), total = 78, units = "ha",
#'  legend = new_continuous_legend(1, 100, c("#000000", "#7570b3")))
#'
#' # create features the datasets
#' f1 <- new_feature(name = "Pangolin", dataset = l1)
#' f2 <- new_feature(name = "Panda", dataset = l2)
#' f3 <- new_feature(name = "Palms", dataset = l3)
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
  mandatory = FALSE,
  round = TRUE,
  icon = "map-marked-alt",
  current_label = "Current",
  feature_order = as.double(rev(seq_along(feature))),
  id = uuid::UUIDgenerate()) {
  # convert icon to shiny.tag if needed
  if (is.character(icon))
    icon <- shiny::icon(icon)
  # return new feature
  MultiTheme$new(
    id = id,
    name = name,
    feature = feature,
    mandatory = mandatory,
    current_label = current_label,
    feature_order = feature_order,
    round = round,
    icon = icon)
}
