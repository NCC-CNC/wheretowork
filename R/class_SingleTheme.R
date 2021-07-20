#' @include internal.R class_Feature.R class_Theme.R
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
    #' @return A new SingleTheme object.
    initialize = function(id, name, feature) {
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
        length(feature) == 1,
        inherits(feature[[1]], "Feature")
      )
      ## set fields
      self$id <- id
      self$name <- name
      self$feature <- feature
    },

    #' @description
    #' Get relative order for displaying each feature on a map.
    #' @details
    #' The relative order is fixed at 1 because each this class
    #' only contains a single feature.
    #' @return `numeric` value.
    get_feature_order = function() {
      1
    },

    #' @description
    #' Set relative order for displaying features on a map.
    #' @param ... not used.
    set_feature_order = function(...) {
      stop("This class has a fixed feature order.")
    },

    #' @description
    #' Get data for displaying the theme in a [solutionSettings] widget.
    #' @return `list` with widget data.
    get_solution_settings_widget_data = function() {
      list(
        id = self$id,
        name = self$name,
        feature_name = self$feature[[1]]$name,
        feature_id = self$feature[[1]]$id,
        feature_status = self$feature[[1]]$status,
        feature_total_amount = self$feature[[1]]$variable$total,
        feature_current_held = self$feature[[1]]$current,
        feature_min_goal = self$feature[[1]]$min_goal,
        feature_max_goal = self$feature[[1]]$max_goal,
        feature_goal = self$feature[[1]]$goal,
        feature_limit_goal = self$feature[[1]]$limit_goal,
        feature_step_goal = self$feature[[1]]$step_goal,
        units = self$feature[[1]]$variable$units
      )
    },

    #' @description
    #' Get data for displaying the theme in a [mapManager()] widget.
    #' @return `list` with widget data.
    get_map_manager_widget_data = function() {
      list(
        id = self$id,
        name = self$name,
        feature_name = self$feature[[1]]$name,
        feature_id = self$feature[[1]]$id,
        feature_visible = self$feature[[1]]$visible,
        feature_legend = self$feature[[1]]$variable$legend$get_widget_data(),
        units = self$feature[[1]]$variable$units,
        type = "theme"
      )
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
#' # find data file paths
#' f1 <- system.file(
#'   "extdata", "projects", "sim_raster", "sim_raster_spatial.tif",
#'   package = "wheretowork"
#' )
#' f2 <- system.file(
#'   "extdata",  "projects", "sim_raster", "sim_raster_attribute.csv.gz",
#'   package = "wheretowork"
#' )
#' f3 <- system.file(
#'   "extdata",  "projects", "sim_raster", "sim_raster_boundary.csv.gz",
#'   package = "wheretowork"
#' )
#'
#' # create new dataset
#' d <- new_dataset(f1, f2, f3)
#'
#' # create new variable
#' v <- new_variable_from_auto(d, index = 1)
#'
#' # create new feature
#' f <- new_feature(name = "Intact Alvar", variable = v)
#'
#' # create a theme using the single feature
#' st <- new_single_theme(name = "Intact Alvar", feature = f)
#'
#' # print object
#' print(st)
#' @export
new_single_theme <- function(name,
                             feature,
                             id = uuid::UUIDgenerate()) {
  # convert to list
  if (inherits(feature, "Feature")) {
    feature <- list(feature)
  }
  # return new feature
  SingleTheme$new(
    id = id,
    feature = feature,
    name = name
  )
}
