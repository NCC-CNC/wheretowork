#' @include internal.R class_Variable.R
NULL

#' ThemeResults class
#'
#' Definition for the ThemeResults class.
#'
#' @seealso [new_theme_results()].
#'
#' @export
ThemeResults <- R6::R6Class(
  "ThemeResults",
  public = list(
    #' @field id `character` value.
    id = NA_character_,

    #' @field theme [Theme] object.
    theme = NA_character_,

    #' @field feature_results `list` of [FeatureResults] objects.
    feature_results = NULL,

    #' @description
    #' Create a new ThemeResults object.
    #' @param id `character` value.
    #' @param theme `[Theme] object.
    #' @param feature_results `list` of [FeatureResults] objects.
    #' @return A new ThemeResults object.
    ## constructor
    initialize = function(id, theme, feature_results) {
      ### assert that arguments are valid
      assertthat::assert_that(
        #### id
        assertthat::is.string(id),
        assertthat::noNA(id),
        #### theme
        inherits(theme, "Theme"),
        ### feature
        is.list(feature_results),
        all_list_elements_inherit(feature_results, "FeatureResults"),
        ### same number of features
        length(theme$feature) == length(feature_results)
      )
      ### set fields
      self$id <- id
      self$theme <- theme
      self$feature_results <- feature_results
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("ThemeResults")
      message("  id:      ", self$id)
      invisible(self)
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @param start `character` symbol used to start the setting list.
    #'   Defaults to `"["`.
    #' @param end `character` symbol used to start the setting list.
    #'   Defaults to `"]"`.
    #' @return `character` value.
    repr = function(start = "[", end = "]") {
      "TODO"
    },

    #' @description
    #' Get results.
    #' @return [tibble::tibble()] object.
    get_results_data = function() {
      tibble::tibble(
        name = self$theme$name,
        feature_name = vapply(
          self$feature_results,
          function(x) x$feature$name,
          character(1)
        ),
        feature_status = vapply(
          self$feature_results, `[[`, logical(1), "status"
        ),
        feature_total_amount = vapply(
          self$feature_results,
          function(x) x$feature$variable$total,
          numeric(1)
        ),
        feature_current_held = vapply(
          self$feature_results, `[[`, numeric(1), "current"
        ),
        feature_goal = vapply(
          self$feature_results, `[[`, numeric(1), "goal"
        ),
        feature_solution_held = vapply(
          self$feature_results, `[[`, numeric(1), "held"
        ),
        units = self$feature_results[[1]]$feature$variable$units,
      )
    },

    #' @description
    #' Get data for displaying the object in a [solutionResults()] widget.
    #' @return `list` with widget data.
    get_widget_data = function() {
      list(
        id = self$id,
        name = self$theme$name,
        feature_name = vapply(
          self$feature_results,
          function(x) x$feature$name,
          character(1)
        ),
        feature_id = vapply(
          self$feature_results, `[[`, character(1), "id"
        ),
        feature_status = vapply(
          self$feature_results, `[[`, logical(1), "status"
        ),
        feature_total_amount = vapply(
          self$feature_results,
          function(x) x$feature$variable$total,
          numeric(1)
        ),
        feature_current_held = vapply(
          self$feature_results, `[[`, numeric(1), "current"
        ),
        feature_goal = vapply(
          self$feature_results, `[[`, numeric(1), "goal"
        ),
        feature_solution_held = vapply(
          self$feature_results, `[[`, numeric(1), "held"
        ),
        units = self$feature_results[[1]]$feature$variable$units,
        type = "theme_results"
      )
    }
  )
)

#' New theme results
#'
#' Create a new [ThemeResults] object to store results for a solution.
#'
#' @param theme [Theme] object.
#'
#' @param feature_results [FeatureResults] object or a `list` of such objects.
#'
#' @inheritParams new_theme
#'
#' @return A [ThemeResults] object.
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
#' th <- new_theme(name = "Intact Alvar", feature = f)
#'
#' # create a feature results object to store results for the feature
#' fr <- new_feature_results(f, held = 0.8)
#'
#' # create a theme results object to store results for the feature
#' thr <- new_theme_results(th, fr)
#'
#' # print object
#' print(thr)
#' @export
new_theme_results <- function(theme, feature_results,
                              id = uuid::UUIDgenerate()) {
  assertthat::assert_that(
    is.list(feature_results) || inherits(feature_results, "FeatureResults")
  )
  if (inherits(feature_results, "FeatureResults")) {
    feature_results <- list(feature_results)
  }
  ThemeResults$new(
    id = id,
    theme = theme,
    feature_results = feature_results
  )
}
