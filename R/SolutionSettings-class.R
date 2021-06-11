#' @include internal.R Weight-class.R Theme-class.R SingleTheme-class.R
#' @include MultiTheme-class.R
NULL

#' Solution settings class
#'
#' Definition for the SolutionSettings class.
SolutionSettings <- R6::R6Class(
  "SolutionSettings",
  public = list(

    #' @field theme_ids `character` vector of identifiers for the themes.
    theme_ids = NULL,

    #' @field weight_ids `character` vector of identifiers for the weights.
    weight_ids = NULL,

    #' @field themes `list` of [Theme] objects.
    themes = NULL,

    #' @field weights `list` of [Weight] objects.
    weights = NULL,

    #' @description
    #' Create a SolutionSettings object.
    #' @param themes `list` of [Theme] objects.
    #' @param weights `list` of [Weight] objects.
    #' @return A new SolutionSettings object.
    initialize = function(themes, weights) {
      assertthat::assert_that(
        is.list(themes),
        is.list(weights),
        all_list_elements_inherit(themes, "Theme"),
        all_list_elements_inherit(weights, "Weight"))
      self$themes <- themes
      self$weights <- weights
      self$theme_ids <- vapply(themes, function(x) x$id, character(1))
      self$weight_ids <- vapply(weights, function(x) x$id, character(1))
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("SolutionSettings")
      # print themes
      if (length(self$themes) > 0) {
        message("  themes: ")
        for (x in vapply(self$themes, function(x) x$repr(), character(1))) {
          message("    " , gsub(nl(), paste0(nl(), "    "), x, fixed = TRUE))
        }
      } else {
        message("  themes: none")
      }
      ## print weights
      if (length(self$weights) > 0) {
        message("  weights: ")
        for (x in vapply(self$weights, function(x) x$repr(), character(1))) {
          message("    " , gsub(nl(), paste0(nl(), "    "), x, fixed = TRUE))
        }
      } else {
        message("  weights: none")
      }
      invisible(self)
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @return `character` value.
    repr = function() {
      "SolutionSettings object"
    },

    #' @description
    #' Get a theme.
    #' @param value `character` theme identifier.
    #' @return [Theme] object.
    get_theme = function(value) {
      assertthat::assert_that(
        assertthat::is.string(value),
        assertthat::noNA(value))
      assertthat::assert_that(
        value %in% self$theme_ids,
        msg = paste0("no theme with the id `", value,"`"))
      self$themes[[which(self$theme_ids == value)]]
    },

    #' @description
    #' Get a weight.
    #' @param value `character` weight identifier.
    #' @return [Weight] object.
    get_weight = function(value) {
      assertthat::assert_that(
        assertthat::is.string(value),
        assertthat::noNA(value))
      assertthat::assert_that(
        value %in% self$weight_ids,
        msg = paste0("no weight with the id `", value,"`"))
      self$weights[[which(self$weight_ids == value)]]
    },

    #' @description
    #' Get a parameter for a weight or theme.
    #' @param value `list` with new parameter information (see Details section)
    #' @details
    #' The argument to `value` should be a `list` with the following elements:
    #' \describe{
    #' \item{id}{`character` identifier for theme or weight.}
    #' \item{parameter}{`character` name of parameter.
    #'   Available options are: `"status"`, `"factor"`, or `"goal"`.}
    #' \item{type}{`character` indicating if the parameter is for a theme or
    #'   weight.
    #'   Available options are: `"theme"` or `"weight"`.}
    #' }
    get_parameter = function(value) {
      assertthat::assert_that(
        is.list(value),
        assertthat::has_name(value, "id"),
        assertthat::is.string(value$id),
        assertthat::has_name(value, "parameter"),
        assertthat::is.string(value$parameter),
        assertthat::has_name(value, "type"),
        assertthat::is.string(value$type),
        isTRUE(value$type %in% c("theme", "weight")))
      if (identical(value$type, "theme")) {
        self$get_theme(value$id)$get_parameter(value$parameter)
      } else if (identical(value$type, "weight")) {
        self$get_weight(value$id)$get_parameter(value$parameter)
      }
    },

    #' @description
    #' Set a parameter for a weight or theme.
    #' @param value `list` with new parameter information (see Details section)
    #' @details
    #' The argument to `value` should be a `list` with the following elements:
    #' \describe{
    #' \item{id}{`character` identifier for theme or weight.}
    #' \item{parameter}{`character` name of parameter.
    #'   Available options are: `"status"`, `"factor"`, or `"goal"`.}
    #' \item{value}{`numeric` or `logical` value for new parameter.}
    #' \item{type}{`character` indicating if the parameter is for a theme or
    #'   weight.
    #'   Available options are: `"theme"` or `"weight"`.}
    #' }
    set_parameter = function(value) {
      assertthat::assert_that(
        is.list(value),
        assertthat::has_name(value, "id"),
        assertthat::is.string(value$id),
        assertthat::has_name(value, "parameter"),
        assertthat::is.string(value$parameter),
        assertthat::has_name(value, "value"),
        assertthat::is.string(value$type),
        isTRUE(value$type %in% c("theme", "weight")))
      if (identical(value$type, "theme")) {
        self$get_theme(value$id)$set_parameter(value$parameter, value$value)
      } else if (identical(value$type, "weight")) {
        self$get_weight(value$id)$set_parameter(value$parameter, value$value)
      }
    },

    #' @description
    #' Get data for displaying the theme in a [solutionSettings()] widget.
    #' @return `list` with widget data.
    get_widget_data = function() {
      list(
        themes =
          lapply(
            self$themes, function(x) x$get_solution_settings_widget_data()),
        weights =
          lapply(
            self$weights, function(x) x$get_solution_settings_widget_data())
      )
    },

    #' @description
    #' Get theme settings for generating a prioritization.
    #' @return [tibble::tibble()] with data.
    get_theme_settings = function() {
      # extract data
      ids <- lapply(self$themes, function(x) x$get_feature_id())
      nms <- lapply(self$themes, function(x) x$get_feature_name())
      currents <- lapply(self$themes, function(x) x$get_feature_current())
      statuses <- lapply(self$themes, function(x) x$get_feature_status())
      goals <- lapply(self$themes, function(x) x$get_feature_goal())
      totals <- lapply(self$themes, function(x) x$get_feature_total())
      # return result
      tibble::tibble(
        id = do.call(c, ids),
        name = do.call(c, nms),
        status = do.call(c, statuses),
        goal = do.call(c, goals),
        current = do.call(c, currents),
        total = do.call(c, totals))
    },

    #' @description
    #' Get weight settings for generating a prioritization.
    #' @return [tibble::tibble()] with data.
    get_weight_settings = function() {
      tibble::tibble(
        id = vapply(self$weights, function(x) x$id, character(1)),
        name = vapply(self$weights, function(x) x$name, character(1)),
        status = vapply(self$weights, function(x) x$status, logical(1)),
        factor = vapply(self$weights, function(x) x$factor, numeric(1)))
    },

    #' @description
    #' Get rij data for the themes.
    #' @return `matrix` with data.
    get_rij_matrix = function() {
      v <- lapply(self$themes, function(x) {
        lapply(x$feature, `[[`, "variable")
      })
      fid <- lapply(self$themes, function(x) {
        lapply(x$feature, `[[`, "id")
      })
      fid <- base::unlist(fid, recursive = TRUE, use.names = FALSE)
      out <- extract_data_matrix(do.call(c, v))
      rownames(out) <- fid
      out
    },

    #' @description
    #' Get wij data for the themes.
    #' @return `matrix` with data.
    get_wij_matrix = function() {
      v <- lapply(self$weights, `[[`, "variable")
      out <- extract_data_matrix(v)
      rownames(out) <- vapply(self$weights, `[[`, character(1), "id")
      out
    }

  )
)

#' New solution settings
#'
#' Create a new [SolutionSettings] object.
#'
#' @param themes `list` of [Theme] objects. D
#'
#' @param weights `list` of [Weight] objects.
#'
#' @return A [SolutionSettings] object.
#'
#' @examples
#' # create dataset
#' f <- system.file("extdata", "sim_raster_data.tif", package = "locationmisc")
#' d <- new_dataset(f)
#'
#' # create variables
#' v1 <- new_variable_from_auto(dataset = d, index = 1)
#' v2 <- new_variable_from_auto(dataset = d, index = 2)
#' v3 <- new_variable_from_auto(dataset = d, index = 3)
#' v4 <- new_variable_from_auto(dataset = d, index = 4)
#'
#' # create a weight using a variable
#' w <- new_weight(
#'   name = "Human Footprint Index", variable = v1,
#'   initial_factor = 90, initial_status = FALSE, id = "W1")
#'
#' # create features using variables
#' f1 <- new_feature(
#'   name = "Possum", variable = v2,
#'   initial_goal = 0.2, initial_status = FALSE, current = 0.5, id = "F1")
#' f2 <- new_feature(
#'   name = "Forests", variable = v3,
#'   initial_goal = 0.3, initial_status = FALSE, current = 0.9, id = "F2")
#' f3 <- new_feature(
#'   name = "Shrubs", variable = v4,
#'   initial_goal = 0.6, initial_status = TRUE, current = 0.4, id = "F3")
#'
#' # create themes using the features
#' t1 <- new_single_theme("Species", f1, id = "T1")
#' t2 <- new_multi_theme("Ecoregions", list(f2, f3), id = "T2")
#'
#' # create solution settings using the themes and weight
#' ss <- new_solution_settings(themes = list(t1, t2), weights = list(w))
#'
#' # print object
#' print(ss)
#'
#' @export
new_solution_settings <- function(themes, weights) {
  SolutionSettings$new(themes = themes, weights = weights)
}
