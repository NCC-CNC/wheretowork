#' @include internal.R class_Variable.R class_ThemeResults.R
NULL

#' MultiThemeResults class
#'
#' Definition for the MultiThemeResults class.
#'
#' @seealso [new_theme_results()].
#'
#' @export
MultiThemeResults <- R6::R6Class(
  "MultiThemeResults",
  inherit = ThemeResults,
  public = list(
    #' @description
    #' Get results.
    #' @return [tibble::tibble()] object.
    get_results_data = function() {
      tibble::tibble(
        name = self$theme$name,
        feature_name =
          vapply(
            self$feature_results,
            function(x) x$feature$name,
            character(1)),
        feature_status =
          vapply(self$feature_results, `[[` , logical(1), "status"),
        feature_total_amount =
          vapply(
            self$feature_results,
            function(x) x$feature$variable$total,
            numeric(1)),
        feature_current_held =
          vapply(self$feature_results, `[[` , numeric(1), "current"),
        feature_goal =
          vapply(self$feature_results, `[[` , numeric(1), "goal"),
        feature_solution_held =
          vapply(self$feature_results, `[[` , numeric(1), "held"),
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
        feature_name =
          vapply(
            self$feature_results,
            function(x) x$feature$name,
            character(1)),
        feature_id =
          vapply(self$feature_results, `[[` , character(1), "id"),
        feature_status =
          vapply(self$feature_results, `[[` , logical(1), "status"),
        feature_total_amount =
          vapply(
            self$feature_results,
            function(x) x$feature$variable$total,
            numeric(1)),
        feature_current_held =
          vapply(self$feature_results, `[[` , numeric(1), "current"),
        feature_goal =
          vapply(self$feature_results, `[[` , numeric(1), "goal"),
        feature_solution_held =
          vapply(self$feature_results, `[[` , numeric(1), "held"),
        units = self$feature_results[[1]]$feature$variable$units,
        type = "theme_results"
      )
    }
  )
)
