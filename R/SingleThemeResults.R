#' @include internal.R Variable-class.R ThemeResults-class.R
NULL

#' SingleThemeResults class
#'
#' Definition for the SingleThemeResults class.
#'
#' @seealso [new_theme_results()].
#'
#' @export
SingleThemeResults <- R6::R6Class(
  "SingleThemeResults",
  inherit = ThemeResults,
  public = list(
    #' @description
    #' Get data for displaying the object in a [solutionResults()] widget.
    #' @return `list` with widget data.
    get_widget_data = function() {
      list(
        id = self$id,
        name = self$theme$name,
        feature_name = self$feature_results[[1]]$feature$name,
        feature_id = self$feature_results[[1]]$feature$id,
        feature_status = self$feature_results[[1]]$status,
        feature_total_amount = self$feature_results[[1]]$feature$variable$total,
        feature_current_held = self$feature_results[[1]]$feature$current,
        feature_goal = self$feature_results[[1]]$goal,
        feature_solution_held = self$feature_results[[1]]$held,
        units = self$feature_results[[1]]$feature$variable$units,
        mandatory = self$theme$mandatory,
        round = self$theme$round,
        icon = as.character(self$theme$icon),
        type ="theme_results"
      )
    }
  )
)
