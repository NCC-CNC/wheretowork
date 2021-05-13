#' @include internal.R Feature-class.R
NULL

#' Theme class
#'
#' Definition for the Theme class. This is a parent class that contains
#' shared fields and methods for the [SingleTheme] and [MultiTheme] classes.
#'
#' @seealso [new_single_theme()], [new_multi_theme()].
Theme <- R6::R6Class(
  "Theme",
  public = list(

    #' @field id `character` value.
    id = NA_character_,

    #' @field name `character` value.
    name = NA_character_,

    #' @field feature `list` of [Feature] objects.
    feature = list(),

    #' @field mandatory `logical` value.
    mandatory = FALSE,

    #' @field initial_status `logical` value.
    initial_status = NA,

    #' @field round `logical` value.
    round = NA,

    #' @field icon `shiny.tag` value.
    icon = NULL,

    #' @description
    #' Create a Theme object.
    #' @details This method intentionally throws an error.
    #' @param ... not used.
    initialize = function(...) {
      stop("The super `Theme` class should not be interacted with directly.")
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Theme")
      message("  id:        ", self$id)
      message("  name:      ", self$name)
      message("  mandatory: ", self$mandatory)
      message("  feature: ")
      for (x in vapply(self$feature, function(x) x$repr(), character(1))) {
        message("    " , x)
      }
      invisible(self)
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @param ... not used.
    #' @return `character` value.
    repr = function(...) {
      paste0(
        self$name,
        ifelse(self$mandatory, " (mandatory):", ":"),
        nl(),
        paste(
          paste0(
            "  ",
            gsub(
              nl(), paste0(nl(), "  "),
              vapply(self$feature, function(x) x$repr(), character(1))),
            collapse = nl())))
    },

    #' @description
    #' Get feature goal values.
    #' @return `numeric` vector with goal value(s).
    get_feature_goal = function() {
      vapply(self$feature, FUN.VALUE = numeric(1), function(x) x$get_goal())
    },

    #' @description
    #' Get feature status values.
    #' @return `logical` vector with status value(s).
    get_feature_status = function() {
      vapply(self$feature, FUN.VALUE = logical(1), function(x) x$get_status())
    },

    #' @description
    #' Get parameter.
    #' @param name `character` parameter name.
    #' Available options are `"feature_status"` or `"feature_goal"`.
    #' @return Value.
    get_parameter = function(name) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c("feature_status", "feature_goal"))
      if (identical(name, "feature_status")) {
        out <- self$get_feature_status()
      } else if (identical(name, "feature_goal")) {
        out <- self$get_feature_goal()
      }
      out
    },

    #' @description
    #' Set feature goal values.
    #' @param value `numeric` vector containing a value for each feature.
    #'   A `list` of `numeric` values can also be supplied.
    set_feature_goal = function(value) {
      if (is.list(value))
        value <- unlist(value, recursive = TRUE, use.names = TRUE)
      assertthat::assert_that(
        is.numeric(value),
        assertthat::noNA(value),
        length(value) == length(self$feature))
      for (i in seq_along(value)) {
        self$feature[[i]]$set_goal(value[[i]])
      }
      invisible(self)
    },

    #' @description
    #' Set feature status values.
    #' @param value `logical` vector containing a value for each feature.
    #'   A `list` of `logical` values can also be supplied.
    set_feature_status = function(value) {
      if (is.list(value))
        value <- unlist(value, recursive = TRUE, use.names = TRUE)
      assertthat::assert_that(
        is.logical(value),
        assertthat::noNA(value),
        length(value) == length(self$feature))
      for (i in seq_along(value)) {
        self$feature[[i]]$set_status(value[[i]])
      }
      invisible(self)
    },

    #' @description
    #' Set parameter.
    #' @param name `character` parameter name.
    #' Available options are `"feature_status"` or `"feature_goal"`.
    #' @param value vector containing a value for each feature.
    set_parameter = function(name, value) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c("feature_status", "feature_goal"))
      if (identical(name, "feature_status")) {
        self$set_feature_status(value)
      } else if (identical(name, "feature_goal")) {
        self$set_feature_goal(value)
      }
      invisible(self)
    }
  )
)

#' New theme
#'
#' Create a new [Theme] object. This is a convenience wrapper for
#' the [new_single_theme()] and [new_multi_theme()] functions.
#'
#' @param ... arguments to be passed to [new_single_theme()]
#'  or [new_multi_theme()].
#'
#' @return A [Theme] object.
#'
#' @examples
#' # create new dataset
#' l <- new_dataset(
#'  source = tempfile(), total = 12, units = "ha",
#'  legend = new_continuous_legend(1, 100, c("#000000", "#1b9e77")))
#'
#' # create feature using the dataset
#' f <- new_feature(name = "Intact Alvar Occurrence", dataset = l)
#'
#' # create a theme using the single feature
#' st <- new_theme(name = "Intact Alvar", feature = f)
#'
#' # print object
#' print(st)
#'
#' @export
new_theme <- function(...) {
  args <- list(...)
  assertthat::assert_that(
    !is.null(args$feature),
    msg = "missing `feature` argument")
  if (inherits(args$feature, "Feature") ||
     (is.list(args$feature) && length(args$feature) == 1)) {
    out <- do.call(what = new_single_theme, args = args)
  } else {
    out <- do.call(what = new_multi_theme, args = args)
  }
  out
}
