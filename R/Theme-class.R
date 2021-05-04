#' @include internal.R Feature-class.R
NULL

#' Theme class
#'
#' Definition for the Theme class. This is a parent class that contains
#' shared fields and methods for the [SingleTheme] and [MultiTheme] classes.
#'
#' @seealso [new_single_theme], [new_multi_theme].
Theme <- R6::R6Class(
  "Theme",
  public = list(

    #' @field id `character` value.
    id = NA_character_,

    #' @field name `character` value.
    name = NA_character_,

    #' @field feature `list` of [Feature] objects.
    feature = list(),

    #' @field status `logical` value.
    status = NA,

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
    #' Get feature goals.
    #' @return `numeric` vector of goal values.
    get_feature_goals = function() {
      vapply(self$feature, FUN.VALUE = numeric(1), function(x) x$get_goal())
    },

    #' @description
    #' Get feature statuses.
    #' @return `logical` vector of status values.
    get_feature_statuses = function() {
      vapply(self$feature, FUN.VALUE = logical(1), function(x) x$get_status())
    },

    #' @description
    #' Set feature goals.
    #' @param value `numeric` vector of new values.
    set_feature_goals = function(value) {
      assertthat::assert_that(
        is.numeric(value),
        assertthat::noNA(value),
        length(value) == length(self$feature))
      for (i in seq_along(value)) {
        self$feature[[i]]$set_goal(value[i])
      }
      invisible(self)
    },

    #' @description
    #' Set feature goals.
    #' @param value `logical` vector of new values.
    set_feature_statuses = function(value) {
      assertthat::assert_that(
        is.logical(value),
        assertthat::noNA(value),
        length(value) == length(self$feature))
      for (i in seq_along(value)) {
        self$feature[[i]]$set_status(value[i])
      }
      invisible(self)
    }

  )
)

#' New theme
#'
#' Create a new [Theme] object. This is a convience wrapper for
#' the [new_single_theme()] and [new_multi_theme()] functions.
#'
#' @param ... arguments to be passed to [new_single_theme()]
#'  or [new_multi_theme()].
#'
#' @return A [Theme] object.
#'
#' @examples
#' # create new layer
#' l <- new_layer(source = tempfile(), current = 0.1, total = 12, units = "ha")
#'
#' # create feature using the layer
#' f <- new_feature(name = "Intact Alvar Occurrence", layer = l)
#'
#' # create a theme using the single feature
#' st <- new_theme(name = "Inact Alvar", feature = f)
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
