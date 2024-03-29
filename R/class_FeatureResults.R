#' @include internal.R class_Variable.R
NULL

#' Feature results class
#'
#' Definition for the `FeatureResults` class.
#'
#' @seealso [new_feature_results()].
#'
#' @export
FeatureResults <- R6::R6Class(
  "FeatureResults",
  public = list(
    #' @field id `character` value.
    id = NA_character_,

    #' @field feature [Feature] object.
    feature = NULL,

    #' @field status `logical` value.
    status = NA,

    #' @field current `numeric` value.
    current = NA_real_,

    #' @field goal `numeric` value.
    goal = NA_real_,

    #' @field held `numeric` value.
    held = NA_real_,

    #' @description
    #' Create a new `FeatureResults` object.
    #' @param id `character` value.
    #' @param feature [Feature] value.
    #' @param held `character` value.
    #' @return A new `FeatureResults` object.
    ## constructor
    initialize = function(id, feature, held) {
      ### assert that arguments are valid
      assertthat::assert_that(
        #### id
        assertthat::is.string(id),
        assertthat::noNA(id),
        ### feature$
        inherits(feature, "Feature"),
        #### held
        assertthat::is.number(held),
        assertthat::noNA(held)
      )
      ### set fields
      self$id <- id
      self$feature <- feature
      self$held <- held
      self$current <- feature$current
      self$goal <- feature$goal
      self$status <- feature$status
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("FeatureResults")
      message("  id:      ", self$id)
      message("  status: ", self$status)
      message("  current: ", round(self$current, 2))
      message("  goal: ", round(self$goal, 2))
      message("  held: ", round(self$held, 2))
      message(
        "  feature: ",
        gsub("\n", "\n  ", self$feature$repr())
      )
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
      paste0(
        self$name,
        " ", start, "status: ", self$status,
        ", current: ", round(self$current, 2), end, nl(),
        ", goal: ", round(self$goal, 2), end, nl(),
        ", held: ", round(self$held, 2), end, nl(),
        "  feature: ", self$feature$repr()
      )
    }
  )
)

#' New feature results
#'
#' Create a new [FeatureResults] object to store results for a solution.
#'
#' @param feature [Feature] object.
#'
#' @param held `numeric` proportion of the feature held by the solution.
#'   (e.g. 0.1 = 10%).
#'
#' @inheritParams new_feature
#'
#' @return A [FeatureResults] object.
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
#' # create a feature results object to store results for the feature
#' fr <- new_feature_results(f, held = 0.8)
#'
#' # print object
#' print(fr)
#' @export
new_feature_results <- function(feature, held, id = uuid::UUIDgenerate()) {
  FeatureResults$new(
    id = id,
    feature = feature,
    held = held
  )
}
