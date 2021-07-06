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

    #' @field feature_order `numeric` value.
    feature_order = NA_real_,

    #' @description
    #' Create a MultiTheme object.
    #' @param id `character` value.
    #' @param name `character` value.
    #' @param feature `list` of [Feature] objects.
    #' @param mandatory `logical` value.
    #' @param feature_order `numeric` vector.
    #' @return A new MultiTheme object.
    initialize = function(
      id, name, feature, mandatory, feature_order) {
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
        #### feature_order
        is.numeric(feature_order),
        assertthat::noNA(feature_order),
        length(feature_order) == length(feature),
        identical(anyDuplicated(feature_order), 0L))
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
      self$feature_order <- feature_order
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
          vapply(self$feature, `[[`, character(1), "name"),
        feature_id =
          vapply(self$feature, `[[`, character(1), "id"),
        feature_status =
          vapply(self$feature, `[[`, logical(1), "status"),
        feature_total_amount =
          vapply(self$feature, function(x) x$variable$total, numeric(1)),
        feature_current_held =
          vapply(self$feature, `[[`, numeric(1), "current"),
        feature_min_goal =
          vapply(self$feature, `[[`, numeric(1), "min_goal"),
        feature_max_goal =
          vapply(self$feature, `[[`, numeric(1), "max_goal"),
        feature_goal =
          vapply(self$feature, `[[`, numeric(1), "goal"),
        feature_limit_goal =
          vapply(self$feature, `[[`, numeric(1), "limit_goal"),
        feature_step_goal =
          vapply(self$feature, `[[`, numeric(1), "step_goal"),
        units = self$feature[[1]]$variable$units,
        mandatory = self$mandatory
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
          vapply(self$feature, `[[`, character(1), "name"),
        feature_id =
          vapply(self$feature, `[[`, character(1), "id"),
        feature_visible =
          vapply(self$feature, `[[`, logical(1), "visible"),
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
#' @param feature_order `numeric` Relative order for displaying each feature
#'  on a map. Defaults to a reverse sequence of integer values.
#'
#' @param id `character` unique identifier.
#'   Defaults to a random identifier ([uuid::UUIDgenerate()]).
#'
#' @return A [MultiTheme] object.
#'
#' @examples
#' # find data file paths
#' f1 <- system.file(
#'   "extdata", "sim_raster_spatial.tif", package = "locationmisc")
#' f2 <- system.file(
#'  "extdata", "sim_raster_attribute.csv.gz", package = "locationmisc")
#' f3 <- system.file(
#'  "extdata", "sim_raster_boundary.csv.gz", package = "locationmisc")
#'
#' # create new dataset
#' d <- new_dataset(f1, f2, f3)
#'
#' # create new variable
#' v1 <- new_variable_from_auto(d, index = 1)
#' v2 <- new_variable_from_auto(d, index = 2)
#' v3 <- new_variable_from_auto(d, index = 3)
#'
#' # create new features
#' f1 <- new_feature(name = "Pangolin", variable = v1)
#' f2 <- new_feature(name = "Panda", variable = v2)
#' f3 <- new_feature(name = "Palms", variable = v3)
#'
#' # create a theme using the features
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
  feature_order = as.double(rev(seq_along(feature))),
  id = uuid::UUIDgenerate()) {
  # return new feature
  MultiTheme$new(
    id = id,
    name = name,
    feature = feature,
    mandatory = mandatory,
    feature_order = feature_order)
}
