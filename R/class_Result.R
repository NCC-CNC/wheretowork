#' @include internal.R class_Solution.R
NULL

#' Result class
#'
#' Definition for the Result class.
#'
#' @seealso [new_result()].
#'
#' @export
Result <- R6::R6Class(
  "Result",
  public = list(
    #' @field id `character` value.
    id = NULL,

    #' @field values `numeric` vector.
    values = NULL,

    #' @field area `numeric` value.
    area = NULL,

    #' @field perimeter `numeric` value.
    perimeter = NULL,

    #' @field theme_coverage `numeric` value.
    theme_coverage = NULL,

    #' @field weight_coverage `numeric` value.
    weight_coverage = NULL,

    #' @field include_coverage `numeric` value.
    include_coverage = NULL,

    #' @field theme_settings `data.frame` object.
    theme_settings = NULL,

    #' @field weight_settings `data.frame` object.
    weight_settings = NULL,

    #' @field include_settings `data.frame` object.
    include_settings = NULL,

    #' @field parameters `list` of [Parameter] objects.
    parameters = NULL,

    #' @description
    #' Create a new Result object.
    #' @param id `character` value.
    #' @param values `numeric` vector.
    #' @param area `numeric` value.
    #' @param perimeter `numeric` value.
    #' @param theme_coverage `numeric` vector.
    #' @param weight_coverage `numeric` vector.
    #' @param include_coverage `numeric` vector.
    #' @param theme_settings `logical` value.
    #' @param weight_settings `logical` value.
    #' @param include_settings `logical` value.
    #' @param parameters `list` of [Parameter] objects.
    #' @return A new Result object.
    ## constructor
    initialize = function(id, values,
                          area, perimeter,
                          theme_coverage, weight_coverage, include_coverage,
                          theme_settings, weight_settings, include_settings,
                          parameters) {
      ### assert that arguments are valid
      assertthat::assert_that(
        ### id
        assertthat::is.string(id),
        assertthat::noNA(id),
        ### values
        is.numeric(values),
        assertthat::noNA(values),
        ### area
        assertthat::is.number(area),
        assertthat::noNA(area),
        ### perimeter
        assertthat::is.number(perimeter),
        assertthat::noNA(perimeter),
        ### theme_coverage
        is.numeric(theme_coverage),
        assertthat::noNA(theme_coverage),
        ### weight_coverage
        is.numeric(weight_coverage),
        assertthat::noNA(weight_coverage),
        ### include_coverage
        is.numeric(include_coverage),
        assertthat::noNA(include_coverage),
        #### theme_settings
        inherits(theme_settings, "data.frame"),
        nrow(theme_settings) == length(theme_coverage),
        identical(theme_settings$id, names(theme_coverage)),
        #### weight_settings
        inherits(weight_settings, "data.frame"),
        nrow(weight_settings) == length(weight_coverage),
        #### include_settings
        inherits(include_settings, "data.frame"),
        nrow(include_settings) == length(include_coverage),
        #### parameters
        is.list(parameters),
        all_list_elements_inherit(parameters, "Parameter")
      )
      if (nrow(weight_settings) > 0) {
        assertthat::assert_that(
          identical(weight_settings$id, names(weight_coverage))
        )
      }
      if (nrow(include_settings) > 0) {
        assertthat::assert_that(
          identical(include_settings$id, names(include_coverage))
        )
      }
      ### set fields
      self$id <- id
      self$area <- area
      self$perimeter <- perimeter
      self$values <- values
      self$theme_coverage <- theme_coverage
      self$weight_coverage <- weight_coverage
      self$include_coverage <- include_coverage
      self$theme_settings <- theme_settings
      self$weight_settings <- weight_settings
      self$include_settings <- include_settings
      self$parameters <- parameters
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Result object")
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
      paste0("Result object")
    }

  )
)

#' New Result
#'
#' Create a new [Result] object. This object is used to store information
#' used to generate a [Solution] object.
#'
#' @param values `numeric` status of planning units.
#'
#' @param area `numeric` total area (m^2) of selected planning units.
#'
#' @param perimeter `numeric` total perimeter (m) of selected planning units.
#'
#' @param theme_coverage `numeric` vector containing the proportion of each
#'  feature within each theme that is covered by the result.
#'
#' @param weight_coverage `numeric` vector containing the proportion of each
#'  weight that is covered by the result.
#'
#' @param include_coverage  `numeric`  vector containing the proportion of each
#'  include that is covered by the result.
#'
#' @param theme_settings `data.frame` containing the theme settings.
#'
#' @param weight_settings `data.frame` containing the weight settings.
#'
#' @param include_settings  `data.frame` containing the include settings.
#'
##' @param parameters  `list` of [Parameter] objects.
#"
#' @param id `character` identifier value.
#'
#' @return A [Result] object.
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
#' # create variables
#' v1 <- new_variable_from_auto(dataset = d, index = 1)
#' v2 <- new_variable_from_auto(dataset = d, index = 2)
#' v3 <- new_variable_from_auto(dataset = d, index = 3)
#' v4 <- new_variable_from_auto(dataset = d, index = 4)
#' v5 <- new_variable_from_auto(dataset = d, index = 5)
#'
#' # create a weight using a variable
#' w <- new_weight(
#'   name = "Human Footprint Index", variable = v1,
#'   factor = 90, status = FALSE, id = "W1"
#' )
#'
#' # create features using variables
#' f1 <- new_feature(
#'   name = "Possum", variable = v2,
#'   goal = 0.2, status = FALSE, current = 0.5, id = "F1"
#' )
#' f2 <- new_feature(
#'   name = "Forests", variable = v3,
#'   goal = 0.3, status = FALSE, current = 0.9, id = "F2"
#' )
#' f3 <- new_feature(
#'   name = "Shrubs", variable = v4,
#'   goal = 0.6, status = TRUE, current = 0.4, id = "F3"
#' )
#'
#' # create themes using the features
#' t1 <- new_theme("Species", f1, id = "T1")
#' t2 <- new_theme("Ecoregions", list(f2, f3), id = "T2")
#'
#' # create an include using a variable
#' i <- new_include(
#'   name = "Protected areas", variable = v5,
#'   status = FALSE, id = "I1"
#' )
#'
#' # create parameters
#' p1 <- new_parameter(name = "Spatial clustering")
#' p2 <- new_parameter(name = "Optimality gap")
#'
#' # create solution settings using the themes and weight
#' ss <- new_solution_settings(
#'   themes = list(t1, t2), weights = list(w), includes = list(i),
#'   parameters = list(p1, p2)
#' )
#'
#' # create solution values
#' values <- sample(
#'   c(0, 1), length(d$get_planning_unit_indices()), replace = TRUE
#' )
#'
#' # create object
#' r <- new_result(
#'   values = values,
#'   area = 12,
#'   perimeter = 10,
#'   theme_coverage = calculate_coverage(values, ss$get_theme_data()),
#'   weight_coverage = calculate_coverage(values, ss$get_weight_data()),
#'   include_coverage = calculate_coverage(values, ss$get_include_data()),
#'   theme_settings = ss$get_theme_settings(),
#'   weight_settings = ss$get_weight_settings(),
#'   include_settings = ss$get_include_settings(),
#'   parameters = ss$parameters
#' )
#'
#' # print object
#' print(r)
#'
#' @export
new_result <- function(values, area, perimeter,
                       theme_coverage, weight_coverage, include_coverage,
                       theme_settings, weight_settings, include_settings,
                       parameters,
                       id = uuid::UUIDgenerate()) {
  Result$new(
    id = id,
    values = values,
    area = area,
    perimeter = perimeter,
    theme_coverage = theme_coverage,
    weight_coverage = weight_coverage,
    include_coverage = include_coverage,
    theme_settings = theme_settings,
    weight_settings = weight_settings,
    include_settings = include_settings,
    parameters = parameters
  )
}
