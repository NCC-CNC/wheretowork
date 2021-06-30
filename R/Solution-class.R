#' @include internal.R
NULL


#' Solution class
#'
#' Definition for the Solution class.
#'
#' @seealso [new_solution()].
Solution <- R6::R6Class(
  "Solution",
  public = list(

    #' @field id `character` identifier.
    id = NA_character_,

    #' @field name `character` name of solution.
    name = NA_character_,

    #' @field initial_visible `logical` value.
    initial_visible = NA,

    #' @field variable [Variable] object.
    variable = NULL,

    #' @field visible `logical` value.
    visible = NA,

    #' @field statistics `list` of [Statistic] objects
    statistics = NULL,

    #' @field theme_results `list` of [ThemeResults] objects.
    theme_results = NULL,

    #' @field weight_results `list` of [WeightResults] objects.
    weight_results = NULL,

    #' @description
    #' Create a Solution object.
    #' @param id `character` value.
    #' @param name `character` value.
    #' @param variable [Variable] object.
    #' @param initial_visible `logical` value.
    #' @param statistics `list` of [Statistic] objects.
    #' @param theme_results `list` of [ThemeResults] objects.
    #' @param weight_results `list` of [WeightResults] objects.
    #' @return A new Solution object.
    initialize = function(
      id, name, variable, initial_visible, statistics, theme_results,
      weight_results) {
      # assert arguments are valid
      assertthat::assert_that(
        assertthat::is.string(id),
        assertthat::noNA(id),
        assertthat::is.string(name),
        assertthat::noNA(name),
        inherits(variable, "Variable"),
        assertthat::is.flag(initial_visible),
        assertthat::noNA(initial_visible),
        is.list(statistics),
        all_list_elements_inherit(statistics, "Statistic"),
        is.list(theme_results),
        all_list_elements_inherit(theme_results, "ThemeResults"),
        is.list(weight_results),
        all_list_elements_inherit(weight_results, "WeightResults"))
      # assign fields
      self$id <- id
      self$name <- name
      self$variable <- variable
      self$visible <- initial_visible
      self$initial_visible <- initial_visible
      self$statistics  <- statistics
      self$theme_results <- theme_results
      self$weight_results <- weight_results
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
        " ",
        start,
        paste(
          vapply(self$statistics, function(x) x$repr(), character(1)),
          collapse = ", "),
        end,
        nl(),
        "  variable: ", self$variable$repr())
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Solution")
      message("  id:      ", self$id)
      message("  name:    ", self$name)
      invisible(self)
    },

    #' @description
    #' Get layer names.
    #' @return `character` vector.
    get_layer_name = function() {
      self$name
    },

    #' @description
    #' Get layer index values.
    #' @return `character` vector.
    get_layer_index = function() {
      self$variable$index
    },

    #' @description
    #' Get visible.
    #' @return `logical` value.
    get_visible = function() {
      self$visible
    },

    #' @description
    #' Get setting.
    #' @param name `character` setting name.
    #' Available options are `"visible"`.
    #' @return Value.
    get_setting = function(name) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c("visible"))
      if (identical(name, "visible")) {
        out <- self$get_visible()
      } else {
        stop(paste0("\"", name, "\" is not a setting"))
      }
      out
    },

    #' @description
    #' Set visible.
    #' @param value `logical` new value.
    set_visible = function(value) {
      assertthat::assert_that(
        assertthat::is.flag(value),
        assertthat::noNA(value))
      self$visible <- value
      invisible(self)
    },

    #' @description
    #' Set setting.
    #' @param name `character` setting name.
    #' Available options are `"visible"``.
    #' @param value `ANY` new value.
    set_setting = function(name, value) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c("visible"))
      if (identical(name, "visible")) {
        self$set_visible(value)
      } else {
        stop(paste0("\"", name, "\" is not a setting"))
      }
      invisible(self)
    },

    #' @description
    #' Get data for displaying the theme in a [solutionResults()] widget.
    #' @return `list` with widget data.
    get_solution_results_widget_data = function() {
      list(
        id = self$id,
        name = self$name,
        statistics = lapply(
          self$statistics,
          function(x) x$get_widget_data()),
        theme_results = lapply(
          self$theme_results,
          function(x) x$get_widget_data()),
        weight_results = lapply(
          self$weight_results,
          function(x) x$get_widget_data()))
    },

    #' @description
    #' Get data for displaying the theme in a [mapManager()] widget.
    #' @return `list` with widget data.
    get_map_manager_widget_data = function() {
      list(
        id = self$id,
        name = self$name,
        visible = self$visible,
        legend = self$variable$legend$get_widget_data(),
        units = self$variable$units,
        type = "solution"
      )
    },

    #' @description
    #' Render on map.
    #' @param x [leaflet::leaflet()] object.
    #' @param zindex `numeric` z-index for ordering.
    #' @return [leaflet::leaflet()] object.
    render_on_map = function(x, zindex) {
      self$variable$render(x, self$id, zindex, self$visible)
    },

    #' @description
    #' Render on map.
    #' @param x [leaflet::leafletProxy()] object.
    #' @param zindex `numeric` z-index for ordering.
    #' @return [leaflet::leafletProxy()] object.
    update_on_map = function(x, zindex) {
      self$variable$update_render(x, self$id, zindex, self$visible)
    }

  )
)

#' New solution
#'
#' Create a new [Solution] object.
#'
#' @param name `character` name for new solution.
#'
#' @param variable [Variable] object with the solution.
#'
#' @param initial_visible `logical` should the solution be visible on a map?
#'
#' @param statistics `list` of [Statistic] objects.
#'
#' @param theme_results `list` of [ThemeResults] objects.
#'
#' @param weight_results `list` of [WeightResults] objects.
#'
#' @inheritParams new_multi_theme
#'
#' @examples
#' #TODO
#'
#' @export
new_solution <- function(
  name, variable, initial_visible, statistics,
  theme_results, weight_results, id = uuid::UUIDgenerate()) {
  Solution$new(
    name = name,
    variable = variable,
    initial_visible = initial_visible,
    statistics = statistics,
    theme_results = theme_results,
    weight_results = weight_results,
    id = id)
}
