#' @include internal.R Variable-class.R
NULL

#' Include class
#'
#' Definition for the Include class.
#'
#' @seealso [new_include()].
#'
#' @export
Include <- R6::R6Class(
  "Include",
  public = list(
    #' @field id `character` value.
    id = NA_character_,

    #' @field name `character` value.
    name = NA_character_,

    #' @field variable [Variable] object.
    variable = NULL,

    #' @field mandatory `logical` value.
    mandatory = FALSE,

    #' @field visible `logical` value.
    visible = NA,

    #' @field initial_visible `logical` value.
    initial_visible = NA,

    #' @field status `logical` value.
    status = NA,

    #' @field initial_status `logical` value.
    initial_status = NA,

    #' @description
    #' Create a new Lock object.
    #' @param id `character` value.
    #' @param name `character` value.
    #' @param variable [Variable] object.
    #' @param mandatory `logical` value.
    #' @param initial_visible `logical` value.
    #' @param initial_status `logical` value.
    #' @return A new Lock object.
    ## constructor
    initialize = function(
      id, name, variable, mandatory, initial_visible, initial_status) {
      ### assert that arguments are valid
      assertthat::assert_that(
        #### id
        assertthat::is.string(id),
        assertthat::noNA(id),
        #### name
        assertthat::is.string(name),
        assertthat::noNA(name),
        ### variable
        inherits(variable, "Variable"),
        #### mandatory
        assertthat::is.flag(mandatory),
        assertthat::noNA(mandatory),
        #### initial_visible
        assertthat::is.flag(initial_visible),
        assertthat::noNA(initial_visible),
        #### initial_status
        assertthat::is.flag(initial_status),
        assertthat::noNA(initial_status)
      )
      ### set fields
      self$id <- id
      self$variable <- variable
      self$name <- name
      self$status <- initial_status
      self$initial_status <- initial_status
      self$visible <- initial_visible
      self$initial_visible <- initial_visible
      self$mandatory <- mandatory
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Include")
      message("  id:       ", self$id)
      message("  name:     ", self$name)
      message("  variable: ", self$variable$repr())
      message("  visible:  ", self$visible)
      message("  status:   ", self$status)
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
        " ", start, "status: ", self$status, end, nl(),
        "  variable: ", self$variable$repr())
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
    #' Get status.
    #' @return `logical` value.
    get_status = function() {
      self$status
    },

    #' @description
    #' Get the data.
    #' @return [sf::st_as_sf()] or [raster::raster()] object.
    get_data = function() {
      self$variable$get_data()
    },

    #' @description
    #' Get setting.
    #' @param name `character` setting name.
    #' Available options are `"status"` or `"visible"`.
    #' @return Value.
    get_setting = function(name) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c("status", "visible"))
      if (identical(name, "status")) {
        out <- self$get_status()
      } else if (identical(name, "visible")) {
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
    #' Set status.
    #' @param value `logical` new value.
    set_status = function(value) {
      assertthat::assert_that(
        assertthat::is.flag(value),
        assertthat::noNA(value))
      self$status <- value
      invisible(self)
    },

    #' @description
    #' Set setting.
    #' @param name `character` setting name.
    #' Available options are `"status"` or `"visible"``.
    #' @param value `ANY` new value.
    set_setting = function(name, value) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c("status", "visible"))
      if (identical(name, "status")) {
        self$set_status(value)
      } else if (identical(name, "visible")) {
        self$set_visible(value)
      } else {
        stop(paste0("\"", name, "\" is not a setting"))
      }
      invisible(self)
    },

    #' @description
    #' Get data for displaying the theme in a [solutionSettings()] widget.
    #' @return `list` with widget data.
    get_solution_settings_widget_data = function() {
      list(
        id = self$id,
        name = self$name,
        status = self$status,
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
        visible = self$visible,
        legend = self$variable$legend$get_widget_data(),
        units = self$variable$units,
        type = "include"
      )
    },

    #' @description
    #' Export settings.
    #' @return `list` object.
    export = function() {
      list(
        name = self$name,
        variable = self$variable$export(),
        mandatory = self$mandatory,
        initial_status = self$status,
        initial_visible = self$visible
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

#' New include
#'
#' Create a new [Include] object.
#'
#' @inheritParams new_multi_theme
#' @inheritParams new_feature
#'
#' @return A [Include] object.
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
#' v <- new_variable_from_auto(d, index = 1)
#'
#' # create a new include
#' w <- new_include(name = "Protected areas", variable = v)
#'
#' # print object
#' print(w)
#'
#' @export
new_include <- function(
  name, variable, mandatory = FALSE,
  initial_visible = TRUE, initial_status = TRUE,
  id = uuid::UUIDgenerate()) {
  Include$new(
    id = id,
    name = name,
    variable = variable,
    mandatory = mandatory,
    initial_visible = initial_visible,
    initial_status = initial_status)
}
