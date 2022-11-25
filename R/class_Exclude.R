#' @include internal.R class_Variable.R
NULL

#' Exclude class
#'
#' Definition for the Exclude class.
#'
#' @seealso [new_exclude()].
#'
#' @export
Exclude <- R6::R6Class(
  "Exclude",
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

    #' @field hidden `logical` value.
    hidden = NA,

    #' @field status `logical` value.
    status = NA,
    
    #' @field overlap `character` vector.
    overlap = NA_character_,    

    #' @description
    #' Create a new Exclude object.
    #' @param id `character` value.
    #' @param name `character` value.
    #' @param variable [Variable] object.
    #' @param mandatory `logical` value.
    #' @param visible `logical` value.
    #' @param hidden `logical` value.
    #' @param status `logical` value.
    #' @param overlap `character` vector.
    #' @return A new Exclude object.
    ## constructor
    initialize = function(id, name, variable, mandatory, visible, hidden,
                          status, overlap) {
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
        #### visible
        assertthat::is.flag(visible),
        assertthat::noNA(visible),
        #### hidden
        assertthat::is.flag(hidden),
        assertthat::noNA(hidden),
        #### status
        assertthat::is.flag(status),
        assertthat::noNA(status),
        #### overlap
        assertthat::is.string(name),
        !assertthat::noNA(overlap) # must be NA        
      )
      ### set fields
      self$id <- enc2ascii(id)
      self$name <- enc2ascii(name)
      self$variable <- variable
      self$status <- status
      self$visible <- visible && !hidden
      self$hidden <- hidden
      self$mandatory <- mandatory
      self$overlap <- overlap
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Exclude")
      message("  id:       ", self$id)
      message("  name:     ", self$name)
      message("  variable: ", self$variable$repr())
      message("  visible:  ", self$visible)
      message("  hidden:  ", self$hidden)
      message("  status:   ", self$status)
      message("  overlap:   ", self$overlap)
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
        "  variable: ", self$variable$repr()
      )
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
    #' Get visible.
    #' @return `logical` value.
    get_hidden = function() {
      self$hidden
    },

    #' @description
    #' Get status.
    #' @return `logical` value.
    get_status = function() {
      self$status
    },
    
    #' @description
    #' Get overlap.
    #' @return `character` vector.
    get_overlap = function() {
      self$overlap
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
        name %in% c("status", "visible")
      )
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
        assertthat::noNA(value)
      )
      self$visible <- value
      if (self$hidden) {
        self$visible <- FALSE
      }
      invisible(self)
    },

    #' @description
    #' Set status.
    #' @param value `logical` new value.
    set_status = function(value) {
      assertthat::assert_that(
        assertthat::is.flag(value),
        assertthat::noNA(value)
      )
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
        name %in% c("status", "visible")
      )
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
    #' Get data for displaying the exclude in a [solutionSettings()] widget.
    #' @return `list` with widget data.
    get_solution_settings_widget_data = function() {
      list(
        id = self$id,
        name = self$name,
        status = self$status,
        mandatory = self$mandatory,
        provenance = self$variable$provenance$get_widget_data(),
        overlap = self$overlap
      )
    },

    #' @description
    #' Get data for displaying the exclude in a [mapManager()] widget.
    #' @return `list` with widget data.
    get_map_manager_widget_data = function() {
      list(
        id = self$id,
        name = self$name,
        visible = self$visible,
        hidden = self$hidden,
        legend = self$variable$legend$get_widget_data(),
        units = self$variable$units,
        provenance = self$variable$provenance$get_widget_data(),
        type = "exclude"
      )
    },

    #' @description
    #' Export settings.
    #' @return `list` object.
    export = function() {
      list(
        name = enc2ascii(self$name),
        variable = self$variable$export(),
        mandatory = self$mandatory,
        status = self$status,
        visible = self$visible,
        hidden = self$hidden,
        overlap = self$overlap
      )
    },

    #' @description
    #' Render on map.
    #' @param x [leaflet::leaflet()] object.
    #' @param zindex `numeric` z-index for ordering.
    #' @return [leaflet::leaflet()] object.
    render_on_map = function(x, zindex) {
      if (self$hidden) return(x) # don't render on map if hidden
      self$variable$render(x, self$id, zindex, self$visible)
    },

    #' @description
    #' Render on map.
    #' @param x [leaflet::leafletProxy()] object.
    #' @param zindex `numeric` z-index for ordering.
    #' @return [leaflet::leafletProxy()] object.
    update_on_map = function(x, zindex) {
      if (self$hidden) return(x) # don't render on map if hidden
      self$variable$update_render(x, self$id, zindex, self$visible)
    }
  )
)

#' New exclude
#'
#' Create a new [Exclude] object.
#'
#' @param mandatory `logical` value indicating if object is mandatory
#'  for generating solutions.
#'
#' @param overlap `character` vector that remains `NA_character_` until set in 
#'  the [SolutionSettings] object. `overlap` defines the include and exclude 
#'  overlap.
#'
#' @inheritParams new_theme
#' @inheritParams new_feature
#'
#' @return A [Exclude] object.
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
#' # create a new exclude
#' e <- new_exclude(name = "Protected areas", variable = v)
#'
#' # print object
#' print(e)
#' @export
new_exclude <- function(name, variable, mandatory = FALSE,
                        visible = TRUE, hidden = FALSE, status = FALSE,
                        overlap = NA_character_, id = uuid::UUIDgenerate()) {
  Exclude$new(
    id = id,
    name = name,
    variable = variable,
    mandatory = mandatory,
    visible = visible,
    hidden = hidden,
    status = status,
    overlap = overlap
  )
}
