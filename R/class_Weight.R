#' @include internal.R class_Variable.R
NULL

#' Weight class
#'
#' Definition for the Weight class.
#'
#' @seealso [new_weight()].
#'
#' @export
Weight <- R6::R6Class(
  "Weight",
  public = list(
    #' @field id `character` value.
    id = NA_character_,

    #' @field name `character` value.
    name = NA_character_,

    #' @field variable [Variable] object.
    variable = NULL,
    
    #' @field pane `character` name.
    pane = NA_character_,        

    #' @field visible `logical` value.
    visible = NA,
    
    #' @field invisible `numeric` date/time.
    invisible = NA_real_,
    
    #' @field loaded `logical` value.
    loaded = NA,    

    #' @field hidden `logical` value.
    hidden = NA,

    #' @field status `logical` value.
    status = NA,

    #' @field current `numeric` value.
    current = NA_real_,

    #' @field factor `numeric` value.
    factor = NA_real_,

    #' @field min_factor `numeric` minimum factor value.
    min_factor = NA_real_,

    #' @field max_factor `numeric` maximum factor value.
    max_factor = NA_real_,

    #' @field step_factor `numeric` step factor value.
    step_factor = NA_real_,

    #' @description
    #' Create a new Weight object.
    #' @param id `character` value.
    #' @param name `character` value.
    #' @param variable [Variable] object.
    #' @param pane `character` value.
    #' @param visible `logical` value.
    #' @param invisible `numeric` date/time value.
    #' @param loaded `logical` value.
    #' @param hidden `logical` value.
    #' @param status `logical` value.
    #' @param current `logical` value.
    #' @param factor `numeric` initial factor value.
    #' @param min_factor `numeric` minimum factor value.
    #' @param max_factor `numeric` maximum factor value.
    #' @param step_factor `numeric` step factor value.
    #' @return A new Weight object.
    ## constructor
    initialize = function(id, name, variable, pane, visible, invisible, loaded, hidden, 
                          status, current, factor, min_factor, max_factor, 
                          step_factor) {
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
        #### pane
        assertthat::is.string(pane),
        assertthat::noNA(pane),
        #### visible
        assertthat::is.flag(visible),
        assertthat::noNA(visible),
        #### invisible
        inherits(invisible, "numeric"),
        #### loaded
        assertthat::is.flag(loaded),
        assertthat::noNA(loaded),        
        #### hidden
        assertthat::is.flag(hidden),
        assertthat::noNA(hidden),
        #### status
        assertthat::is.flag(status),
        assertthat::noNA(status),
        #### current
        assertthat::is.number(current),
        assertthat::noNA(current),
        #### factor
        assertthat::is.number(factor),
        assertthat::noNA(factor),
        factor >= min_factor,
        factor <= max_factor,
        #### min_factor
        assertthat::is.number(min_factor),
        assertthat::noNA(min_factor),
        min_factor <= max_factor,
        #### max_factor
        assertthat::is.number(max_factor),
        assertthat::noNA(max_factor),
        max_factor >= min_factor,
        #### step_factor
        assertthat::is.number(step_factor),
        assertthat::noNA(step_factor),
        step_factor <= max_factor
      )
      ### set fields
      self$id <- enc2ascii(id)
      self$variable <- enc2ascii(variable)
      self$pane <- enc2ascii(pane)
      self$name <- name
      self$status <- status
      self$current <- current
      self$visible <- visible && !hidden
      self$invisible <- invisible
      self$loaded <- visible # if layer is visible on init, load it
      self$hidden <- hidden
      self$factor <- factor
      self$min_factor <- min_factor
      self$max_factor <- max_factor
      self$step_factor <- step_factor
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Weight")
      message("  id:       ", self$id)
      message("  name:     ", self$name)
      message("  variable: ", self$variable$repr())
      message("  pane:  ", self$pane)
      message("  current:  ", round(self$current, 2))
      message("  visible:  ", self$visible)
      message("  invisible:  ", self$invisible)
      message("  loaded:  ", self$loaded)
      message("  hidden:  ", self$hidden)
      message("  status:   ", self$status)
      message("  factor:   ", round(self$factor, 2))
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
        ", current: ", round(self$current, 2),
        ", factor: ", round(self$factor, 2), end, nl(),
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
    #' Get layer pane class.
    #' @return `character` vector.
    get_layer_pane = function() {
      self$pane
    },
    
    #' @description
    #' Get weight identifier.
    #' @return `character` vector.
    get_id = function() {
      self$id
    },      

    #' @description
    #' Get visible.
    #' @return `logical` value.
    get_visible = function() {
      self$visible
    },
    
    #' @description
    #' Get invisible.
    #' @return `numeric` date/time value.
    get_invisible = function() {
      self$invisible
    }, 
    
    #' @description
    #' Get loaded.
    #' @return `logical` value.
    get_loaded = function() {
      self$loaded
    },    

    #' @description
    #' Get hidden.
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
    #' Get current.
    #' @return `logical` value.
    get_current = function() {
      self$current
    },

    #' @description
    #' Get factor.
    #' @return `numeric` value.
    get_factor = function() {
      self$factor
    },

    #' @description
    #' Get the data.
    #' @return [sf::st_as_sf()] or [terra::rast()] object.
    get_data = function() {
      self$variable$get_data()
    },
    
    #' @description
    #' Set new pane.
    #' @param id `character` unique identifier.
    #' @param index `character` variable index.
    #' @return `character` value.
    set_new_pane = function(id, index) {
      self$pane <- enc2ascii(paste(id, index, sep = "-"))
    }, 

    #' @description
    #' Get setting.
    #' @param name `character` setting name.
    #' Available options are `"status"` `"factor"`, `"current"`, or `"visible"`.
    #' @return Value.
    get_setting = function(name) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c("status", "factor", "visible", "current")
      )
      if (identical(name, "status")) {
        out <- self$get_status()
      } else if (identical(name, "factor")) {
        out <- self$get_factor()
      } else if (identical(name, "current")) {
        out <- self$get_current()
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
    #' Set invisible.
    #' @param value `numeric` date/time value.
    set_invisible = function(value) {
      assertthat::assert_that(
        inherits(value, "numeric")
      )
      self$invisible <- value
      if (self$hidden) {
        self$invisible <- NA_real_
      }
      invisible(self)
    },
    
    #' @description
    #' Set loaded.
    #' @param value `logical` new value.
    set_loaded = function(value) {
      assertthat::assert_that(
        assertthat::is.flag(value),
        assertthat::noNA(value)
      )
      self$loaded <- value
      if (self$hidden) {
        self$loaded <- FALSE
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
    #' Set factor.
    #' @param value `numeric` new value.
    set_factor = function(value) {
      assertthat::assert_that(
        assertthat::is.number(value),
        assertthat::noNA(value),
        value >= self$min_factor,
        value <= self$max_factor
      )
      self$factor <- value
      invisible(self)
    },

    #' @description
    #' Set current.
    #' @param value `numeric` new value.
    set_current = function(value) {
      assertthat::assert_that(
        assertthat::is.number(value),
        assertthat::noNA(value)
      )
      self$current <- value
      invisible(self)
    },

    #' @description
    #' Set setting.
    #' @param name `character` setting name.
    #' Available options are `"status"` `"factor"`, `"current"`,
    #' or `"visible"``.
    #' @param value `ANY` new value.
    set_setting = function(name, value) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c("status", "factor", "current", "visible")
      )
      if (identical(name, "status")) {
        self$set_status(value)
      } else if (identical(name, "factor")) {
        self$set_factor(value)
      } else if (identical(name, "current")) {
        self$set_current(value)
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
        factor = self$factor,
        min_factor = self$min_factor,
        max_factor = self$max_factor,
        step_factor = self$step_factor,
        provenance = self$variable$provenance$get_widget_data()
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
        hidden = self$hidden,
        legend = self$variable$legend$get_widget_data(),
        units = self$variable$units,
        provenance = self$variable$provenance$get_widget_data(),
        type = "weight"
      )
    },

    #' @description
    #' Export settings.
    #' @return `list` object.
    export = function() {
      list(
        name = enc2ascii(self$name),
        variable = self$variable$export(),
        status = self$status,
        visible = self$visible,
        hidden = self$hidden,
        factor = self$factor
      )
    },

    #' @description
    #' Render on map.
    #' @param x [leaflet::leaflet()] object.
    #' @param zindex `numeric` z-index for ordering.
    #' @return [leaflet::leaflet()] object.
    render_on_map = function(x, zindex) {
      if (self$hidden) return(x) # don't render on map if hidden
      self$variable$render(x, self$pane, zindex, self$visible)
    },

    #' @description
    #' Render on map.
    #' @param x [leaflet::leafletProxy()] object.
    #' @param zindex `numeric` z-index for ordering.
    #' @return [leaflet::leafletProxy()] object.
    update_on_map = function(x, zindex) {
      if (self$hidden) return(x) # don't render on map if hidden
      self$variable$update_render(x, self$pane, zindex, self$visible)
    }
  )
)

#' New weight
#'
#' Create a new [Weight] object.
#'
#' @param factor `numeric` initial factor value.
#'   Defaults to 0.
#'
#' @inheritParams new_theme
#' @inheritParams new_feature
#'
#' @return A [Weight] object.
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
#' # create a new weight
#' w <- new_weight(name = "NDVI", variable = v)
#'
#' # print object
#' print(w)
#' @export
new_weight <- function(
    name, 
    variable, 
    visible = TRUE, 
    invisible = NA_real_, 
    loaded = TRUE, 
    hidden = FALSE, 
    status = TRUE,
    current = 0, 
    factor = 0,
    id = uuid::UUIDgenerate(),
    pane = paste(
      uuid::UUIDgenerate(), 
      variable$index, sep = "-"
      )
  ) {
  Weight$new(
    id = id,
    name = name,
    pane = pane,
    variable = variable,
    visible = visible,
    invisible = invisible,
    loaded = loaded,
    hidden = hidden,
    status = status,
    current = current,
    factor = factor,
    min_factor = -100,
    max_factor = 100,
    step_factor = 1
  )
}
