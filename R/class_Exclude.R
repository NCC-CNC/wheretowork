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
    
    #' @field pane `character` name.
    pane = NA_character_,    

    #' @field mandatory `logical` value.
    mandatory = FALSE,

    #' @field visible `logical` value.
    visible = NA,
    
    #' @field invisible `numeric` date/time value.
    invisible = NA_real_, 
    
    #' @field loaded `logical` value.
    loaded = NA,    

    #' @field hidden `logical` value.
    hidden = NA,
    
    #' @field download `logical` value.
    download = NA,

    #' @field status `logical` value.
    status = NA,
    
    #' @field overlap `character` vector.
    overlap = NA_character_,    

    #' @description
    #' Create a new Exclude object.
    #' @param id `character` value.
    #' @param name `character` value.
    #' @param variable [Variable] object.
    #' @param pane `character` value.
    #' @param mandatory `logical` value.
    #' @param visible `logical` value.
    #' @param invisible `numeric` date/time value.
    #' @param loaded `logical` value.
    #' @param hidden `logical` value.
    #' @param download `logical` value.
    #' @param status `logical` value.
    #' @param overlap `character` vector.
    #' @return A new Exclude object.
    ## constructor
    initialize = function(id, name, variable, pane, mandatory, visible, invisible, 
                          loaded, hidden, download, status, overlap) {
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
        #### mandatory
        assertthat::is.flag(mandatory),
        assertthat::noNA(mandatory),
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
        #### download
        assertthat::is.flag(download),
        assertthat::noNA(download),
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
      self$pane <- enc2ascii(pane)
      self$status <- status
      self$visible <- visible && !hidden
      self$invisible <- invisible
      self$loaded <- visible # if layer is visible on init, load it
      self$hidden <- hidden
      self$download <- download
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
      message("  pane:  ", self$pane)
      message("  visible:  ", self$visible)
      message("  invisible:  ", self$invisible)
      message("  loaded:  ", self$loaded)
      message("  hidden:  ", self$hidden)
      message("  download:  ", self$download)
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
    #' Get layer pane class.
    #' @return `character` vector.
    get_layer_pane = function() {
      self$pane
    },
    
    #' @description
    #' Get exclude identifier.
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
    #' Get download.
    #' @return `logical` value.
    get_download = function() {
      self$download
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
        download = self$download,
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
new_exclude <- function(
    name, 
    variable, 
    mandatory = FALSE,
    visible = TRUE,
    invisible = NA_real_,
    loaded = TRUE,
    hidden = FALSE,
    download = TRUE,
    status = FALSE,
    overlap = NA_character_, 
    id = uuid::UUIDgenerate(),
    pane = paste(
      uuid::UUIDgenerate(), 
      variable$index, sep = "-"
    )
  ) {
  Exclude$new(
    id = id,
    name = name,
    pane = pane,
    variable = variable,
    mandatory = mandatory,
    visible = visible,
    invisible = invisible,
    loaded = loaded,
    hidden = hidden,
    download = download,
    status = status,
    overlap = overlap
  )
}
