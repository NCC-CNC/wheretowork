#' @include internal.R class_Weight.R class_Theme.R class_Solution.R
NULL

#' Map manager class
#'
#'
#' Definition for the MapManager class.
MapManager <- R6::R6Class(
  "MapManager",
  public = list(

    #' @field layers `list` of [Theme], [Weight], [Include], [Solution] objects.
    layers = list(),

    #' @field ids `character` vector of identifiers for the layers.
    ids = NULL,

    #' @field order `numeric` vector indicating the order each layer
    #'  should appear on them map. A value of 1 indicates that a layer should
    #'  appear beneath every other layer.
    order = NA_real_,

    #' @description
    #' Create a MapManager object.
    #' @param layers `list` of [Theme], [Weight], [Include], [Solution]
    #'  objects.
    #' @param order `numeric` vector.
    #' @return A new MapManager object.
    initialize = function(layers, order) {
      assertthat::assert_that(
        is.list(layers),
        all_list_elements_inherit(
          layers, c("Include", "Weight", "Theme", "Solution")
        ),
        length(order) == length(layers),
        is.numeric(order),
        setequal(order, seq_along(layers))
      )
      self$layers <- layers
      self$ids <- vapply(layers, function(x) x$id, character(1))
      self$order <- order
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("MapManager")
      # print layers
      if (length(self$layers) > 0) {
        po <- order(self$get_order(), decreasing = TRUE)
        message("  layers: ")
        for (x in vapply(self$layers[po], function(x) x$repr(), character(1))) {
          message("    ", gsub(nl(), paste0(nl(), "    "), x, fixed = TRUE))
        }
      } else {
        message("  layers: none")
      }
      invisible(self)
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @return `character` value.
    repr = function() {
      "MapManager object"
    },

    #' @description
    #' Get information on the plot order of each layer.
    get_order = function() {
      self$order
    },

    #' @description
    #' Get a layer.
    #' @param value `character` layer identifier.
    #' @return [Theme] object.
    get_layer = function(value) {
      assertthat::assert_that(
        assertthat::is.string(value),
        assertthat::noNA(value)
      )
      assertthat::assert_that(
        value %in% self$ids,
        msg = paste0("no layer with the id `", value, "`")
      )
      self$layers[[which(self$ids == value)]]
    },

    #' @description
    #' Get layer names.
    #' @return `character` vector.
    get_layer_names = function() {
      unlist(
        lapply(self$layers, function(x) x$get_layer_name()),
        recursive = TRUE, use.names = FALSE
      )
    },

    #' @description
    #' Get layer index values.
    #' @return `character` vector.
    get_layer_indices = function() {
      unlist(
        lapply(self$layers, function(x) x$get_layer_index()),
        recursive = TRUE, use.names = FALSE
      )
    },

    #' @description
    #' Get a setting for the object.
    #' @param value `list` with setting information (see Details section)
    #' @details
    #' The argument to `value` should be a `list` with the following elements:
    #' \describe{
    #' \item{id}{`character` (optional) name of layer.}
    #' \item{setting}{`character` name of setting.
    #'   Available options are: `"order"`, `"feature_order"` and `"visible"`.
    #'   Note that the `"id"` element is required for `"feature_order"`
    #'   and `"visible"` settings.}
    #' }
    get_setting = function(value) {
      assertthat::assert_that(
        is.list(value),
        assertthat::has_name(value, "setting"),
        assertthat::is.string(value$setting)
      )
      if (is.null(value$id)) {
        # map manager settings
        if (identical(value$setting, "order")) {
          return(self$get_order())
        } else {
          stop("unknown setting.")
        }
      } else {
        # layer settings
        assertthat::assert_that(
          assertthat::has_name(value, "id"),
          assertthat::is.string(value$id),
          value$id %in% self$ids
        )
        return(self$get_layer(value$id)$get_setting(value$setting))
      }
    },

    #' @description
    #' Set information on the plot order of each layer.
    #' @param value `logical` vector indicating if each layer is visible or not.
    set_order = function(value) {
      if (is.list(value)) {
        value <- unlist(value, recursive = TRUE, use.names = TRUE)
      }
      assertthat::assert_that(
        is.numeric(value),
        assertthat::noNA(value),
        length(value) == length(self$layers)
      )
      self$order <- value
      invisible(self)
    },

    #' @description
    #' Set visibility for all layers.
    #' @param value `logical` vector indicating if layers should be visible or
    #' not.
    set_visible = function(value) {
      assertthat::assert_that(
        assertthat::is.flag(value),
        assertthat::noNA(value)
      )
      vapply(self$layers, FUN.VALUE = logical(1), function(x) {
        x$set_visible(value)
        TRUE
      })
      invisible(self)
    },

    #' @description
    #' Set a setting for the object.
    #' @param value `list` with new setting information (see Details section)
    #' @details
    #' \describe{
    #' \item{id}{`character` (optional) identifier for layer.}
    #' \item{setting}{`character` name of setting.
    #'   Available options are:
    #'   `"order"`, "remove"`,
    #'   `"visible"`, `"feature_order"`, `"feature_visible"`.
    #'   Note that the `"id"` element is required for
    #'   `"remove"`, `"visible"`, `"feature_order"`, `"feature_visible"`
    #'    settings.}
    #' \item{value}{`numeric` or `logical` value for new setting.}
    #' }
    set_setting = function(value) {
      assertthat::assert_that(
        is.list(value),
        assertthat::has_name(value, "setting"),
        assertthat::is.string(value$setting)
      )
      if (identical(value$setting, "remove")) {
        stop("$drop_layer() must be called directly.")
      } else if (is.null(value$id)) {
        # map manager settings
        if (identical(value$setting, "order")) {
          self$set_order(value$value)
        } else {
          stop("unknown setting.")
        }
      } else {
        # layer settings
        assertthat::assert_that(
          assertthat::has_name(value, "id"),
          assertthat::is.string(value$id),
          value$id %in% self$ids
        )
        self$get_layer(value$id)$set_setting(value$setting, value$value)
      }
      invisible(self)
    },

    #' @description
    #' Add a new layer.
    #' @param value `Layer` object.
    #' @param map [leaflet::leafletProxy()] object.
    add_layer = function(value, map) {
      # assert arguments are valid
      assertthat::assert_that(
        inherits(value, c("Weight", "Theme", "Solution")),
        inherits(map, "leaflet_proxy")
      )
      assertthat::assert_that(
        !value$id %in% self$ids,
        msg = paste0(
          "cannot add new layer because the id `", value$id,
          "` already exists"
        )
      )
      # add layer from object
      self$layers[[length(self$layers) + 1]] <- value
      self$ids <- c(self$ids, value$id)
      self$order <- c(self$order, max(self$order) + 1)
      # update map
      self$layers[[length(self$layers)]]$render_on_map(
        map,
        zindex = last(self$order) * 100
      )
      # return invisible self
      invisible(self)
    },

    #' @description
    #' Remove a layer.
    #' @param value `character` layer identifier.
    #' @param map [leaflet::leafletProxy()] object.
    drop_layer = function(value, map) {
      # assert arguments are valid
      assertthat::assert_that(
        assertthat::is.string(value),
        assertthat::noNA(value),
        inherits(map, "leaflet_proxy")
      )
      assertthat::assert_that(
        value %in% self$ids,
        msg = paste0(
          "cannot drop layer because the id `", value,
          "` doesn't exist"
        )
      )
      # drop layer from object
      idx <- which(self$ids != value)
      self$layers <- self$layers[idx]
      self$ids <- self$ids[idx]
      self$order <- self$order[idx]
      self$order <- rank(self$order)
      # update map
      ## since we can't actually delete panes,
      ## we will move the pane below the map and remove its contents
      leaflet::updateMapPane(map, paste0("pane-", value), -1, FALSE)
      leaflet::clearGroup(map, value)
      # return invisible self
      invisible(self)
    },

    #' @description
    #' Get data for displaying the theme in a [mapManager()] widget.
    #' @return `list` with widget data.
    get_widget_data = function() {
      list(
        ids = self$ids,
        order = self$order,
        layers =
          lapply(self$layers, function(x) x$get_map_manager_widget_data())
      )
    },

    #' @description
    #' Initial map by adding data to it.
    #' @param map [leaflet::leafletProxy] object.
    initialize_map = function(map) {
      # compute zIndex values for layers
      zv <- self$order * 100
      # add layers
      for (i in seq_along(self$layers)) {
        map <- self$layers[[i]]$render_on_map(map, zindex = zv[i])
      }
      # return result
      invisible(map)
    },

    #' @description
    #' Update map.
    #' @param map [leaflet::leafletProxy()] object.
    update_map = function(map) {
      # compute zIndex values
      zv <- self$order * 100
      # update layers
      for (i in seq_along(self$layers)) {
        self$layers[[i]]$update_on_map(map, zindex = zv[i])
      }
      invisible(self)
    }
  )
)

#' New map manager
#'
#' Create a new [MapManager] object.
#'
#' @param layers `list` of [Theme] and/or [Weight] objects.
#'
#' @param order `numeric` vector containing a value for each element in
#'  `layers`. These values indicate the order each layer should
#'  appear on them map. A value of 1 indicates that a layer should appear
#'  beneath every other layer.
#'  Defaults to an integer sequence of numbers based on the number of elements
#'  in `layers`.
#'
#' @return A [MapManager] object.
#'
#' @examples
#' # create dataset
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
#' # create variables
#' v1 <- new_variable_from_auto(dataset = d, index = 1)
#' v2 <- new_variable_from_auto(dataset = d, index = 2)
#' v3 <- new_variable_from_auto(dataset = d, index = 3)
#' v4 <- new_variable_from_auto(dataset = d, index = 4)
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
#' t1 <- new_single_theme("Species", f1, id = "T1")
#' t2 <- new_multi_theme("Ecoregions", list(f2, f3), id = "T2")
#'
#' # create a map manager for the themes and weight
#' mm <- new_map_manager(layers = list(t1, t2, w))
#'
#' # print object
#' print(mm)
#' @export
new_map_manager <- function(layers, order = rev(seq_along(layers))) {
  MapManager$new(layers = layers, order = order)
}
