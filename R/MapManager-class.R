#' @include internal.R Weight-class.R SingleTheme-class.R MultiTheme-class.R
NULL

#' Map manager class
#'
#'
#' Definition for the MapManager class.
MapManager <- R6::R6Class(
  "MapManager",
  public = list(

    #' @field layers `list` of [Theme] or [Weight] objects.
    layers = list(),

    #' @field ids `character` vector of identifiers for the layers.
    ids = NULL,

    #' @field order `numeric` vector indicating the order each layer
    #'  should appear on them map. A value of 1 indicates that a layer should
    #'  appear beneath every other layer.
    order = NA_real_,

    #' @description
    #' Create a MapManager object.
    #' @param layers `list` of Layer objects.
    #' @param order `numeric` vector.
    #' @return A new MapManager object.
    initialize = function(layers, order) {
      assertthat::assert_that(
        is.list(layers),
        all_list_elements_inherit(layers, c("Theme", "Weight")),
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
          message("    " , gsub(nl(), paste0(nl(), "    "), x, fixed = TRUE))
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
        assertthat::noNA(value))
      assertthat::assert_that(
        value %in% self$ids,
        msg = paste0("no layer with the id `", value,"`"))
      self$layers[[which(self$ids == value)]]
    },

    #' @description
    #' Get a parameter for the object.
    #' @param value `list` with parameter information (see Details section)
    #' @details
    #' The argument to `value` should be a `list` with the following elements:
    #' \describe{
    #' \item{id}{`character` (optional) name of layer.}
    #' \item{parameter}{`character` name of parameter.
    #'   Available options are: `"order"`, `"feature_order"` and `"visible"`.
    #'   Note that the `"id"` element is required for `"feature_order"`
    #'   and `"visible"` parameters.}
    #' }
    get_parameter = function(value) {
      assertthat::assert_that(
        is.list(value),
        assertthat::has_name(value, "parameter"),
        assertthat::is.string(value$parameter))
      if (is.null(value$id)) {
        # map manager parameters
        if (identical(value$parameter, "order")) {
          return(self$get_order())
        } else {
          stop("unknown parameter.")
        }
      } else {
        # layer parameters
        assertthat::assert_that(
          assertthat::has_name(value, "id"),
          assertthat::is.string(value$id),
          value$id %in% self$ids)
          return(self$get_layer(value$id)$get_parameter(value$parameter))
      }
    },

    #' @description
    #' Set information on the plot order of each layer.
    #' @param value `logical` vector indicating if each layer is visible or not.
    set_order = function(value) {
      if (is.list(value))
        value <- unlist(value, recursive = TRUE, use.names = TRUE)
      assertthat::assert_that(
        is.numeric(value),
        assertthat::noNA(value),
        length(value) == length(self$layers))
      self$order <- value
      invisible(self)
    },

    #' @description
    #' Set a parameter for the object.
    #' @param value `list` with new parameter information (see Details section)
    #' @details
    #' \describe{
    #' \item{id}{`character` (optional) name of layer.}
    #' \item{parameter}{`character` name of parameter.
    #'   Available options are: `"order"`, `"visible"`, `"feature_order"`,
    #'   or `"feature_visible"`.
    #'   Note that the `"id"` element is required for
    #'   `"visible"`, `"feature_order"`, `"feature_visible"` parameters.}
    #' \item{value}{`numeric` or `logical` value for new parameter.}
    #' }
    set_parameter = function(value) {
      assertthat::assert_that(
        is.list(value),
        assertthat::has_name(value, "parameter"),
        assertthat::is.string(value$parameter))
      if (is.null(value$id)) {
        # map manager parameters
        if (identical(value$parameter, "order")) {
          self$set_order(value$value)
        } else {
          stop("unknown parameter.")
        }
      } else {
        # layer parameters
        assertthat::assert_that(
          assertthat::has_name(value, "id"),
          assertthat::is.string(value$id),
          value$id %in% self$ids)
          self$get_layer(value$id)$set_parameter(value$parameter, value$value)
      }
      invisible(self)
    },

    #' @description
    #' Add a new layer.
    #' @param value `Layer` object.
    add_layer = function(value) {
      assertthat::assert_that(inherits(value, c("Weight", "Theme")))
      assertthat::assert_that(
        !value$id %in% self$ids,
        msg = paste0(
          "cannot add new layer because the id `", value$id,
          "` already exists"))
        self$layers[[length(self$layers) + 1]] <- value
        self$ids <- c(self$ids, value$id)
        self$order <- c(self$order, max(self$order + 1))
        invisible(self)
    },

    #' @description
    #' Remove a layer.
    #' @param value `character` identifier of the layer to remove.
    drop_layer = function(value) {
      assertthat::assert_that(
        assertthat::is.string(value),
        assertthat::noNA(value))
      assertthat::assert_that(
        value$id %in% self$ids,
        msg = paste0(
          "cannot drop layer because the id `", value$id,
          "` doesn't exist"))
        idx <- which(self$ids != value)
        self$layers <- self$layers[idx]
        self$order <- self$order[idx]
        self$order <- rank(self$order)
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
    #' Update map.
    #' @param map [leaflet::leafletProxy()] object.
    update_map = function(map) {
      # TODO
      stop()
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
#' f <- system.file("extdata", "sim_raster_data.tif", package = "locationmisc")
#' d <- new_dataset(f)
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
#'   initial_factor = 90, initial_status = FALSE, id = "W1")
#'
#' # create features using variables
#' f1 <- new_feature(
#'   name = "Possum", variable = v2,
#'   initial_goal = 0.2, initial_status = FALSE, current = 0.5, id = "F1")
#' f2 <- new_feature(
#'   name = "Forests", variable = v3,
#'   initial_goal = 0.3, initial_status = FALSE, current = 0.9, id = "F2")
#' f3 <- new_feature(
#'   name = "Shrubs", variable = v4,
#'   initial_goal = 0.6, initial_status = TRUE, current = 0.4, id = "F3")
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
#'
#' @export
new_map_manager <- function(
  layers, order = rev(seq_along(layers))) {
  MapManager$new(layers = layers, order = order)
}
