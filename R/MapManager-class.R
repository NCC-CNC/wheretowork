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

    #' @field visible `logical` vector indicating if each layer should
    #'   be visible or not.
    visible = NULL,

    #' @field order `integer` vector indicating the order each layer
    #'  should appear on them map. A value of 1 indicates that a layer should
    #'  appear beneath every other layer.
    order = NULL,

    #' @field sub_order `list` object indicating the relative order each
    #'  dataset within each layer.
    #'  Each element should contain an `integer`
    #'  vector that contains a value for each dataset within each layer.
    #'  A value of 1 indicates that a layer dataset appear beneath every other
    #'  dataset within a layer.
    sub_order = NULL,

    #' @description
    #' Create a MapManager object.
    #' @param layers `list` of Layer objects.
    #' @param visible `logical` vector.
    #' @param order `numeric` vector.
    #' @return A new MapManager object.
    initialize = function(layers, visible, order) {
      assertthat::assert_that(
        is.list(layers),
        all_list_elements_inherit(layers, c("Theme", "Weight")),
        length(order) == length(layers),
        is.numeric(order),
        setequal(order, seq_along(layers)),
        length(visible) == length(layers),
        is.logical(visible),
        assertthat::noNA(visible)
      )
      self$layers <- layers
      self$ids <- vapply(layers, function(x) x$id, character(1))
      self$visible <- visible
      self$order <- order
      self$sub_order <-
        lapply(layers, function(x) {
          if (inherits(x, "MultiTheme")) {
            return(as.double(rev(seq_along(x$feature))))
          } else {
            return(1)
          }
        })
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("MapManager")
      # print layers
      if (length(self$layers) > 0) {
        message("  layers: ")
        for (x in vapply(self$layers, function(x) x$repr(), character(1))) {
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
    #' Get information on the visibility of each layer.
    get_visible = function() {
      self$visible
    },

    #' @description
    #' Get information on the plot order of each layer.
    get_order = function() {
      self$order
    },

    #' @description
    #' Get information on the plot order of each dataset within each layer.
    get_sub_order = function() {
      self$sub_order
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
    #' @param value `list` with new parameter information (see Details section)
    #' @details
    #' The argument to `value` should be a `list` with the following elements:
    #' \describe{
    #' \item{parameter}{`character` name of parameter.
    #'   Available options are: `"visible"` and `"order"`.}
    #' }
    get_parameter = function(value) {
      assertthat::assert_that(
        is.list(value),
        assertthat::has_name(value, "parameter"),
        assertthat::is.string(value$parameter))
      if (identical(value$parameter, "visible")) {
        return(self$get_visible())
      } else if (identical(value$parameter, "order")) {
        return(self$get_order())
      } else if (identical(value$parameter, "sub_order")) {
        return(self$get_sub_order())
      } else {
        stop("unknown parameter.")
      }
    },

    #' @description
    #' Set information on the visibility of each layer.
    #' @param value `logical` vector indicating if each layer is visible or not.
    set_visible = function(value) {
      assertthat::assert_that(
        is.logical(value),
        assertthat::noNA(value),
        length(value) == length(self$layers))
      self$visible <- value
      invisible(self)
    },

    #' @description
    #' Set information on the plot order of each layer.
    #' @param value `logical` vector indicating if each layer is visible or not.
    set_order = function(value) {
      assertthat::assert_that(
        is.numeric(value),
        assertthat::noNA(value),
        length(value) == length(self$layers))
      self$order <- value
      invisible(self)
    },

    #' @description
    #' Set information on the plot order of each dataset within each layer.
    set_sub_order = function(id, value) {
      # validate arguments
      assertthat::assert_that(
        ## id
        assertthat::is.string(id),
        assertthat::noNA(id),
        id %in% self$ids,
        ## value
        is.numeric(value),
        assertthat::noNA(value),
        anyDuplicated(value) == 0L)
      # find index for layer based on id
      idx <- which(id == self$ids)
      # verify that layer with id exists
      assertthat::assert_that(
        length(idx) == 1,
        assertthat::noNA(idx),
        msg =
          "cannot set sub_order for layer because no such layer exists")
      assertthat::assert_that(
        length(value) == length(self$sub_order[[idx]]),
        msg =
          "incorrect number of values specified for changing layer sub_order")
      self$sub_order[[idx]] <- value
      invisible(self)
    },

    #' @description
    #' Set a parameter for the object.
    #' @param value `list` with new parameter information (see Details section)
    #' @details
    #' The argument to `value` should be a `list` with the following elements:
    #' \describe{
    #' \item{id}{`character` identifier for theme or weight.}
    #' \item{parameter}{`character` name of parameter.
    #'   Available options are: `"visible"` and `"order"`.}
    #' \item{value}{`numeric` or `logical` value for new parameter.}
    #' }
    set_parameter = function(value) {
      assertthat::assert_that(
        is.list(value),
        assertthat::has_name(value, "parameter"),
        assertthat::is.string(value$parameter),
        assertthat::has_name(value, "value"))
      if (identical(value$parameter, "visible")) {
        self$set_visible(value$value)
      } else if (identical(value$parameter, "order")) {
        self$set_order(value$value)
      } else if (identical(value$parameter, "sub_order")) {
        self$set_sub_order(value$id, value$value)
      } else {
        stop("unknown parameter.")
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
        self$visible <- c(self$visible, FALSE)
        self$order <- c(self$order, max(self$order + 1))
        if (inherits(value, "MultiTheme")) {
          sub <- as.double(rev(seq_along(value$feature)))
        } else {
          sub <- 1
        }
        self$sub_order <- append(self$sub_order, list(sub))
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
        self$visible <- self$visible[idx]
        self$order <- self$order[idx]
        self$order <- rank(self$order)
        self$sub_order <- self$sub_order[idx]
        invisible(self)
    },

    #' @description
    #' Get data for displaying the theme in a [mapManager()] widget.
    #' @return `list` with widget data.
    get_widget_data = function() {
      list(
        ids = self$ids,
        visible = self$visible,
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
#' @param visible `logical` vector containing a value for each element in
#'  `layers`. These values indicate if (`TRUE`) each layer should be
#'  visible on the map or (`FALSE`) not.
#'  Defaults to a vector of `TRUE` values for each elements in `layers`.
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
#' # create datasets
#' l1 <- new_dataset(
#'  source = tempfile(), total = 12, units = "ha",
#'  legend = new_continuous_legend(1, 100, c("#000000", "#1b9e77")))
#' l2 <- new_dataset(
#'  source = tempfile(), total = 14, units = "ha",
#'  legend = new_continuous_legend(1, 100, c("#000000", "#d95f02")))
#' l3 <- new_dataset(
#'  source = tempfile(), total = 78, units = "ha",
#'  legend = new_continuous_legend(1, 100, c("#000000", "#7570b3")))
#' l4 <- new_dataset(
#'  source = tempfile(), total = 90, units = "ha",
#'  legend = new_continuous_legend(1, 100, c("#000000", "#e31a1c")))
#'
#' # create a weight using a dataset
#' w <- new_weight(name = "Human Footprint Index", dataset = l1)
#'
#' # create features using datasets
#' f1 <- new_feature(name = "Possum Occurrence", dataset = l2)
#' f2 <- new_feature(name = "Forests", dataset = l3)
#' f3 <- new_feature(name = "Shrublands", dataset = l4)
#'
#' # create themes using the features
#' t1 <- new_single_theme(name = "Species", f1)
#' t2 <- new_multi_theme(name = "Ecoregions", list(f1, f2))
#'
#' # create a map manager for the themes and weight
#' mm <- new_map_manager(layers = list(t1, t2, w))
#'
#' # print object
#' print(mm)
#'
#' @export
new_map_manager <- function(
  layers, visible = rep(TRUE, length(layers)), order = seq_along(layers)) {
  MapManager$new(layers = layers, visible = visible, order = order)
}
