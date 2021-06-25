#' @include internal.R Weight-class.R Theme-class.R Solution-class.R
NULL

#' Map manager class
#'
#'
#' Definition for the MapManager class.
MapManager <- R6::R6Class(
  "MapManager",
  public = list(

    #' @field layers `list` of [Theme], [Weight], [Solution] objects.
    layers = list(),

    #' @field ids `character` vector of identifiers for the layers.
    ids = NULL,

    #' @field order `numeric` vector indicating the order each layer
    #'  should appear on them map. A value of 1 indicates that a layer should
    #'  appear beneath every other layer.
    order = NA_real_,

    #' @description
    #' Create a MapManager object.
    #' @param layers `list` of [Theme], [Weight], [Solution] objects.
    #' @param order `numeric` vector.
    #' @return A new MapManager object.
    initialize = function(layers, order) {
      assertthat::assert_that(
        is.list(layers),
        all_list_elements_inherit(
          layers, c("Theme", "Weight", "Solution", "Include")),
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
    #' \item{id}{`character` (optional) identifier for layer.}
    #' \item{parameter}{`character` name of parameter.
    #'   Available options are:
    #'   `"order"`, "remove"`,
    #'   `"visible"`, `"feature_order"`, `"feature_visible"`.
    #'   Note that the `"id"` element is required for
    #'   `"remove"`, `"visible"`, `"feature_order"`, `"feature_visible"`
    #'    parameters.}
    #' \item{value}{`numeric` or `logical` value for new parameter.}
    #' }
    set_parameter = function(value) {
      assertthat::assert_that(
        is.list(value),
        assertthat::has_name(value, "parameter"),
        assertthat::is.string(value$parameter))
      if (identical(value$parameter, "remove")) {
        # remove layer from map manager
        self$drop_layer(value$id)
      } else if (is.null(value$id)) {
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
      assertthat::assert_that(inherits(value, c("Weight", "Theme", "Solution")))
      assertthat::assert_that(
        !value$id %in% self$ids,
        msg = paste0(
          "cannot add new layer because the id `", value$id,
          "` already exists"))
        self$layers[[length(self$layers) + 1]] <- value
        self$ids <- c(self$ids, value$id)
        self$order <- c(self$order, max(self$order) + 1)
        invisible(self)
    },

    #' @description
    #' Remove a layer.
    #' @param value `character` layer identifier.
    drop_layer = function(value) {
      assertthat::assert_that(
        assertthat::is.string(value),
        assertthat::noNA(value))
      assertthat::assert_that(
        value %in% self$ids,
        msg = paste0(
          "cannot drop layer because the id `", id,
          "` doesn't exist"))
      idx <- which(self$ids != value)
      self$layers <- self$layers[idx]
      self$ids <- self$ids[idx]
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
    #' Get initial map.
    #' @param dataset `Dataset` object.
    #' @return [leaflet::leaflet()] object.
    get_initial_map = function(dataset) {
      # get spatial extent for dataset
      ## extract extent
      ext <- methods::as(raster::extent(
        dataset$get_spatial_data()), "SpatialPolygons")
      ## prepare bounding box
      ext <- sf::st_set_crs(sf::st_as_sf(ext), dataset$get_crs())
      ## convert to WGS1984
      ext <- raster::extent(sf::st_transform(ext, 4326))
      ## create bounding box by expanding viewport by 10%
      bb <- list()
      bb$xmin <- unname(ext@xmin - (0.1 * (ext@xmax - ext@xmin)))
      bb$xmax <- unname(ext@xmax + (0.1 * (ext@xmax - ext@xmin)))
      bb$ymin <- unname(ext@ymin - (0.1 * (ext@ymax - ext@ymin)))
      bb$ymax <- unname(ext@ymax + (0.1 * (ext@ymax - ext@ymin)))
      bb$xmin <- max(bb$xmin, -180)
      bb$xmax <- min(bb$xmax, 180)
      bb$ymin <- max(bb$ymin, -90)
      bb$ymax <- min(bb$ymax, 90)
      # prepare JS code for button
      fly_to_sites_js <- paste0(
        "function(btn, map){ map.flyToBounds([",
        "[", bb$ymin, ", ", bb$xmin, "],",
        "[", bb$ymax, ", ", bb$xmax, "]]);}")
      zoom_in_js <- paste0(
        "function(btn, map){",
        "map.setZoom(Math.min(map.getZoom() + 1, map.getMaxZoom()));",
        "}")
      zoom_out_js <- paste0(
        "function(btn, map){",
        "map.setZoom(Math.max(map.getZoom() - 1, map.getMinZoom()));",
        "}")
      # initialize map
      map <-
        leaflet::leaflet() %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery) %>%
        leaflet::flyToBounds(
          bb$xmin, bb$ymin, bb$xmax, bb$ymax) %>%
        leaflet::addEasyButton(
          leaflet::easyButton(
            icon = shiny::icon("plus"),
            title = "Zoom in",
            position = "topright",
            onClick = htmlwidgets::JS(zoom_in_js))) %>%
        leaflet::addEasyButton(
          leaflet::easyButton(
            icon = shiny::icon("minus"),
            title = "Zoom out",
            position = "topright",
            onClick = htmlwidgets::JS(zoom_out_js))) %>%
        leaflet::addEasyButton(
          leaflet::easyButton(
            icon = shiny::icon("home"),
            title = "Zoom to data",
            position = "topright",
            onClick = htmlwidgets::JS(fly_to_sites_js))) %>%
        leaflet::addScaleBar(
          position = "bottomright") %>%
        leaflet::addMiniMap(position = "bottomright")
      # compute zIndex values for layers
      zv <- self$order * 100
      # add layers
      for (i in seq_along(self$layers)) {
        map <- self$layers[[i]]$render_on_map(map, zindex = zv[i])
      }
      # return result
      map
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
#'   "extdata", "sim_raster_spatial.tif", package = "locationmisc")
#' f2 <- system.file(
#'  "extdata", "sim_raster_attribute.csv.gz", package = "locationmisc")
#' f3 <- system.file(
#'  "extdata", "sim_raster_boundary.csv.gz", package = "locationmisc")
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
