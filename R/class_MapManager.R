#' @include internal.R class_Weight.R class_Theme.R class_Solution.R
NULL

#' Map manager class
#'
#'
#' Definition for the `MapManager` class.
MapManager <- R6::R6Class(
  "MapManager",
  public = list(

    #' @field layers `list` of [Theme], [Weight], [Include], [Exclude], [Solution] objects.
    layers = list(),

    #' @field ids `character` vector of identifiers for the layers.
    ids = NULL,

    #' @field order `numeric` vector indicating the order each layer
    #'  should appear on them map. A value of 1 indicates that a layer should
    #'  appear beneath every other layer.
    order = NA_real_,

    #' @description
    #' Create a `MapManager` object.
    #' @param layers `list` of [Theme], [Weight], [Include], [Exclude], [Solution]
    #'  objects.
    #' @param order `numeric` vector.
    #' @return A new `MapManager` object.
    initialize = function(layers, order) {
      assertthat::assert_that(
        is.list(layers),
        all_list_elements_inherit(
          layers, c("Include", "Exclude", "Weight", "Theme", "Solution")
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
    #' @param download_only get only layer names that are flagged as downloadable.
    #' Available options are `TRUE` or `FALSE`. 
    #' @return `character` vector.
    get_layer_names = function(download_only) {
      if (download_only) {
        unlist(
          lapply(self$layers, function(x) {
            if (inherits(x, "Theme")) {
              lapply(x$feature, function(y) {
                # get layer name if downloadable is TRUE; needed for export list
                if (y$get_downloadable()) {
                  y$get_layer_name()
                } 
              })
            } else {
              # get layer name if downloadable is TRUE; needed for export list
              if (x$get_downloadable()) {
                x$get_layer_name()
              } 
            }
          }),
          recursive = TRUE, use.names = FALSE
        )
      } else {
        # get all layer names; needed for mapping
        unlist(
          lapply(self$layers, function(x) x$get_layer_name()),
          recursive = TRUE, use.names = FALSE
        )
      }
    },
    
    #' @description
    #' Get layer index values.
    #' @param download_only get only layer indices that are flagged as downloadable.
    #' Available options are `TRUE` or `FALSE`.  
    #' @return `character` vector.
    get_layer_indices = function(download_only) {
      if (download_only) {
        unlist(
          lapply(self$layers, function(x) {
            if (inherits(x, "Theme")) {
              lapply(x$feature, function(y) {
                # get layer index if downloadable is TRUE; needed for export list
                if (y$get_downloadable()) {
                  y$get_layer_index()
                } 
              })
            } else {
              # get layer index if downloadable is TRUE; needed for export list
              if (x$get_downloadable()) {
                x$get_layer_index()
              } 
            }
          }),
          recursive = TRUE, use.names = FALSE
        )
      } else {
        # get all layer indices; needed for mapping
        unlist(
          lapply(self$layers, function(x) x$get_layer_index()),
          recursive = TRUE, use.names = FALSE
        )
      }
    },
    
    #' @description
    #' Get layer ids values.
    #' @return `character` vector.
    get_layer_ids = function() {
      unlist(
        lapply(seq_along(self$layers), function(i) {
          if (inherits(self$layers[[i]], "Theme")) {
            self$layers[[i]]$get_feature_id()
          } else {
            self$layers[[i]]$get_id()
          }
        }),
        recursive = TRUE, use.names = FALSE
      )
    },
    
    #' @description
    #' Get panes.
    #' @return `character` vector.
    get_layer_panes = function() {
      unlist(
        lapply(self$layers, function(x) x$get_layer_pane()),
        recursive = TRUE, use.names = FALSE
      )
    },     
    
    #' @description
    #' Get layer visible values.
    #' @return `character` vector.
    get_layer_visible = function() {
      unlist(
        lapply(seq_along(self$layers), function(i) {
          if (inherits(self$layers[[i]], "Theme")) {
            self$layers[[i]]$get_feature_visible()
          } else {
            self$layers[[i]]$get_visible()
          }
        }),
        recursive = TRUE, use.names = FALSE
      )
    },
    
    #' @description
    #' Get layer invisible values.
    #' @return `date/time` vector.
    get_layer_invisible = function() {
      unlist(
        lapply(seq_along(self$layers), function(i) {
          if (inherits(self$layers[[i]], "Theme")) {
            self$layers[[i]]$get_feature_invisible()
          } else {
            self$layers[[i]]$get_invisible()
          }
        }),
        recursive = TRUE, use.names = FALSE
      )
    },
    
    #' @description
    #' Get layer loaded values.
    #' @return `logical` vector.
    get_layer_loaded = function() {
      unlist(
        lapply(seq_along(self$layers), function(i) {
          if (inherits(self$layers[[i]], "Theme")) {
            self$layers[[i]]$get_feature_loaded()
          } else {
            self$layers[[i]]$get_loaded()
          }
        }),
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
    #' Set loaded for all layers.
    #' @param value `logical` vector indicating if layers are loaded or
    #' not.
    set_loaded = function(value) {
      assertthat::assert_that(
        assertthat::is.flag(value),
        assertthat::noNA(value)
      )
      vapply(self$layers, FUN.VALUE = logical(1), function(x) {
        x$set_loaded(value)
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
        zindex = 100 + (last(self$order) * 100)
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
      # get map pane
      pane <- self$get_lazyload() %>% 
        dplyr::filter(ids == value) %>% 
        dplyr::pull(panes)
      # remove layer
      leaflet::clearGroup(map, pane)
      # delete pane
      removeMapPane(pane)
      
      # drop layer from object
      idx <- which(self$ids != value)
      self$layers <- self$layers[idx]
      self$ids <- self$ids[idx]
      self$order <- self$order[idx]
      self$order <- rank(self$order)

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
    #' Get group layer ids.
    #' @return `character` vector.
    get_group_layer_ids = function() {
      unlist(
        lapply(seq_along(self$layers), function(i) {
          if (inherits(self$layers[[i]], "Theme")) {
            rep(self$layers[[i]]$id, length(self$layers[[i]]$feature))
          } else {
            self$layers[[i]]$id
          }
        }),
        recursive = TRUE, use.names = FALSE
      )
    },
    
    #' @description
    #' Get group layer ids.
    #' @return `character` vector.
    get_layer_classes = function() {
      unlist(
        lapply(seq_along(self$layers), function(i) {
          if (inherits(self$layers[[i]], "Theme")) {
            rep("Feature", length(self$layers[[i]]$feature))
          } else {
            class(self$layers[[i]])[1]
          }
        }),
        recursive = TRUE, use.names = FALSE
      )
    },     
    
    #' @description
    #' Get data frame of map manager layers.
    #' @return `data.frame` object.
    get_lazyload = function() {
      tibble::tibble(
        classes = self$get_layer_classes(),
        group_ids = self$get_group_layer_ids(), 
        ids = self$get_layer_ids(),
        panes = self$get_layer_panes(),
        names = self$get_layer_names(download_only = FALSE),
        indices = self$get_layer_indices(download_only = FALSE),
        visible = self$get_layer_visible(),
        invisible = self$get_layer_invisible(),
        loaded = self$get_layer_loaded()
      )
    },    

    #' @description
    #' Initial map by adding data to it.
    #' @param map [leaflet::leafletProxy] object.
    initialize_map = function(map) {
      # compute zIndex values for layers
      zv <- 100 + (self$order * 100)
      # add layers
      for (i in seq_along(self$layers)) {
        if (inherits(self$layers[[i]], "Theme")) {
          ## render logic is in Theme class
          map <- self$layers[[i]]$render_on_map(map, zindex = zv[i])
        } else {
          ## only render layers that are visible on init
          if (self$layers[[i]]$visible) {
            map <- self$layers[[i]]$render_on_map(map, zindex = zv[i])
          } 
        }
      }
      # return result
      invisible(map)
    },

    #' @description
    #' Update map.
    #' @param map [leaflet::leafletProxy()] object.
    update_map = function(map) {
      # compute zIndex values
      zv <- 100 + (self$order * 100)
      # update layers
      for (i in seq_along(self$layers)) {
        # if layer is a Theme ...
        if (inherits(self$layers[[i]], "Theme")) {
          ## render vs. update logic is in Theme class ...
          self$layers[[i]]$update_on_map(map, zindex = zv[i])
        } else {
          # Weight, Include, Excludes and Solutions ...
          idx <- self$layers[[i]]$variable$index # layer index
          v <- self$layers[[i]]$visible # visible
          l <- self$layers[[i]]$loaded # loaded
          iv <- self$layers[[i]]$invisible # invisible
          h <- self$layers[[i]]$hidden # hidden
          if (!h && v && !l) {
            # visible + not loaded + not hidden:
            self$layers[[i]]$set_new_pane(uuid::UUIDgenerate(), idx) # new pane
            self$layers[[i]]$render_on_map(map, zindex = zv[i]) # render pane
            self$layers[[i]]$set_loaded(TRUE) # set loaded to TRUE
          } else if (!v && l && identical(iv, NA_real_)) {
            # loaded + first time invisible
            self$layers[[i]]$update_on_map(map, zindex = zv[i]) # update pane
            self$layers[[i]]$set_invisible(as.numeric(Sys.time())) # time stamp
          } else if (l) {
            ## only update loaded layers (loaded + visible) or (loaded + invisible)
            self$layers[[i]]$update_on_map(map, zindex = zv[i]) # update pane
          }
        }
      }
      invisible(self)
    },
    
    #' @description
    #' Delete single map pane
    #' @param map [leaflet::leafletProxy()] object.
    delete_sinlge_map_pane = function(map) {
      ## invisible layers with a numeric Sys.time && visible == false
      loaded_invisible <- self$get_lazyload() %>%
        dplyr::filter(visible == FALSE) %>%
        dplyr::filter(!is.na(invisible))
      ## set a 4 layer invisible threshold hold
      if (nrow(loaded_invisible) > 3) {
        ### filter for oldest invisible layer to clear
        clear_layer <- loaded_invisible %>%
          dplyr::filter(invisible == min(loaded_invisible$invisible))
        ## clear group
        leaflet::clearGroup(map, clear_layer$panes)
        ### delete pane from map
        removeMapPane(clear_layer$panes)
        ### if layer is a Feature ...
        if (identical(clear_layer$classes, "Feature")) {
          tid <- clear_layer$group_ids # extract theme id
          fid <- clear_layer$ids # extract feature id
          #### within the theme, find Feature id index
          idx <- which(self$get_layer(tid)$get_feature_id() == fid)
          #### update loaded and visible status 
          self$get_layer(tid)$feature[[idx]]$set_loaded(FALSE)
          self$get_layer(tid)$feature[[idx]]$set_invisible(NA_real_)
        } else {
          #### Weight, Include, Exclude and Solution:
          id <- clear_layer$ids # extract id
          ##### update loaded and visible status 
          self$get_layer(id)$set_loaded(FALSE)
          self$get_layer(id)$set_invisible(NA_real_)
        }
      }
      invisible(self)
    },
    
    #' @description
    #' Delete all map panes.
    #' @param map [leaflet::leafletProxy()] object.
    delete_all_map_panes = function(map) {
      loaded <- self$get_lazyload() %>%
        dplyr::filter(loaded == TRUE)
      if (nrow(loaded) > 0) {
        for (i in 1:nrow(loaded)) {
          ## clear group
          leaflet::clearGroup(map, loaded[i,]$panes)
          ### remove pane from map
          removeMapPane(loaded[i,]$panes) 
          ### if layer is a Feature ...
          if (identical(loaded[i,]$classes, "Feature")) {
            tid <- loaded[i,]$group_ids # extract theme id
            fid <- loaded[i,]$ids # extract feature id
            #### within the theme, find Feature id index
            idx <- which(self$get_layer(tid)$get_feature_id() == fid)
            #### update loaded and visible status 
            self$get_layer(tid)$feature[[idx]]$set_loaded(FALSE)
            self$get_layer(tid)$feature[[idx]]$set_invisible(NA_real_)
          } else {
            #### Weight, Include, Exclude and Solution:
            id <- loaded[i,]$ids # extract id
            ##### update loaded and visible status 
            self$get_layer(id)$set_loaded(FALSE)
            self$get_layer(id)$set_invisible(NA_real_)
          }        
        }
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
#'   factor = -90, status = FALSE, id = "W1"
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
#' # create a map manager for the themes and weight
#' mm <- new_map_manager(layers = list(t1, t2, w))
#'
#' # print object
#' print(mm)
#' @export
new_map_manager <- function(layers, order = rev(seq_along(layers))) {
  MapManager$new(layers = layers, order = order)
}
