#' @include internal.R class_Dataset.R
NULL

#' Variable class
#'
#' Definition for the Variable class.
#'
#' @seealso [new_variable()].
Variable <- R6::R6Class(
  "Variable",
  public = list(

    #' @field dataset `Dataset` object.
    dataset = NULL,

    #' @field index `character` value.
    index = NULL,

    #' @field total `numeric` value.
    total = NA_real_,

    #' @field units `character` value.
    units = NA_character_,

    #' @field legend `Legend` object.
    legend = NULL,

    #' @field provenance [Provenance] object.
    provenance = NULL,

    #' @description
    #' Create a Variable object.
    #' @param dataset `Dataset` value.
    #' @param index `character` or `integer` value.
    #' @param total `numeric` value.
    #' @param units `character` value.
    #' @param legend `Legend` object.
    #' @param provenance [Provenance] object.
    #' @return A new Variable object.
    initialize = function(dataset, index, total, units, legend, provenance) {
      ### assert that arguments are valid
      assertthat::assert_that(
        #### dataset
        inherits(dataset, "Dataset"),
        ## index
        assertthat::is.string(index) || assertthat::is.count(index),
        assertthat::noNA(index),
        #### total
        assertthat::is.number(total),
        assertthat::noNA(total),
        isTRUE(total >= 0),
        #### units
        assertthat::is.string(units),
        assertthat::noNA(units),
        #### legend
        inherits(
          legend,
          c("ContinuousLegend", "CategoricalLegend", "ManualLegend", "NullLegend")
        ),
        #### provenance
        inherits(provenance, "Provenance")
      )
      ### set fields
      self$dataset <- dataset
      self$total <- total
      self$units <- units
      self$legend <- legend
      self$provenance <- provenance
      assertthat::assert_that(dataset$has_index(index))
      if (is.numeric(index)) {
        self$index <- dataset$get_names()[[index]]
      } else {
        self$index <- index
      }
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Variable")
      message("  dataset: ", self$dataset$repr())
      message("  index:   ", self$index)
      message("  total:   ", round(self$total, 2))
      message("  units:   ", self$units)
      message("  provenance:   ", self$provenance$repr())
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
        self$dataset$repr(),
        "#", self$index,
        " ", start, "total: ", round(self$total, 2), " ",
        self$units, end
      )
    },

    #' @description
    #' Verify that the data can be extracted from the dataset.
    #' @return invisible `TRUE` indicating success.
    verify = function() {
      assertthat::assert_that(
        self$dataset$has_index(self$index),
        msg = paste0(
          "dataset does not have variable at index \"", self$index, "\""
        )
      )
      invisible(TRUE)
    },

    #' @description
    #' Get the data.
    #' @return [sf::st_as_sf()] or [raster::raster()] object.
    get_data = function() {
      self$dataset$get_index(self$index)
    },

    #' @description
    #' Export settings
    #' @return `list` object.
    export = function() {
      list(
        index = self$index,
        units = self$units,
        legend = self$legend$export(),
        provenance = self$provenance$export()
      )
    },

    #' @description
    #' Render variable on map.
    #' @param x [leaflet::leaflet()] object.
    #' @param id `character` identifier for map pane.
    #' @param zindex `numeric` z-index for ordering.
    #' @param visible `logical` should the variable be visible?
    #' @return [leaflet::leaflet()] object.
    render = function(x, id, zindex, visible) {
      # assert arguments are valid
      assertthat::assert_that(
        inherits(x, c("leaflet", "leaflet_proxy")),
        assertthat::is.string(id),
        assertthat::is.number(zindex),
        assertthat::is.flag(visible)
      )
      # extract data
      d <- self$get_data()
      # add map pane for variable
      pane_id <- paste0("pane-", id)
      x <- leaflet::addMapPane(x, pane_id, zindex, visible)
      # add data to leaflet map
      if (inherits(d, "Raster")) {
        # Set project on the fly flag
        if (self$dataset$get_crs() == st_crs(3857)) {
          project_on_fly <- FALSE
        } else {
          project_on_fly <- TRUE
        }
        
        ## add raster data
        suppressWarnings({
          x <- leaflet::addRasterImage(
            map = x,
            x = d,
            opacity = 0.8,
            project = project_on_fly,
            maxBytes = 1 * 1024 * 1024, # 1MB max size
            method = self$legend$get_resample_method(),
            colors = self$legend$get_color_map(),
            group = id,
            pane = pane_id
          )
        })
      } else if (inherits(d, "sf")) {
        ## re-project sf to 4326 for display (if necessary)
        if (
          !raster::compareCRS(
          methods::as(sf::st_crs(d), "CRS"),
          methods::as(sf::st_crs(4326), "CRS"))
          ) {
           d <- sf::st_transform(d, 4326)
          }
        ## prepare data
        d <- sf::as_Spatial(d)        
        
        if (inherits(d, "SpatialPolygonsDataFrame")) {
          f <- leaflet::addPolygons
        } else if (inherits(d, "SpatialLinesDataFrame")) {
          f <- leaflet::addPolylines
        } else if (inherits(d, "SpatialPointsDataFrame")) {
          f <- leafgl::addCircleMarkers
        } else {
          stop("unrecognized dataset format")
        }
        ### prepare colors
        col <- self$legend$get_color_map()(d[[1]])
        ### add geometry to map
        x <- f(
          map = x,
          data = d,
          stroke = FALSE,
          opacity = 0.8,
          fillOpacity = 0.8,
          color = col,
          fillColor = col,
          group = id,
          options = leaflet::pathOptions(pane = pane_id)
        )
      }
      # return result
      x
    },

    #' @description
    #' Update rendering of variable on map.
    #' @param x [leaflet::leafletProxy()] object.
    #' @param id `character` identifier for map pane.
    #' @param zindex `numeric` z-index for ordering.
    #' @param visible `logical` should the variable be visible?
    #' @return [leaflet::leaflet()] object.
    update_render = function(x, id, zindex, visible) {
      # assert arguments are valid
      assertthat::assert_that(
        inherits(x, "leaflet_proxy"),
        assertthat::is.string(id),
        assertthat::is.number(zindex),
        assertthat::is.flag(visible)
      )
      # update map pane to update variable
      leaflet::updateMapPane(x, paste0("pane-", id), zindex, visible)
    }
  )
)

#' New variable
#'
#' Create a new [Variable] object.
#'
#' @param dataset `Dataset` file path for the dataset.
#'
#' @param index `character` or `integer` indicating the field/layer with
#'  the data.
#'
#' @param total `numeric` total amount of all values in the underlying data.
#'
#' @param units `character` units for the values in the underlying data.
#'
#' @param legend `Legend` object.
#'
#' @param provenance  [Provenance] object.
#'   Defaults to `new_provenance_from_source("missing")`.
#'
#' @return A [Variable] object.
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
#' v <- new_variable(d,
#'   index = 1, total = 12, units = "ha",
#'   legend = new_continuous_legend(1, 100, c("#000000", "#AAAAAA"))
#' )
#'
#' # print object
#' print(v)
#' @export
new_variable <- function(dataset, index, units, total, legend,
                         provenance = new_provenance_from_source("missing")) {
  Variable$new(
    dataset = dataset, index = index,
    total = total, units = units, legend = legend,
    provenance = provenance
  )
}

#' New variable from automatic calculations
#'
#' Create a new [Variable] object by automatically calculating
#' all metadata from the underlying data.
#' This function is useful when pre-calculated metadata are not available.
#' Note this function will take longer to create variables than other
#' functions because it requires performing geospatial operations.
#'
#' @inheritParams new_variable
#'
#' @param type `character` indicating if the data contain
#'   continuous (`"continuous"`) or categorical (`"categorical"`)
#'   numerical values.
#'   Defaults to `"auto"` such that data are automatically identified.
#'
#' @param colors `character` object containing the colors for visualization
#'   (see Details for more information).
#'   Defaults to `"random"` such that colors are randomly generated.
#'
#' @param provenance `character` value indicating the type of provenance.
#'   The argument must be a valid type (see [new_provenance_from_source()]).
#'   Defaults to `"missing"`.
#'   
#' @param labels `character` object containing manual legend labels.
#'   Defaults to `"missing"`.
#'
#' @details
#' The argument to `colors` can be a vector of different colors
#' (in hexadecimal format, e.g. `"#112233"), or a single `character`
#' containing the name of a color palette that is used to generate a vector
#' of different colors (see [color_palette()] for more information).
#' The color palette name `"random"` is also available, such that
#' colors are generated using a randomly selected palette.
#'
#' @return A [Variable] object.
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
#' # print object
#' print(v)
#' @export
new_variable_from_auto <- function(dataset, index,
                                   units = "", type = "auto",
                                   colors = "random",
                                   provenance = "missing",
                                   labels = "missing",
                                   hidden = FALSE) {
  # assert arguments are valid
  assertthat::assert_that(
    ## dataset
    inherits(dataset, "Dataset"),
    ## index
    assertthat::is.string(index) || assertthat::is.count(index),
    assertthat::noNA(index),
    ## type
    assertthat::is.string(type),
    assertthat::noNA(type),
    type %in% c("continuous", "categorical", "manual", "auto"),
    ## units
    assertthat::is.string(units),
    assertthat::noNA(units),
    ## colors
    is.character(colors),
    assertthat::noNA(colors),
    length(colors) >= 1,
    ## provenance
    assertthat::is.string(provenance),
    assertthat::noNA(provenance),
    ## labels
    is.character(labels),
    assertthat::noNA(labels),
    length(labels) >= 1
  )

  # import attribute table
  d <- dataset$get_attribute_data()[index]
  
  # if hidden, construct Variable object with no legend
  if (hidden) {
    # create Variable object with no legend
    Variable$new(
      dataset = dataset, index = index, total = sum(d[[index]]),
      units = units, legend = new_null_legend(),
      provenance = new_provenance_from_source(provenance))
  } else {
    # if needed, automatically determine data type
    if (identical(type, "auto")) {
      type <- spatial_data_type(d, 1)
    }
    # compute statistics for data
    s <- spatial_data_statistics(d, type, 1)    
    
    # create new variable using automatically deduced settings
    new_variable_from_metadata(
      dataset = dataset,
      metadata = append(
        list(
          index = index,
          units = units,
          colors = colors,
          type = type,
          provenance = provenance,
          labels = labels
        ),
        s
      )
    )    
  }
}

#' New variable from metadata
#'
#' Create a new [Variable] object using metadata.
#' This function is useful when pre-calculated are available, so that
#' previously calculated metadata can be used.
#'
#' @inheritParams new_variable
#'
#' @param metadata `list` object (see Details for more information).
#'
#' @details
#' The argument to `metadata` should contain the following elements:
#'
#' \describe{
#' \item{"index"}{`character` or `integer` indicating the field/layer
#'   with the data within the dataset that has the data.}
#' \item{"units"}{`character` units for the values in the underlying data.}
#' \item{"type"}{`character` indicating if the data contain
#'   continuous (`"continuous"`) or categorical (`"categorical"`) values.}
#' \item{"provenance"}{`character` indicating the data source.
#'   (see [new_provenance_from_source()]).}
#' \item{"colors"}{`character` vector containing colors for visualization.}
#' \item{"total"}{`numeric` sum of all values in dataset.}
#' \item{"min_value"}{`numeric` minimum value in dataset.
#'   Required only for continuous data.}
#' \item{"max_value"}{`numeric` maximum value in dataset.
#'   Required only for continuous data.}
#' \item{"values"}{`numeric` vector of unique value in dataset.
#'   Required only for categorical data.}
#' \item{"labels"}{`character` vector of unique labels for manual legend.
#'   Required only for categorical data, manual legend.}
#' }
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
#' v <- new_variable_from_metadata(
#'   d, list(
#'     index = 1, units = "ha", type = "continuous",
#'     colors = c("#000000", "#AAAAAA"), total = 12,
#'     min_value = 1, max_value = 3, provenance = "missing", 
#'     labels = "missing"
#'   )
#' )
#'
#' # print object
#' print(v)
#' @export
new_variable_from_metadata <- function(dataset, metadata) {
  # assert arguments are valid
  assertthat::assert_that(
    ## dataset
    inherits(dataset, "Dataset"),
    ## metadata
    is.list(metadata)
  )
  assertthat::assert_that(
    ## index
    assertthat::is.string(metadata$index) ||
      assertthat::is.count(metadata$index),
    assertthat::noNA(metadata$index),
    ## units
    assertthat::is.string(metadata$units),
    assertthat::noNA(metadata$units),
    ## type
    assertthat::is.string(metadata$type),
    assertthat::noNA(metadata$type),
    metadata$type %in% c("continuous", "categorical", "manual", "auto"),
    ## colors
    is.character(metadata$colors),
    assertthat::noNA(metadata$colors),
    ## total
    assertthat::is.number(metadata$total),
    assertthat::noNA(metadata$total),
    ## provenance
    assertthat::is.string(metadata$provenance),
    assertthat::noNA(metadata$provenance)
  )
  if (identical(metadata$type, "continuous")) {
    ## continuous metrics
    assertthat::assert_that(
      assertthat::is.number(metadata$min_value),
      assertthat::noNA(metadata$min_value),
      assertthat::is.number(metadata$max_value),
      assertthat::noNA(metadata$max_value)
    )
  } else {
    ## categorical metrics
    assertthat::assert_that(
      is.numeric(metadata$values),
      assertthat::noNA(metadata$values)
    )
  }

  # validate colors
  if (startsWith(metadata$colors[[1]], "#")) {
    ## if first element of colors does start with hash symbol,
    ## then verify if colors are represented using hexadecimal format
    assertthat::assert_that(
      all(startsWith(metadata$colors, "#")),
      all(nchar(metadata$colors) %in% c(7, 9))
    )
  } else {
    ## if first element of colors does not start with hash symbol,
    ## then assume it should be the name of a palette
    assertthat::assert_that(
      length(metadata$colors) == 1
    )
  }

  # prepare colors
  if (length(metadata$colors) == 1) {
    ## generate colors using palette name
    if (identical(metadata$type, "categorical")) {
      colors <-
        color_palette(metadata$colors, length(metadata$values))
    } else {
      colors <- color_palette(metadata$colors, NULL)
    }
  } else {
    colors <- metadata$colors
    if (identical(metadata$type, "categorical")) {
      ## verify that correct number of colors supplied if categorical legend
      assertthat::assert_that(
        length(colors) == length(metadata$values),
        msg = paste0(
          metadata$type, " data has ", length(metadata$values),
          " unique values and so ",
          "the argument to \"colors\" must contain this many elements."
        )
      )
    } else {
      ## verify that at least two colors supplied for continuous legend
      assertthat::assert_that(
        length(colors) >= 2,
        msg = paste0(
          metadata$type,
          " data requires at least 2 colors to create a color ramp."
        )
      )
    }
  }
  
  # create legend
  if (identical(metadata$type, "continuous")) {
    legend <- new_continuous_legend(
      min_value = metadata$min_value,
      max_value = metadata$max_value,
      colors = colors
    )
  } else if (identical(metadata$type, "categorical")) {
    legend <- new_categorical_legend(
      values = metadata$values,
      colors = colors
    )
  } else if (identical(metadata$type, "manual")) {
    legend <- new_manual_legend(
      values = metadata$values,
      colors = metadata$colors,
      labels = metadata$labels
    )
  } 

  # create object
  Variable$new(
    dataset = dataset, index = metadata$index, total = metadata$total,
    units = metadata$units, legend = legend,
    provenance = new_provenance_from_source(metadata$provenance)
  )
}
