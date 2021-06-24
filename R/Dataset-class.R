#' @include internal.R
NULL

#' Dataset class
#'
#' Definition for the Dataset class.
#'
#' @seealso [new_dataset()].
Dataset <- R6::R6Class(
  "Dataset",
  public = list(

    #' @field id `character` identifier.
    id = NA_character_,

    #' @field spatial_path `character` file path.
    spatial_path = NA_character_,

    #' @field attribute_path `character` file path.
    attribute_path = NA_character_,

    #' @field boundary_path `character` file path.
    boundary_path = NA_character_,

    #' @field spatial_data `NULL`, [sf::st_sf()], or [raster::raster()] object.
    spatial_data = NULL,

    #' @field attribute_data `NULL`, or [tibble::tibble()] object.
    attribute_data = NULL,

    #' @field boundary_data `NULL`, or [Matrix::sparseMatrix()] object.
    boundary_data = NULL,

    #' @description
    #' Create a Dataset object.
    #' @param id `character` value.
    #' @param spatial_path `character` file path.
    #' @param attribute_path `character` file path.
    #' @param boundary_path `character` file path.
    #' @param spatial_data [sf::st_sf()], or [raster::raster()] object.
    #' @param attribute_data [tibble::tibble()] object.
    #' @param boundary_data [Matrix::sparseMatrix()] object.
    #' @return A new Dataset object.
    initialize = function(
      id, spatial_path, attribute_path, boundary_path,
      spatial_data, attribute_data, boundary_data) {
      ## assert that arguments are valid
      assertthat::assert_that(
        ## id
        assertthat::is.string(id),
        assertthat::noNA(id),
        ## spatial_path
        assertthat::is.string(spatial_path),
        assertthat::noNA(spatial_path),
        ## attribute_path
        assertthat::is.string(attribute_path),
        assertthat::noNA(attribute_path),
        ## boundary_path
        assertthat::noNA(boundary_path),
        assertthat::is.string(boundary_path),
        ## spatial_data
        inherits(spatial_data, c("NULL", "sf", "Raster")),
        ## attribute_data
        inherits(attribute_data, c("NULL", "data.frame")),
        ## boundary_data
        inherits(boundary_data, c("NULL", "dsCMatrix"))
      )
      ## validate paths
      if (!identical(spatial_path, "memory")) {
        assertthat::assert_that(assertthat::is.readable(spatial_path))
      }
      if (!identical(attribute_path, "memory")) {
        assertthat::assert_that(assertthat::is.readable(attribute_path))
      }
      if (!identical(boundary_path, "memory")) {
        assertthat::assert_that(assertthat::is.readable(boundary_path))
      }
      ## set fields
      self$id <- id
      self$spatial_path <- spatial_path
      self$attribute_path <- attribute_path
      self$boundary_path <- boundary_path
      self$spatial_data <- spatial_data
      self$attribute_data <- attribute_data
      self$boundary_data <- boundary_data

      ## validate attribute data
      if (inherits(attribute_data, "data.frame")) {
        assertthat::assert_that(
          assertthat::has_name(self$attribute_data, "_index"),
          last(names(self$attribute_data)) == "_index"
        )
      }
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Dataset")
      message("  paths:")
      message("    spatial: ", self$spatial_path)
      message("    attribute: ", self$attribute_path)
      message("    boundary: ", self$boundary_path)
      invisible(self)
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @return `character` value.
    repr = function() {
      if (identical(self$spatial_path, "memory")) {
        out <- "memory"
      } else {
        out <- paste0(".../", basename(self$spatial_path))
      }
      out
    },

    #' @description
    #' Import the data into memory.
    import = function() {
      # if data files are not stored in memory, then import them
      ## spatial data
      if (is.null(self$spatial_data)) {
        suppressWarnings({
          self$spatial_data <- read_spatial_data(self$spatial_path)
        })
      }
      ## attribute data
      if (is.null(self$attribute_data)) {
        ### import data
        self$attribute_data <-
          tibble::as_tibble(data.table::fread(
            self$attribute_path, data.table = FALSE))
        ## validate attribute data
        assertthat::assert_that(
          assertthat::has_name(self$attribute_data, "_index"),
          last(names(self$attribute_data)) == "_index")
      }
      ## boundary data
      if (is.null(self$boundary_data)) {
        ### import data
        bd <- tibble::as_tibble(data.table::fread(
            self$boundary_path, data.table = FALSE))
        ### find dimensions
        if (inherits(self$spatial_data, "sf")) {
          n_total_units <- nrow(self$spatial_data)
        } else {
          n_total_units <- raster::ncell(self$spatial_data[[1]])
        }
        ### convert to matrix
        self$boundary_data <-
          Matrix::sparseMatrix(
            i = bd[[1]], j = bd[[2]], x = bd[[3]],
            index1 = FALSE, repr = "C", symmetric = TRUE,
            dims = rep(n_total_units, 2)
          )
      }
      invisible(self)
    },

    #' @description
    #' Write the data to disk.
    #' @param spatial_path `character` file path.
    #' @param attribute_path `character` file path.
    #' @param boundary_path `character` file path.
    write = function(spatial_path, attribute_path, boundary_path) {
      # assert that arguments are valid
      assertthat::assert_that(
        assertthat::is.string(spatial_path),
        assertthat::noNA(spatial_path),
        assertthat::is.string(attribute_path),
        assertthat::noNA(attribute_path),
        assertthat::is.string(boundary_path),
        assertthat::noNA(boundary_path))
      self$import()
      # spatial data
      if (inherits(self$spatial_data, "sf")) {
        suppressWarnings({
          sf::write_sf(self$spatial_data, spatial_path)
        })
      } else {
        suppressWarnings({
          raster::writeRaster(
            self$spatial_data, spatial_path, overwrite = TRUE, NAflag = -9999)
        })
      }
      # attribute data
      data.table::fwrite(
        self$attribute_data, attribute_path, sep = ",", row.names = FALSE)
      # boundary data
      bd <- methods::as(self$boundary_data, "dsTMatrix")
      data.table::fwrite(
        tibble::tibble(i = bd@i, j = bd@j, x = bd@x),
        boundary_path, sep = ",", row.names = FALSE)
      # return result
      invisible(self)
    },

    #' @description
    #' Clean the dataset from memory.
    #' @details
    #' Note that this method has no effect if the dataset does not have
    #' file path on disk.
    clean = function() {
      ## spatial data
      if (!identical(self$spatial_path, "memory")) {
        self$spatial_data <- NULL
      }
      ## attribute data
      if (!identical(self$attribute_path, "memory")) {
        self$attribute_data <- NULL
      }
      ## boundary data
      if (!identical(self$boundary_path, "memory")) {
        self$boundary_data <- NULL
      }
      invisible(self)
    },

    #' @description
    #' Get the spatial data.
    #' @return [sf::st_as_sf()] or [raster::raster()] object.
    get_spatial_data = function() {
      self$import()
      self$spatial_data
    },

    #' @description
    #' Get the attribute data.
    #' @return [tibble::tibble()] object.
    get_attribute_data = function() {
      self$import()
      self$attribute_data
    },

    #' @description
    #' Get the spatial data.
    #' @return [Matrix::sparseMatrix()] object.
    get_boundary_data = function() {
      self$import()
      self$boundary_data
    },

    #' @description
    #' Get the coordinate reference system.
    #' @return [sf::st_crs()] object.
    get_crs = function() {
      self$import()
      sf::st_crs(self$spatial_data)
    },

    #' @description
    #' Get planning unit indices.
    #' @return `integer` vector of indices.
    get_planning_unit_indices = function() {
      self$import()
      self$attribute_data[["_index"]]
    },

    #' @description
    #' Get area values.
    #' @return `numeric` vector of values.
    get_planning_unit_areas = function() {
      self$import()
      idx <- self$attribute_data[["_index"]]
      if (inherits(self$spatial_data, "Raster")) {
        out <-
          rep(prod(raster::res(self$spatial_data)), length(idx))
      } else {
        out <- as.numeric(sf::st_area(self$spatial_data[idx, ]))
      }
      out
    },

    #' @description
    #' Get a data from the dataset at an index.
    #' @param index `character` or `integer` indicating the field/layer with
    #'   the data.
    #' @return [sf::st_as_sf()] or [raster::raster()] object.
    get_index = function(index) {
      assertthat::assert_that(
        assertthat::is.string(index) || assertthat::is.count(index),
        assertthat::noNA(index),
        self$has_index(index))
      self$import()
      idx <- self$attribute_data[["_index"]]
      if (inherits(self$spatial_data, "Raster")) {
        out <- raster::setValues(self$spatial_data, NA_real_)
        out[idx] <- self$attribute_data[[index]]
      } else {
        out <- sf::st_as_sf(tibble::tibble(
            x = self$attribute_data[[index]][idx],
            geometry = sf::st_geometry(self$spatial_data)[idx]))
        attr(out, "agr") <- NULL
      }
      if (is.character(index)) {
        names(out)[[1]] <- index
      } else {
        names(out)[[1]] <- paste0("V", index)
      }
      out
    },

    #' @description
    #' Check if the dataset has an index.
    #' @param index `character` or `integer` indicating the field/layer with
    #'   the data.
    #' @return `logical` indicating if data is present or not.
    has_index = function(index) {
      assertthat::assert_that(
        assertthat::is.string(index) || assertthat::is.count(index),
        assertthat::noNA(index))
      self$import()
      if (is.numeric(index)) {
        out <-
          index %in% seq_along(names(self$attribute_data)[-1])
      } else {
        out <-
          index %in% (names(self$attribute_data)[-ncol(self$attribute_data)])
      }
      out
    },

    #' @description
    #' Maximum index.
    #' @return `integer` largest index.
    max_index = function() {
      self$import()
      length(names(self$attribute_data)) - 1
    },

    #' @description
    #' Add data at an index.
    #' @param index `character` or `integer` indicating the field/layer with
    #'   the data.
    #' @param values `numeric` vector.
    add_index = function(index, values) {
      assertthat::assert_that(
        assertthat::is.string(index) || assertthat::is.count(index),
        assertthat::noNA(index))
      self$import()
      assertthat::assert_that(
        length(values) == nrow(self$attribute_data))
      self$attribute_data[[index]] <- values
      self$attribute_data <-
        self$attribute_data[,
          c(setdiff(names(self$attribute_data), "_index"), "_index")]
      invisible(self)
    }

  )
)

#' New dataset
#'
#' Create a new [Dataset] object.
#'
#' @param spatial_path `character` file path for spatial data.
#'
#' @param attribute_path `character` file path for attribute data.
#'
#' @param boundary_path `character` file path for boundary data.
#'
#' @param spatial_data `NULL`, [sf::st_sf()], or [raster::raster()] object.
#'   Defaults to `NULL` such that data are automatically imported
#'   using the argument to `spatial_path`.
#'
#' @param attribute_data `NULL`, or [tibble::tibble()] object.
#'   Defaults to `NULL` such that data are automatically imported
#'   using the argument to `attribute_path`.
#'
#' @param boundary_data `NULL`, or [Matrix::sparseMatrix()] object.
#'   Defaults to `NULL` such that data are automatically imported
#'   using the argument to `boundary_path`.
#'
#' @param id `character` unique identifier.
#'   Defaults to a random identifier ([uuid::UUIDgenerate()]).
#'
#' @return A [Dataset] object.
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
#' # print object
#' print(d)
#'
#' @export
new_dataset <- function(
  spatial_path, attribute_path, boundary_path,
  spatial_data = NULL, attribute_data = NULL, boundary_data = NULL,
  id = uuid::UUIDgenerate()) {
  # verify that data are supplied when specifying that data
  # are stored in memory
  if (identical(spatial_path, "memory")) {
    assertthat::assert_that(!is.null(spatial_data))
  }
  if (identical(attribute_path, "memory")) {
    assertthat::assert_that(!is.null(attribute_data))
  }
  if (identical(boundary_path, "memory")) {
    assertthat::assert_that(!is.null(boundary_data))
  }
  # create new dataset
  Dataset$new(
    id = id,
    spatial_path = spatial_path,
    attribute_path = attribute_path,
    boundary_path = boundary_path,
    spatial_data = spatial_data,
    attribute_data = attribute_data,
    boundary_data = boundary_data
  )
}

#' New dataset from automatic calculations
#'
#' Create a new [Dataset] object by automatically calculating
#' all metadata from the underlying data.
#' This function is useful when pre-calculated metadata are not available.
#' Note this function will take longer to create variables than other
#' functions because it requires performing geospatial operations.
#'
#' Create a new [Dataset] object.
#'
#' @param x [sf::st_sf()] or [raster::stack()] object.
#'
#' @inheritParams new_dataset
#'
#' @inherit new_dataset return
#'
#' @examples
#' # find example data
#' f <- system.file(
#'   "extdata", "sim_raster_spatial.tif", package = "locationmisc")
#'
#' # import data
#' r <- suppressWarnings(raster::raster(f))
#' r <- raster::stack(r, r * 2, r * 3, r* 4)
#'
#' # create new dataset
#' d <- new_dataset_from_auto(r)
#'
#' # print object
#' print(d)
#'
#' @export
new_dataset_from_auto <- function(x, id = uuid::UUIDgenerate()) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, c("sf", "Raster")))
  # prepare geometry data
  if (inherits(x, "sf")) {
    x[["_index"]] <- seq_len(nrow(x))
    spatial_data <- x[, "_index"]
  } else {
    spatial_data <- x[[1]]
  }
  # prepare attribute data
  if (inherits(x, "sf")) {
    attribute_data <- sf::st_drop_geometry(spatial_data)
  } else {
    attribute_data <- raster::as.data.frame(x, na.rm = FALSE)
    pu_idx <- rowSums(is.na(as.matrix(attribute_data)))
    attribute_data <- tibble::as_tibble(attribute_data)
    attribute_data[["_index"]] <- seq_len(nrow(attribute_data))
    attribute_data <- attribute_data[pu_idx < 0.5, , drop = FALSE]
  }
  # determine settings for boundary matrix
  str_tree <- inherits(x, "sf") && !identical(Sys.info()[["sysname"]], "Darwin")
  # create new dataset
  Dataset$new(
    spatial_path = "memory",
    attribute_path = "memory",
    boundary_path = "memory",
    spatial_data = spatial_data,
    attribute_data = attribute_data,
    boundary_data = prioritizr::boundary_matrix(
      spatial_data, str_tree = str_tree),
    id = id
  )
}
