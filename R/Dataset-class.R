#' @include internal.R
NULL

#' Dataset class
#'
#' Definition for the Dataset class.
#'
#' @seealso [new_dataset()], [read_dataset()], [read_dataset_with_metadata()].
Dataset <- R6::R6Class(
  "Dataset",
  public = list(

    #' @field source `character` value.
    source = NA_character_,

    #' @field subset `character` or `integer` value indicating the
    #'  field/layer within the dataset with the data.
    subset = NULL,

    #' @field total `numeric` value.
    total = NA_real_,

    #' @field units `character` value.
    units = NA_character_,

    #' @field legend `Legend` object.
    legend = NULL,

    #' @description
    #' Create a Dataset object.
    #' @param source `character` value.
    #' @param subset `character` or `integer` value.
    #' @param total `numeric` value.
    #' @param units `character` value.
    #' @param legend `Legend` object.
    #' @return A new Dataset object.
    initialize = function(source, subset, total, units, legend) {
      ### assert that arguments are valid
      assertthat::assert_that(
        #### source
        assertthat::is.string(source),
        assertthat::noNA(source),
        ## subset
        assertthat::is.string(subset) || assertthat::is.count(subset),
        assertthat::noNA(subset),
        #### total
        assertthat::is.number(total),
        assertthat::noNA(total),
        isTRUE(total >= 0),
        #### units
        assertthat::is.string(units),
        assertthat::noNA(units),
        #### legend
        inherits(legend, c("ContinuousLegend", "CategoricalLegend")))
      ### set fields
      self$source <- source
      self$subset <- subset
      self$total <- total
      self$units <- units
      self$legend <- legend
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Dataset")
      message("  source: ", self$source)
      message("  subset: ", self$subset)
      message("  total:  ", round(self$total, 2))
      message("  units:  ", self$units)
      invisible(self)
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @param start `character` symbol used to start the parameter list.
    #'   Defaults to `"["`.
    #' @param end `character` symbol used to start the parameter list.
    #'   Defaults to `"]"`.
    #' @return `character` value.
    repr = function(start = "[", end = "]") {
      paste0(
        ".../", basename(self$source),
        " ", start, "total: ", round(self$total, 2), " ",
        self$units, end)
    }

    #' @description
    #' Get the data.
    #' @return [sf::st_as_sf()] or [rater::raster()] object.
    get_data = function() {
      out <- read_spatial_data(self$source)
      if (is.character(self$subset)) {
        i <- which(names(out) == self$subset)
        asserrthat::assert_that(
          length(i) == 1,
          msg = paste0(
            "dataset does not contain a \"", self$subset,
            "\" field/layer name"))
      } else {
        asserrthat::assert_that(
          i %in% seq_along(names(i)),
          msg = paste0(
            "dataset does not contain a field/layer at index \"", self$subset,
            "\""))
        i <- self$subset
      }
      out[[i]]
    }
  )
)

#' New dataset
#'
#' Create a new [Dataset] object.
#'
#' @param source `character` file path for the dataset.
#'
#' @param subset `character` or `integer` indicating the field/layer with
#'  data within the dataset.
#'
#' @param total `numeric` total amount of all values in the underlying data.
#'
#' @param units `character` units for the values in the underlying data.
#'
#' @param legend `Legend` object.
#'
#' @return A [Dataset] object.
#'
#' @examples
#' # create new object
#' l <- new_dataset(
#'   source = tempfile(), subset = 1, total = 12, units = "ha",
#'   legend = new_continuous_legend(1, 100, c("#000000", "#AAAAAA")))
#'
#' # print object
#' print(l)
#'
#' @export
new_dataset <- function(source, subset, total, units, legend) {
  Dataset$new(
    source = source, subset = subset,
    total = total, units = units, legend = legend)
}

#' Read dataset
#'
#' Read a [Dataset] object from disk
#' This function is useful when pre-calculated metadata are not available,
#' because it will automatically calculate the required metadata to import
#' the dataset.
#'
#' @inheritParams new_dataset
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
#' @details
#' The argument to `colors` can be a vector of different colors
#' (in hexadecimal format, e.g. `"#112233"), or a single `character`
#' containing the name of a color palette that is used to generate a vector
#' of different colors (see [color_palette()] for more information).
#' The color palette name `"random"` is also available, such that
#' colors are generated using a randomly selected palette.
#'
#' @return A [Dataset] object.
#'
#' @examples
#' # load example raster
#' data(sim_pu_raster, package = "prioritizr")
#' r <- sim_pu_raster
#'
#' # create temporary file name for saving raster to disk
#' f <- tempfile(fileext = ".tif")
#'
#' # save raster to disk
#' f <- writeRaster(r, f, NAflag = -9999)
#'
#' # create new object
#' d <- read_dataset(f)
#'
#' # print object
#' print(d)
#'
#' @export
read_dataset <- function(
  source, subset, type = "auto", colors = "random") {
  # assert arguments are valid
  assertthat::assert_that(
    ## source
    assertthat::is.string(source),
    assertthat::noNA(source),
    ## subset
    assertthat::is.string(subset) || assertthat::is.count(subset),
    assertthat::noNA(subset),
    ## type
    assertthat::is.string(type),
    assertthat::noNA(type),
    type %in% c("continuous", "categorical", "auto"),
    ## units
    assertthat::is.string(units),
    assertthat::noNA(units),
    ## colors
    is.character(colors),
    assertthat::noNA(colors),
    length(colors) >= 1)
  # validate colors
  if (startsWith(colors[[1]], "#")) {
    ## if first element of colors does not start with hash symbol,
    ## then assume it should be the name of a palette
    assertthat::assert_that(
      length(colors) == 1)
  } else {
    ## if first element of colors does start with hash symbol,
    ## then verify if colors are represented using hexadecimal format
    assertthat::assert_that(
      all(startsWith(colors, "#")),
      all(nchar(colors) %in% c(7, 9)))
  }

  # import dataset
  data <- read_spatial_dataset(source)

  # if needed, automatically determine data type
  if (identical(type, "auto")) {
    type <- spatial_data_type(data)
  }

  # compute statistics for data
  ss <- spatial_data_statistics(data, type)

  # prepare colors
  if (length(colors) == 1) {
    ## generate colors using palette name
    if (identical(type, "categorical")) {
      colors <- color_palette(colors, n = length(ss$values))
    } else {
      colors <- color_palette(colors, n = 20)
    }
  } else {
    if (identical(type, "categorical")) {
      ## verify that correct number of colors supplied if categorical legend
      asserrthat::assert_that(
        length(colors) == length(ss$values),
        msg = paste0(
          type, " data has ", length(ss$values), " unique values and so ",
          "the argument to \"colors\" must contain this many elements."))
    } else {
      ## verify that at least two colors supplied for continuous legend
      asserrthat::assert_that(
        length(colors) >= 2,
        msg = paste0(
          type, " data requires at least 2 colors to create a color ramp."))
    }
  }

  # create new dataset using mock config file
  read_dataset_with_metadata(
    list(
      source = source,
      units = units,
      type = type,
      colors = colors,
      statistics = ss)
  )
}

#' Read dataset with metadata
#'
#' Read a [Dataset] object from disk using metadata.
#' This function is useful when pre-calculated are available, so that
#' previously calculated metadata can be used to import the dataset.
#'
#' @param x `list` object (see Details for more information).
#'
#' @details
#' The argument to `x` should contain the following elements:
#'
#' \describe{
#' \item{"source"}{`character` file path for the data.}
#' \item{"subset"}{`character` or `integer` indicating the field/layer
#'   with the data within the dataset.}
#' \item{"units"}{`character` units for the values in the underlying data.}
#' \item{"type"}{`character` indicating if the data contain
#'   continuous (`"continuous"`) or categorical (`"categorical"`)
#'   numerical values.}
#' \item{"colors"}{`character` vector containing colors for visualization.}
#' \item{"statistics"}{`list` object containing statistics:
#'    \describe{
#'    \item{"total"}{`numeric` sum of all values in dataset.
 #'     Required for both continuous and categorical data.}
#'    \item{"min_value"}{`numeric` minimum value in dataset.
#'      Required for continuous data, and optional for categorical data.}
#'    \item{"max_value"}{`numeric` maximum value in dataset.
#'      Required for continuous data, and optional for categorical data.}
#'    \item{"values"}{`numeric` vector of unique value in dataset.
#'      Required for categorical data, and optional for continuous data.}
#'    }
#' }
#' }
#'
#' @export
read_dataset_with_metadata <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(
    ## source
    assertthat::is.string(x$source),
    assertthat::noNA(x$source),
    ## subset
    assertthat::is.string(x$subset),
    assertthat::noNA(x$subset),
    ## type
    assertthat::is.string(x$type),
    assertthat::noNA(x$type),
    type %in% c("continuous", "categorical"),
    ## units
    assertthat::is.string(x$units),
    assertthat::noNA(x$units),
    ## colors
    is.character(x$colors),
    assertthat::noNA(x$colors),
    ## summary statistics
    is.list(x$statistics)
  )

  # create legend
  if (identical(type, "continuous")) {
    legend <- new_continuous_legend(
      min_value = x$statistics$min_value,
      max_value = x$statistics$max_value,
      colors = x$colors)
  } else {
    legend <- new_categorical_legend(
      values = x$statistics$values,
      colors = x$colors)
  }

  # create object
  Dataset$new(
    source = source, subset = subset, total = total,
    units = units, legend = legend)
}
