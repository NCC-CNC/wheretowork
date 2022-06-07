#' @include internal.R
NULL

#' Is valid configuration file?
#'
#' Verify if a file path corresponds to a valid configuration file.
#'
#' @param x `character` file path.
#'
#' @return `logical` value indicating validity.
#'
#' @export
is_valid_configuration_file <- function(x) {
  # assert argument is valid
  assertthat::assert_that(assertthat::is.string(x))

  # check if YAML file based on file name
  if (!endsWith(x, ".yaml")) {
    return("Error: file must have a \".yaml\" extension")
  }

  # try importing file
  f <- try(yaml::read_yaml(x), silent = TRUE)

  # check if can be parsed as valid YAML file
  if (inherits(f, "try-error")) {
    return("Error: not valid file format")
  }

  # check if YAML file contains correct keys
  key_names <-
    c(
      "name", "spatial_path", "attribute_path", "boundary_path",
      "mode", "themes"
    )
  if (!all(c(key_names) %in% names(f))) {
    return("Error: YAML file is missing data")
  }

  # return success
  TRUE
}

#' Is valid spatial file?
#'
#' Verify if a file path corresponds to a valid spatial dataset.
#'
#' @inheritParams is_valid_configuration_file
#'
#' @inherit is_valid_configuration_file return
#'
#' @export
is_valid_spatial_file <- function(x) {
  # assert argument is valid
  assertthat::assert_that(is.character(x))

  # check if is a vector or raster format
  if (any(endsWith(x, ".shp"))) {
    ## extract shp file
    p <- x[endsWith(x, ".shp")]

    ## verify that shapefile has all components
    if (!any(endsWith(x, ".shx"))) {
      return("Error: missing \".shx\" file")
    }
    if (!any(endsWith(x, ".dbf"))) {
      return("Error: missing \".dbf\" file")
    }
    if (!any(endsWith(x, ".prj"))) {
      return("Error: missing \".prj\" file")
    }

    ## verify valid shapefile
    ## note that only first couple of rows are imported to reduce run time
    l <- try(sf::st_layers(p)$name[[1]], silent = TRUE)
    if (inherits(l, "try-error")) {
      return("Error: not valid ESRI Shapefile format")
    }
    f <- suppressWarnings(try(sf::read_sf(p), silent = TRUE))
    if (inherits(f, "try-error")) {
      return("Error: not valid ESRI Shapefile format")
    }

    ## try to any fix geometry issues
    f <- try(repair_spatial_data(f), silent = TRUE)
    if (inherits(f, "try-error") || !all(sf::st_is_valid(f))) {
      return(
        "Error: ESRI Shapefile geometry needs repairing"
      )
    }

    ## verify projection (vector)
    if (is.na(methods::as(sf::st_crs(f), "CRS"))) {
      return("Error: coordinate reference system must be defined")
    } else if (sf::st_is_longlat(f)) {
      return("Warning: projected coordinate system recommended")
    }
  } else if (any(endsWith(x, ".tif"))) {
    ## extract tif file
    p <- x[endsWith(x, ".tif")]

    ## verify that valid tiff file
    f <- suppressWarnings(try(raster::raster(p), silent = FALSE))
    if (inherits(f, "try-error")) {
      return("Error: not valid GeoTIFF file format")
    }

    ## verify projection (raster)
    if (is.na(methods::as(sf::st_crs(f), "CRS"))) {
      return("Error: coordinate reference system must be defined")
    } else if (raster::isLonLat(f)) {
      return("Error: projected coordinate system required")
    }
  } else {
    ## throw error because file is not ESRI Shapefile or GeoTIFF
    return("Error: not valid spatial data file format.")
  }

  # return success
  TRUE
}

#' Is valid attribute file?
#'
#' Verify if a file path corresponds to a valid attribute dataset.
#'
#' @inheritParams is_valid_configuration_file
#'
#' @inherit is_valid_configuration_file return
#'
#' @export
is_valid_attribute_file <- function(x) {
  # assert argument is valid
  assertthat::assert_that(assertthat::is.string(x))

  # check if YAML file based on file name
  if (!endsWith(x, ".csv") && !endsWith(x, ".csv.gz")) {
    return("Error: file must have a \".csv\" or \".csv.gz\" extension")
  }

  # try importing file
  f <- try(data.table::fread(x, data.table = FALSE), silent = TRUE)

  # check if can be parsed as valid file
  if (inherits(f, "try-error")) {
    return("Error: not valid file format")
  }

  # check if has data
  if (nrow(f) == 0 || ncol(f) == 0) {
    return("Error: file contains no data")
  }

  # check if has _index column
  if (last(names(f)) != "_index") {
    return("Error: file does not contain valid attribute data")
  }

  # return success
  TRUE
}

#' Is valid boundary file?
#'
#' Verify if a file path corresponds to a valid boundary dataset.
#'
#' @inheritParams is_valid_configuration_file
#'
#' @inherit is_valid_configuration_file return
#'
#' @export
is_valid_boundary_file <- function(x) {
  # assert argument is valid
  assertthat::assert_that(assertthat::is.string(x))

  # check if YAML file based on file name
  if (
    !endsWith(x, ".csv") &&
    !endsWith(x, ".csv.gz") &&
    !endsWith(x, ".dat") &&
    !endsWith(x, ".dat.gz")
  ) {
    return(paste0(
      "Error: file must have a \".csv\", \".csv.gz\" ",
      "\".dat\", or \".dat.gz\" extension"
    ))
  }

  # try importing file
  f <- try(data.table::fread(x, sep = ",", data.table = FALSE), silent = TRUE)

  # check if can be parsed as valid file
  if (inherits(f, "try-error")) {
    return("Error: not valid file format")
  }

  # check if has data
  if (nrow(f) == 0 || ncol(f) == 0) {
    return("Error: file contains no data")
  }

  # check if has correct columns
  if (endsWith(x, ".csv") || endsWith(x, ".csv.gz")) {
    if (!identical(names(f), c("i", "j", "x"))) {
      return("Error: file does not contain valid boundary data")
    }
  } else {
    if (!identical(names(f), c("id1", "id2", "boundary"))) {
      return("Error: file does not contain valid boundary data")
    }
  }

  # return success
  TRUE
}
