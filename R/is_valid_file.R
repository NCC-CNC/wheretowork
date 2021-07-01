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
  #TODO
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
  #TODO
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
  #TODO
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
  #TODO
  TRUE
}
