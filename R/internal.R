#' All elements inherit
#'
#' Check if all elements in a `list` object inherit from a class.
#'
#' @param x `list` object.
#'
#' @param class `character` class name.
#
#' @return `logical` indicating if all elements in `x` inherit from `class`.
#'
#' @noRd
all_list_elements_inherit <- function(x, class) {
  assertthat::assert_that(
    is.list(x),
    is.character(class),
    assertthat::noNA(class))
  all(vapply(x, inherits, logical(1), class))
}

assertthat::on_failure(all_list_elements_inherit) <- function(call, env) {
  paste0("all ", deparse(call$x), " do not inherit from", deparse(call$class))
}

# Return the correct new line character symbol for the system
nl <- function() {
  ifelse(identical(.Platform$OS.type, "unix"), "\n", "\n\r")
}

# alias for dplyr::n_distinct()
n_distinct <- function(x) {
  length(unique(x))
}

# alias for dplyr::last()
last <- function(x) {
  x[[length(x)]]
}

# alias for (x - mean(x)) / sd(x)
zscale <- function(x) {
  sdx <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(sdx) || isTRUE(sdx < 1e-5)) {
    sdx <- 1
  }
  (x - mean(x, na.rm = TRUE)) / sdx
}

#' Example include names
#'
#' Import example include names for simulating [Include] objects.
#'
#' @return A [tibble()] object.
#'
#' @noRd
example_include_names <- function() {
  tibble::as_tibble(
    utils::read.table(
      system.file("extdata", "example-includes.csv", package = "locationmisc"),
      stringsAsFactors = FALSE,
      sep = ",",
      header = TRUE
    )
  )
}

#' Example weight names
#'
#' Import example weight names for simulating [Weight] objects.
#'
#' @return A [tibble()] object.
#'
#' @noRd
example_weight_names <- function() {
  tibble::as_tibble(
    utils::read.table(
      system.file("extdata", "example-weights.csv", package = "locationmisc"),
      stringsAsFactors = FALSE,
      sep = ",",
      header = TRUE
    )
  )
}

#' Example theme names
#'
#' Import species data and prepare it for simulating [Theme] objects.
#'
#' @return A [tibble()] object.
#'
#' @noRd
example_theme_names <- function() {
  # import data
  suppressMessages({
    d <-
      readxl::read_excel(
        system.file(
          "extdata", "Clements-Checklist-v2019-August-2019.xlsx",
          package = "locationmisc"),
        sheet = 1)
   })
   # format column names
   d <- stats::setNames(d, gsub(" ", "_", tolower(names(d)), fixed = TRUE))
   d <- tibble::as_tibble(d)
   # select relevant columns
   d <- d[, c("english_name", "family")]
   # subset to include only species with English common names
   d <- stats::na.omit(d)
   # remove duplicates
   d <- d[!duplicated(d$english_name), ]
   # extract English family names
   d$family <-
    gsub(
      "[\\(\\)]", "",
      regmatches(d$family, gregexpr("\\(.*?\\)", d$family)))
  # rename columns for output
  d <- stats::setNames(d, c("feature", "theme"))
  # return result
  d
}

#' Example feature icon
#'
#' Randomly generate an icon for an example feature.
#'
#' @return A `shiny.tag` icon.
#'
#' @noRd
example_feature_icon <- function() {
  # define names for feature icons
  x <- c(
    "map-marked-alt", "atlas", "map-pin", "map", "database",
    "hdd")
  # return icon
  shiny::icon(sample(x, 1))
}

#' Example theme icon
#'
#' Randomly generate an icon for an example theme.
#'
#' @return A `shiny.tag` icon.
#'
#' @noRd
example_theme_icon <- function() {
  # define names for theme icons
  x <- c(
    "cat", "crow", "dog", "dove", "dragon", "fish",
    "frog", "hippo", "horse", "kiwi-bird", "otter", "spider")
  # return icon
  shiny::icon(sample(x, 1))
}

#' Make valid names
#'
#' Coerce a `character` vector to valid field/layer names for a spatial or
#' raster dataset.
#'
#' @param x `character` object.
#'
#' @return `character` object.
#'
#' @noRd
make_valid_names <- function(x) {
  assertthat::assert_that(
    is.character(x),
    assertthat::noNA(x))
  x <- make.names(x)
  x <- gsub(pattern = ".", replacement = "_", x, fixed = TRUE)
  x <- gsub(pattern = "(_)\\1+", replacement = "\\1", x)
  x
}

#' Extract color opacity
#'
#' Extract the opacity from a hexadecimal color (i.e.`#RRGGBBAA`).
#'
#' @param x `character` object.
#'
#' @return `numeric` values between zero and one.
#'
#' @noRd
color_opacity <- function(x) {
  assertthat::assert_that(is.character(x))
  nc <- nchar(x)
  nc9 <- nc == 9
  out <- numeric(length(x))
  out[nc == 7] <- 1
  out[nc9] <- strtoi(substr(x[nc9], 8, 9), base = 16) / 255
  out
}
