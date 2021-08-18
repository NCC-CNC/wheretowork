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
    assertthat::noNA(class)
  )
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
      system.file(
        "extdata", "data", "example-includes.csv",
        package = "wheretowork"
      ),
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
      system.file(
        "extdata", "data", "example-weights.csv",
        package = "wheretowork"
      ),
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
          "extdata", "data", "Clements-Checklist-v2019-August-2019.xlsx",
          package = "wheretowork"
        ),
        sheet = 1
      )
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
      regmatches(d$family, gregexpr("\\(.*?\\)", d$family))
    )
  # rename columns for output
  d <- stats::setNames(d, c("feature", "theme"))
  # return result
  d
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
    assertthat::noNA(x)
  )
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

#' Wrap text
#'
#' Wrap text for displaying in the application.
#'
#' @param x `character` vector
#'
#' @param width `integer` width. Defaults to 20.
#'
#' @return `character` vector
#'
#' @noRd
wrap_text <- function(x, width = 20)
  vapply(x, FUN.VALUE = character(1), function(x) {
  paste(
    stringi::stri_wrap(x[[1]], width = width, whitespace_only = FALSE),
    collapse = "\n"
  )
})


# copied from prioritizr::internal_eval_rare_richness_importance
prioritizr_internal_eval_rare_richness_importance <- function(x, indices,
                                                              rescale) {
  assertthat::assert_that(
    inherits(x, "ConservationProblem"),
    prioritizr::number_of_zones(x) == 1,
    is.integer(indices), length(indices) > 0,
    assertthat::is.flag(rescale))
  # calculate rarity weighted richness for each selected planning unit
  rs <- x$feature_abundances_in_total_units()
  m <- matrix(apply(x$data$rij_matrix[[1]], 1, max, na.rm = TRUE),
              nrow = nrow(rs), ncol = length(indices), byrow = FALSE)
  out <- x$data$rij_matrix[[1]][, indices, drop = FALSE]
  ## account for divide by zero issues result in NaNs
  out <- (out / m)
  out[!is.finite(out)] <- 0
  ## account for divide by zero issues result in NaNs
  out <- out / rs[, rep.int(1, ncol(out)), drop = FALSE]
  out[!is.finite(out)] <- 0
  out <- colSums(out)
  # rescale values if specified
  if (rescale) {
    rescale_ind <- is.finite(out) & (abs(out) > 1e-10)
    out[rescale_ind] <- scales::rescale(out[rescale_ind], to = c(0.01, 1))
  }
  # return result
  out
}
