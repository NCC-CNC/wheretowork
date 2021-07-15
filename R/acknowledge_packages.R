#' @include internal.R
NULL

#' Acknowledge R packages.
#'
#' This function creates a HTML element for acknowledging R packages.
#'
#' @param x `character` names of package.
#'
#' @param prefix `character` prefix text.
#'  Defaults to `"We thank: "`.
#'
#' @param suffix `character` suffix text.
#'  Defaults to `" R packages."`.
#'
#' @return `shiny.tag` object.
#'
#' @export
acknowledge_packages <- function(
  x, prefix = "We thank the ", suffix = " R packages.") {
  # assert valid argument
  assertthat::assert_that(
    ## x
    is.character(x),
    assertthat::noNA(x),
    length(x) >= 1,
    ## prefix
    assertthat::is.string(prefix),
    assertthat::noNA(prefix),
    ## suffix
    assertthat::is.string(suffix),
    assertthat::noNA(suffix)
  )

  # generate acknowledgments
  out <- lapply(x, function(x) list(acknowledge_package(x), ", "))
  out <- unlist(out, recursive = FALSE, use.names = FALSE)
  out <- out[-length(out)]

  # create argument
  arg <- append(list(class = "text-justify", prefix), out)
  arg <- append(arg, list(suffix, .noWS = c("after-begin", "before-end")))

  # return result
  do.call(htmltools::tags$p, arg)
}


#' Acknowledge an R package.
#'
#' This function creates a HTML element for acknowledging a single R package.
#'
#' @param x `character` names of package.
#'
#' @return `shiny.tag` object.
#'
#' @noRd
acknowledge_package <- function(x) {
  # assert valid argument
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x)
  )

  # extract url for link
  pkg_url <- packageDescription(x)$URL[[1]]
  if (is.null(pkg_url)) {
    # if no URL, then link to CRAN
    href_url <- paste0("https://CRAN.R-project.org/package=", x)
  } else if (grepl(",", pkg_url)) {
    # if multiple URLs, then extract first
    href_url <- strsplit(pkg_url, ",")[[1]][[1]]
  } else {
    # if single URL, then use that
    href_url <- pkg_url
  }

  # return HTML element
  htmltools::tags$a(
    href = href_url,
    target = "_blank",
    x,
    .noWS = "outside"
  )
}
