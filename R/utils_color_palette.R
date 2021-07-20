#' @include internal.R
NULL

#' Color palette
#'
#' Generate colors using the name of a color palette.
#'
#' @param x `character` name of the color palette
#' (see Details for available options).
#'
#' @param n `integer` number of colors required.
#'  A `NULL` argument can also be used to obtain a default number of colors.
#'  If a `NULL` argument is supplied and the palette has a limited number of
#'  colors, then the maximum number of colors is returned.
#'  Otherwise, if a `NULL` argument is supplied and the palette does not have
#'  a limited number of colors, then a 20 colors are returned.
#'  The default argument is `NULL`.
#'
#' @details
#' This function can be used to generate colors using palettes provided by
#' the \pkg{RColorBrewer} and \pkg{viridisLite} packages. If colors are
#' from multiple palettes, they can be supplied as a single `character`
#' object delimited by semicolon (`";"`) characters.
#'
#' Specifically, available palettes include:
#' * `"viridis"` (see [viridisLite::viridis])
#' * `"magma"` (see [viridisLite::magma])
#' * `"plasma"` (see [viridisLite::plasma])
#' * `"inferno"` (see [viridisLite::inferno])
#' * `"cividis"` (see [viridisLite::cividis])
#' * `"mako"` (see [viridisLite::mako])
#' * `"rocket"` (see [viridisLite::rocket])
#' * `"turbo"` (see [viridisLite::turbo])
#' * `"BrBG`" (see [RColorBrewer::brewer.pal])
#' * `"PiYG`" (see [RColorBrewer::brewer.pal])
#' * `"PRGn`" (see [RColorBrewer::brewer.pal])
#' * `"PuOr`" (see [RColorBrewer::brewer.pal])
#' * `"RdBu`" (see [RColorBrewer::brewer.pal])
#' * `"RdGy`" (see [RColorBrewer::brewer.pal])
#' * `"RdYlBu`" (see [RColorBrewer::brewer.pal])
#' * `"RdYlGn`" (see [RColorBrewer::brewer.pal])
#' * `"Spectral`" (see [RColorBrewer::brewer.pal])
#' * `"Accent`" (see [RColorBrewer::brewer.pal])
#' * `"Dark2`" (see [RColorBrewer::brewer.pal])
#' * `"Paired`" (see [RColorBrewer::brewer.pal])
#' * `"Pastel1`" (see [RColorBrewer::brewer.pal])
#' * `"Pastel2`" (see [RColorBrewer::brewer.pal])
#' * `"Set1`" (see [RColorBrewer::brewer.pal])
#' * `"Set2`" (see [RColorBrewer::brewer.pal])
#' * `"Set3`" (see [RColorBrewer::brewer.pal])
#' * `"Blues`" (see [RColorBrewer::brewer.pal])
#' * `"BuGn`" (see [RColorBrewer::brewer.pal])
#' * `"BuPu`" (see [RColorBrewer::brewer.pal])
#' * `"GnBu`" (see [RColorBrewer::brewer.pal])
#' * `"Greens`" (see [RColorBrewer::brewer.pal])
#' * `"Greys`" (see [RColorBrewer::brewer.pal])
#' * `"Oranges`" (see [RColorBrewer::brewer.pal])
#' * `"OrRd`" (see [RColorBrewer::brewer.pal])
#' * `"PuBu`" (see [RColorBrewer::brewer.pal])
#' * `"PuBuGn`" (see [RColorBrewer::brewer.pal])
#' * `"PuRd`" (see [RColorBrewer::brewer.pal])
#' * `"Purples`" (see [RColorBrewer::brewer.pal])
#' * `"RdPu`" (see [RColorBrewer::brewer.pal])
#' * `"Reds`" (see [RColorBrewer::brewer.pal])
#' * `"YlGn`" (see [RColorBrewer::brewer.pal])
#' * `"YlGnBu`" (see [RColorBrewer::brewer.pal])
#' * `"YlOrBr`" (see [RColorBrewer::brewer.pal])
#' * `"YlOrRd`" (see [RColorBrewer::brewer.pal])
#' * `"random"` (the above palettes are randomly used to generate colors)
#'
#' @examples
#' # obtain 5 colors from the Greens palette
#' color_palette("Greens", 5)
#'
#' # obtain all colors from the Greens palette
#' color_palette("Greens", NULL)
#'
#' # obtain 15 colors using the Set1 and Pastel1 palettes
#' color_palette("Set1;Pastel1", 15)
#'
#' # obtain all colors from the plasma palette
#' color_palette("plasma", NULL)
#'
#' # obtain all colors from a random palette
#' color_palette("random", NULL)
#'
#' # obtain 5 colors from random palette(s)
#' color_palette("random", 5)
#' @export
color_palette <- function(x, n = NULL) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x)
  )
  if (!is.null(n)) {
    assertthat::assert_that(
      assertthat::is.count(n),
      assertthat::noNA(n)
    )
  }
  if (identical(x, "random")) {
    if (is.null(n)) {
      x <- sample(rownames(RColorBrewer::brewer.pal.info), 1)
    } else {
      x <-
        paste(sample(rownames(RColorBrewer::brewer.pal.info)), collapse = ";")
    }
  }

  # identify palettes
  pals <- strsplit(x, ";", fixed = TRUE)[[1]]

  # generate colors
  if (is.null(n)) {
    out <- do.call(c, lapply(pals, color_palette_all_colors))
  } else {
    out <- c()
    i <- 1
    while (length(out) < n) {
      out <- c(out, color_palette_n_colors(pals[i], n - length(out)))
      i <- i + 1
    }
  }

  # return result
  out
}

#' Extract all colors from palette
#'
#' Extract all colors from a color palette.
#'
#' @param x `character` name of color palette.
#'
#' @return `character` vector of colors.
#'
#' @noRd
color_palette_all_colors <- function(x) {
  # assert argument is valid
  assertthat::assert_that(assertthat::is.string(x))
  # extract color brewer palette data
  bp <- as.data.frame(RColorBrewer::brewer.pal.info)
  # generate colors
  if (x %in% rownames(bp)) {
    n <- bp$maxcolors[[which(rownames(bp) == x)]]
    suppressWarnings({
      out <- RColorBrewer::brewer.pal(n = n, name = x)
    })
  } else {
    n <- 5
    out <- viridisLite::viridis(n = 5, option = x)
  }
  # return result
  out
}

#' Extract a number of colors from a palette
#'
#' Extract a number colors from a color palette.
#'
#' @param x `character` color palette.
#'
#' @return `character` vector of colors.
#'
#' @noRd
color_palette_n_colors <- function(x, n) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::is.count(n)
  )
  # extract color brewer palette data
  bp <- as.data.frame(RColorBrewer::brewer.pal.info)
  # generate colors
  if (x %in% rownames(bp)) {
    n <- min(n, bp$maxcolors[[which(rownames(bp) == x)]])
    suppressWarnings({
      out <- RColorBrewer::brewer.pal(n = n, name = x)
    })
  } else {
    out <- viridisLite::viridis(n = n, option = x)
  }
  # return result
  out[seq_len(n)]
}
