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
#' # obtain colors from the plasma palette
#' color_palette("plasma", NULL)
#'
#' # obtain colors from a random palette
#' color_palette("random", 20)
#' @export
color_palette <- function(x, n = NULL) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x))
  if (!is.null(n)) {
    assertthat::assert_that(
      assertthat::is.count(n),
      assertthat::noNA(n))
  }
  if (identical(x, "random")) {
    assertthat::assert_that(
      assertthat::is.count(n),
      assertthat::noNA(n),
      msg = paste0(
        "the argument to \"n\" must be an integer when using a ",
        "\"random\" palette"))
    x <- paste(sample(rownames(RColorBrewer::brewer.pal.info)), collapse = ";")
  }

  # extract color brewer palette data
  bp <- as.data.frame(RColorBrewer::brewer.pal.info)

  # identify palettes
  pals <- strsplit(x, ";", fixed = TRUE)[[1]]

  # create vector to store number of colors
  cols <- c()

  # generate colors
  for (pal in pals) {
    ## determine which palette to use
    if (pal %in% rownames(bp)) {
      # determine number of colors
      if (is.null(n)) {
        curr_n <- bp$maxcolors[[which(rownames(bp) == pal)]]
      } else {
        curr_n <-
          min(bp$maxcolors[[which(rownames(bp) == pal)]], n - length(cols))
      }
      # generate colors
      new_cols <- RColorBrewer::brewer.pal(n = curr_n, name = pal)
    } else {
      # determine number of colors
      if (is.null(n)) {
        curr_n <- 20
      } else {
        curr_n <- n - length(cols)
      }
      # generate colors
      new_cols <- viridisLite::viridis(n = curr_n, option = pal)
    }
    # store olors
    cols <- c(cols, new_cols)
  }

  # return result
  cols
}
