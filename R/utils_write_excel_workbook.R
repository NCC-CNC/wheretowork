#' @include internal.R
NULL

#' Write Excel workbook
#'
#' Save a table to disk as an Excel Spreadsheet.
#'
#' @param x `data.frame` object (or `list` of `data.frame` objects) to save to
#' disk.
#'
#' @param path `character` file path to save data.
#'
#' @details
#' This function is similar to the [openxlsx::write.xlsx] function.
#' The main difference is that it formats cells columns containing `numeric`
#' values (and `numeric`-like values) using the `"NUMBER"` format, and the
#' remaining cells as `"TEXT"` format.
#'
#' @return Invisible `TRUE` indicating success.
#'
#' @examples
#' # create file path to save data
#' f <- tempfile(fileext = ".xlsx")
#'
#' # load example dataset to save to disk
#' data(iris)
#' d <- iris
#'
#' # save dataset to disk
#' write_excel_workbook(d, f)
#' @export
write_excel_workbook <- function(x, path) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, c("list", "data.frame")),
    assertthat::is.string(path),
    assertthat::noNA(path)
  )
  if (inherits(x, "data.frame")) {
    x <- list(x)
  }
  assertthat::assert_that(all_list_elements_inherit(x, "data.frame"))

  # set default worksheet names if needed
  if (is.null(names(x))) {
    names(x) <- paste("Sheet", seq_along(x))
  }

  # create workbook
  wb <- openxlsx::createWorkbook("data")

  # set style for numbers
  number_style <- openxlsx::createStyle(numFmt = "NUMBER")
  text_style <- openxlsx::createStyle(numFmt = "TEXT")

  # save data to sheets
  for (i in seq_along(x)) {
    # generate sheet names
    n <- names(x)[[i]]
    # create worksheet
    openxlsx::addWorksheet(wb, n)
    # save data
    openxlsx::writeDataTable(wb, n, x[[i]])
    # style columns
    ## determine columns and rows with data
    cols <- vapply(
      x[[i]], FUN.VALUE = logical(1), USE.NAMES = FALSE, function(x) {
        if (is.numeric(x)) return(TRUE)
        z <- suppressWarnings(as.numeric(trimws(as.character(x))))
        return(all(!is.na(z)))
    })

    rows <- 1 + seq_len(nrow(x[[1]]))
    ## style column headers
    openxlsx::addStyle(
      wb, n, style = text_style,
      rows = 1, cols = seq_along(cols),
      gridExpand = TRUE
    )
    ## style number values
    if (any(cols)) {
      openxlsx::addStyle(
        wb, n, style = number_style,
        rows = rows, cols = which(cols),
        gridExpand = TRUE
      )
    }
    ## style text values
    if (any(!cols)) {
      openxlsx::addStyle(
        wb, n, style = text_style,
        rows = rows, cols = which(!cols),
        gridExpand = TRUE
      )
    }
  }

  # save workbook
  openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE)

  # return success
  invisible(TRUE)
}
