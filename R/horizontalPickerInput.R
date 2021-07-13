#' @include internal.R
NULL

#' Horizontal picker input
#'
#' Create a horizontal [shinyWidgets::pickerInput].
#'
#' @param ... arguments passed to [shinyWidgets::pickerInput].
#'
#' @return  A select list control that can be added to a UI definition.
#'
#' @export
horizontalPickerInput <- function(...) {
  # create select input
  w <- shinyWidgets::pickerInput(..., width = "fit", inline = TRUE)

  # # add style class to select input
  # w$children[[2]]$attribs$class <-
  #   paste(w$children[[2]]$attribs$class, "form-control-sm")

  # change from-group to input-group
  w$attribs[[1]] <- "input-group shiny-input-container"

  # update child label
  w$children[[1]] <-
    structure(
      class = "shiny.tag",
      list(
        name = "span",
        attribs = list(class = "input-group-addon"),
        children = list(chr = list(...)$label)
      )
    )

  # return result
  w
}
