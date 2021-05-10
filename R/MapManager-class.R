#' @include internal.R SingleTheme-class.R MultiTheme-class.R Weight-class.R
NULL

#' Map manager class
#'
#'
#' Definition for the MapManager class.
#'

MapManager <- R6::R6Class(
  "MapManager",
  public = list(

    #' @field layers `list` of Layer objects.
    layers = list()


  )
)
