#' @include internal.R
NULL

#' Provenance class
#'
#' Definition for the Provenance class.
#'
#' @seealso [new_provenance()].
Provenance <- R6::R6Class(
  "Provenance",
  public = list(

    #' @field name `character` value.
    name = NA_character_,

    #' @field icon `character` value
    icon = NA_character_,

    #' @field color `character` value.
    color = NA_character_,

    #' @field description `character` value
    description = NA_character_,

    #' @description
    #' Create a Provenance object.
    #' @param name `character` value.
    #' @param icon `numeric` value
    #' @param color `character` value.
    #' @param description `numeric` value.
    #' @return A Provenance object.
    initialize = function(name, icon, color, description) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        assertthat::is.string(icon),
        assertthat::noNA(icon),
        assertthat::is.string(color),
        assertthat::noNA(color),
        assertthat::is.string(description),
        assertthat::noNA(description)
      )
      self$name <- name
      self$icon <- icon
      self$color <- color
      self$description <- description
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message(self$repr())
      invisible(self)
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @param ... not used.
    #' @return `character` value.
    repr = function(...) {
      self$name
    },

    #' @description
    #' Get widget data.
    #' @return `list` object.
    get_widget_data = function() {
      list(
        name = self$name,
        icon = self$icon,
        color = self$color,
        description = self$description
      )
    },

    #' @description
    #' Get widget data.
    #' @return `character` value.
    export = function() {
      self$name
    }

  )
)

#' New Provenance
#'
#' Create a new [Provenance] object.
#'
#' @param name `character` name of provenance source.
#'
#' @param icon `character` name of icon to depict the soruce.
#'
#' @param color `character` value containing the color for the icon.
#'
#' @param description `character` value  containing a description of the
#'   provenance.
#'
#' @return A [Provenance] object.
#'
#' @examples
#' # create a provenance
#' x <- new_provenance(
#'   name = "Global", icon = "globe", color = "#123456",
#'   description = "Data is global."
#' )
#'
#' # print object
#' print(x)
#' @export
new_provenance <- function(name, icon, color, description) {
  Provenance$new(
    name = name,
    icon = icon,
    color = color,
    description = description
  )
}

#' New provenance from source
#'
#' Create a new [Provenance] object from a recognized source.
#'
#' @param source `character` provenance type.
#' Available options include `"national"`, `"regional"`, or `"missing"`.
#'
#' @inherit new_provenance return
#'
#' @examples
#' # create a provenance
#' x <- new_provenance_from_source("missing")
#'
#' # print object
#' print(x)
#'
#' @export
new_provenance_from_source <- function(source) {
  # assert argument is valid
  assertthat::assert_that(
    assertthat::is.string(source),
    assertthat::noNA(source))
  assertthat::assert_that(source %in% c("national", "regional", "missing"))
  # create object
  if (identical(source, "national")) {
    out <- new_provenance(
      name = source,
      icon = "canadian-maple-leaf",
      color = "#EF3340",
      description = "This contains National data"
    )
  } else if (identical(source, "regional")) {
    out <- new_provenance(
      name = source,
      icon = "certificate",
      color = "#43a2ca",
      description = "This contains Regional data"
    )
  } else {
    out <- new_provenance(
      name = source,
      icon = "question",
      color = "#7F7F7F",
      description = "This contains data of unknown provenance"
    )
  }
  # return result
  out
}
