#' @include internal.R class_Feature.R
NULL

#' Theme class
#'
#' Definition for the Theme class.
#'
#' @seealso [new_theme()].
Theme <- R6::R6Class(
  "Theme",
  public = list(

    #' @field id `character` value.
    id = NA_character_,

    #' @field name `character` value.
    name = NA_character_,

    #' @field feature `list` of [Feature] objects.
    feature = list(),

    #' @field feature_order `numeric` value.
    feature_order = NA_real_,

    #' @description
    #' Create a Theme object.
    #' @param id `character` value.
    #' @param name `character` value.
    #' @param feature `list` of [Feature] objects.
    #' @param feature_order `numeric` vector.
    #' @return A new Theme object.
    initialize = function(id, name, feature, feature_order) {
      ### assert that arguments are valid
      assertthat::assert_that(
        #### id
        assertthat::is.string(id),
        assertthat::noNA(id),
        #### name
        assertthat::is.string(name),
        assertthat::noNA(name),
        #### feature
        is.list(feature),
        all_list_elements_inherit(feature, "Feature"),
        #### feature_order
        is.numeric(feature_order),
        assertthat::noNA(feature_order),
        length(feature_order) == length(feature),
        identical(anyDuplicated(feature_order), 0L)
      )
      ## assert all feature have ame units
      assertthat::assert_that(
        n_distinct(
          vapply(
            feature,
            FUN.VALUE = character(1),
            function(x) x$variable$units
          )
        ) == 1,
        msg = "argument to `feature` contains elements with different units"
      )
      ## set fields
      self$id <- id
      self$name <- name
      self$feature <- feature
      self$feature_order <- feature_order
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      po <- order(self$get_feature_order(), decreasing = TRUE)
      message("Theme")
      message("  id:        ", self$id)
      message("  name:      ", self$name)
      message("  feature: ")
      for (x in vapply(self$feature[po], function(x) x$repr(), character(1))) {
        message("    ", gsub("\n", "\n    ", x))
      }
      invisible(self)
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @param ... not used.
    #' @return `character` value.
    repr = function(...) {
      po <- order(self$get_feature_order(), decreasing = TRUE)
      paste0(
        self$name,
        ":", nl(),
        paste(
          paste0(
            "  ",
            gsub(
              nl(), paste0(nl(), "  "),
              vapply(self$feature[po], function(x) x$repr(), character(1))
            ),
            collapse = nl()
          )
        )
      )
    },

    #' @description
    #' Get layer names.
    #' @return `character` vector.
    get_layer_name = function() {
      vapply(self$feature, `[[`, character(1), "name")
    },

    #' @description
    #' Get layer index values.
    #' @return `character` vector.
    get_layer_index = function() {
      vapply(
        self$feature,
        FUN.VALUE = character(1), function(x) x$variable$index
      )
    },

    #' @description
    #' Get feature identifiers.
    #' @return `character` vector with identifier(s).
    get_feature_id = function() {
      vapply(self$feature, `[[`, character(1), "id")
    },

    #' @description
    #' Get feature names.
    #' @return `character` vector with identifier(s).
    get_feature_name = function() {
      vapply(self$feature, `[[`, character(1), "name")
    },

    #' @description
    #' Get feature current.
    #' @return `numeric` vector with value(s).
    get_feature_current = function() {
      vapply(self$feature, `[[`, numeric(1), "current")
    },

    #' @description
    #' Get feature limit.
    #' @return `numeric` vector with value(s).
    get_feature_limit = function() {
      vapply(self$feature, `[[`, numeric(1), "limit_goal")
    },

    #' @description
    #' Get feature current.
    #' @return `numeric` vector with value(s).
    get_feature_total = function() {
      vapply(self$feature, FUN.VALUE = numeric(1), function(x) x$variable$total)
    },

    #' @description
    #' Set visible value for all features.
    #' @return `logical` value.
    get_visible = function() {
      any(self$get_feature_visible())
    },


    #' @description
    #' Get feature visible values.
    #' @return `logical` vector with status value(s).
    get_feature_visible = function() {
      vapply(self$feature, `[[`, logical(1), "visible")
    },

    #' @description
    #' Get feature status values.
    #' @return `logical` vector with status value(s).
    get_feature_status = function() {
      vapply(self$feature, `[[`, logical(1), "status")
    },

    #' @description
    #' Get feature goal values.
    #' @return `numeric` vector with goal value(s).
    get_feature_goal = function() {
      vapply(self$feature, `[[`, numeric(1), "goal")
    },

    #' @description
    #' Set relative order for displaying features on a map.
    get_feature_order = function() {
      self$feature_order
    },

    #' @description
    #' Set relative order for displaying features on a map.
    #' @param value `numeric` vector of new orders.
    set_feature_order = function(value) {
      if (is.list(value)) {
        value <- unlist(value, recursive = TRUE, use.names = TRUE)
      }
      assertthat::assert_that(
        is.numeric(value),
        assertthat::noNA(value),
        length(value) == length(self$feature),
        identical(anyDuplicated(value), 0L)
      )
      self$feature_order <- value
      invisible(self)
    },

    #' @description
    #' Get setting.
    #' @param name `character` setting name.
    #' Available options are `"feature_status"`, `"feature_goal"`,
    #' `"feature_visible"`, `"visible"`, or `"feature_order"`.
    #' @return Value.
    get_setting = function(name) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c(
          "feature_status", "feature_goal", "feature_visible",
          "feature_order", "feature_current", "visible"
        )
      )
      if (identical(name, "feature_status")) {
        out <- self$get_feature_status()
      } else if (identical(name, "feature_goal")) {
        out <- self$get_feature_goal()
      } else if (identical(name, "feature_visible")) {
        out <- self$get_feature_visible()
      } else if (identical(name, "feature_order")) {
        out <- self$get_feature_order()
      } else if (identical(name, "feature_current")) {
        out <- self$get_feature_current()
      } else if (identical(name, "visible")) {
        out <- self$get_visible()
      } else {
        stop(paste0("\"", name, "\" is not a setting"))
      }
      out
    },

    #' @description
    #' Set visible value for all features.
    #' @param value `logical` value.
    set_visible = function(value) {
      assertthat::assert_that(
        assertthat::is.flag(value),
        assertthat::noNA(value)
      )
      self$set_feature_visible(rep(value, length(self$feature)))
      invisible(self)
    },

    #' @description
    #' Set feature visible values.
    #' @param value `logical` vector containing a value for each feature.
    #'   A `list` of `logical` values can also be supplied.
    set_feature_visible = function(value) {
      if (is.list(value)) {
        value <- unlist(value, recursive = TRUE, use.names = TRUE)
      }
      assertthat::assert_that(
        is.logical(value),
        assertthat::noNA(value),
        length(value) == length(self$feature)
      )
      for (i in seq_along(value)) {
        self$feature[[i]]$set_visible(value[[i]])
      }
      invisible(self)
    },

    #' @description
    #' Set feature status values.
    #' @param value `logical` vector containing a value for each feature.
    #'   A `list` of `logical` values can also be supplied.
    set_feature_status = function(value) {
      if (is.list(value)) {
        value <- unlist(value, recursive = TRUE, use.names = TRUE)
      }
      assertthat::assert_that(
        is.logical(value),
        assertthat::noNA(value),
        length(value) == length(self$feature)
      )
      for (i in seq_along(value)) {
        self$feature[[i]]$set_status(value[[i]])
      }
      invisible(self)
    },

    #' @description
    #' Set feature goal values.
    #' @param value `numeric` vector containing a value for each feature.
    #'   A `list` of `numeric` values can also be supplied.
    set_feature_goal = function(value) {
      if (is.list(value)) {
        value <- unlist(value, recursive = TRUE, use.names = TRUE)
      }
      assertthat::assert_that(
        is.numeric(value),
        assertthat::noNA(value),
        length(value) == length(self$feature)
      )
      for (i in seq_along(value)) {
        self$feature[[i]]$set_goal(value[[i]])
      }
      invisible(self)
    },

    #' @description
    #' Set feature current values.
    #' @param value `numeric` vector containing a value for each feature.
    #'   A `list` of `numeric` values can also be supplied.
    set_feature_current = function(value) {
      if (is.list(value)) {
        value <- unlist(value, recursive = TRUE, use.names = TRUE)
      }
      assertthat::assert_that(
        is.numeric(value),
        assertthat::noNA(value),
        length(value) == length(self$feature)
      )
      for (i in seq_along(value)) {
        self$feature[[i]]$set_current(value[[i]])
      }
      invisible(self)
    },

    #' @description
    #' Set setting.
    #' @param name `character` setting name.
    #' Available options are `"feature_status"`, `"feature_goal"`,
    #' `"feature_visible"`, `"visible"`, `"feature_order"`,
    #' or `"feature_current"`.
    #' @param value vector containing a value for each feature.
    set_setting = function(name, value) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in%
          c(
            "feature_status", "feature_goal", "feature_visible",
            "feature_order", "feature_current", "visible"
          )
      )
      if (identical(name, "feature_status")) {
        self$set_feature_status(value)
      } else if (identical(name, "feature_goal")) {
        self$set_feature_goal(value)
      } else if (identical(name, "feature_visible")) {
        self$set_feature_visible(value)
      } else if (identical(name, "feature_order")) {
        self$set_feature_order(value)
      } else if (identical(name, "feature_current")) {
        self$set_feature_current(value)
      } else if (identical(name, "visible")) {
        self$set_visible(value)
      } else {
        stop(paste0("\"", name, "\" is not a setting"))
      }
      invisible(self)
    },


    #' @description
    #' Get data for displaying the theme in a [solutionSettings()] widget.
    #' @return `list` with widget data.
    get_solution_settings_widget_data = function() {
      list(
        id = self$id,
        name = self$name,
        feature_name = vapply(
          self$feature, `[[`, character(1), "name"
        ),
        feature_id = vapply(
          self$feature, `[[`, character(1), "id"
        ),
        feature_status = vapply(
            self$feature, `[[`, logical(1), "status"
          ),
        feature_total_amount = vapply(
          self$feature, function(x) x$variable$total, numeric(1)
        ),
        feature_current_held = vapply(
            self$feature, `[[`, numeric(1), "current"
          ),
        feature_min_goal = vapply(
          self$feature, `[[`, numeric(1), "min_goal"
        ),
        feature_max_goal = vapply(
          self$feature, `[[`, numeric(1), "max_goal"
        ),
        feature_goal = vapply(
          self$feature, `[[`, numeric(1), "goal"
        ),
        feature_limit_goal = vapply(
          self$feature, `[[`, numeric(1), "limit_goal"
        ),
        feature_step_goal = vapply(
          self$feature, `[[`, numeric(1), "step_goal"
        ),
        units = self$feature[[1]]$variable$units
      )
    },

    #' @description
    #' Get data for displaying the theme in a [mapManager()] widget.
    #' @return `list` with widget data.
    get_map_manager_widget_data = function() {
      list(
        id = self$id,
        name = self$name,
        feature_name =
          vapply(self$feature, `[[`, character(1), "name"),
        feature_id =
          vapply(self$feature, `[[`, character(1), "id"),
        feature_visible =
          vapply(self$feature, `[[`, logical(1), "visible"),
        feature_legend =
          lapply(self$feature, function(x) x$variable$legend$get_widget_data()),
        units = self$feature[[1]]$variable$units,
        type = "theme"
      )
    },

    #' @description
    #' Render on map.
    #' @param x [leaflet::leaflet()] object.
    #' @param zindex `numeric` z-index for ordering.
    #' @return [leaflet::leaflet()] object.
    render_on_map = function(x, zindex) {
      # extract feature data
      fid <- self$get_feature_id()
      fo <- self$get_feature_order() + zindex
      fv <- self$get_feature_visible()
      # add feature data
      for (i in seq_along(self$feature)) {
        x <- self$feature[[i]]$variable$render(x, fid[i], fo[i], fv[i])
      }
      # return result
      x
    },

    #' @description
    #' Render on map.
    #' @param x [leaflet::leafletProxy()] object.
    #' @param zindex `numeric` z-index for ordering.
    #' @return [leaflet::leafletProxy()] object.
    update_on_map = function(x, zindex) {
      # extract feature data
      fid <- self$get_feature_id()
      fo <- self$get_feature_order() + zindex
      fv <- self$get_feature_visible()
      # add feature data
      for (i in seq_along(self$feature)) {
        x <- self$feature[[i]]$variable$update_render(x, fid[i], fo[i], fv[i])
      }
      # return result
      x
    },

    #' @description
    #' Export settings
    #' @return `list` object.
    export = function() {
      list(
        name = self$name,
        feature = lapply(self$feature, function(x) x$export())
      )
    }

  )
)

#' New theme
#'
#' Create a new [Theme] object.
#'
#' @param name `character` Name to display.
#'
#' @param feature `list` of [Feature] objects.
#'
#' @param feature_order `numeric` Relative order for displaying each feature
#'  on a map. Defaults to a reverse sequence of integer values.
#'
#' @param id `character` unique identifier.
#'   Defaults to a random identifier ([uuid::UUIDgenerate()]).
#'
#' @return A [Theme] object.
#'
#' @examples
#' # find data path
#' # find data file paths
#' f1 <- system.file(
#'   "extdata", "projects", "sim_raster", "sim_raster_spatial.tif",
#'   package = "wheretowork"
#' )
#' f2 <- system.file(
#'   "extdata",  "projects", "sim_raster", "sim_raster_attribute.csv.gz",
#'   package = "wheretowork"
#' )
#' f3 <- system.file(
#'   "extdata",  "projects", "sim_raster", "sim_raster_boundary.csv.gz",
#'   package = "wheretowork"
#' )
#'
#' # create new dataset
#' d <- new_dataset(f1, f2, f3)
#'
#' # create new variable
#' v <- new_variable_from_auto(d, index = 1)
#'
#' # create new feature
#' f <- new_feature(name = "Intact Alvar map", variable = v)
#'
#' # create a theme using the feature
#' x <- new_theme(name = "Intact Alvar", feature = f)
#'
#' # print object
#' print(x)
#' @export
new_theme <- function(name,
                     feature,
                     feature_order = as.double(rev(seq_along(feature))),
                     id = uuid::UUIDgenerate()) {
  # put feature in a list if needed
  if (inherits(feature, "Feature")) {
    feature <- list(feature)
  }
  # return new theme
  Theme$new(
    id = id,
    name = name,
    feature = feature,
    feature_order = feature_order
  )
}