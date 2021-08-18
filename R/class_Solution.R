#' @include internal.R
NULL


#' Solution class
#'
#' Definition for the Solution class.
#'
#' @seealso [new_solution()].
Solution <- R6::R6Class(
  "Solution",
  public = list(

    #' @field id `character` identifier.
    id = NA_character_,

    #' @field name `character` name of solution.
    name = NA_character_,

    #' @field visible `logical` value.
    visible = NA,

    #' @field variable [Variable] object.
    variable = NULL,

    #' @field parameters `list` of [Parameter] objects
    parameters = NULL,

    #' @field statistics `list` of [Statistic] objects
    statistics = NULL,

    #' @field theme_results `list` of [ThemeResults] objects.
    theme_results = NULL,

    #' @field weight_results `list` of [WeightResults] objects.
    weight_results = NULL,

    #' @field include_results `list` of [IncludeResults] objects.
    include_results = NULL,

    #' @description
    #' Create a Solution object.
    #' @param id `character` value.
    #' @param name `character` value.
    #' @param variable [Variable] object.
    #' @param visible `logical` value.
    #' @param parameters `list` of [Statistic] objects.
    #' @param statistics `list` of [Statistic] objects.
    #' @param theme_results `list` of [ThemeResults] objects.
    #' @param weight_results `list` of [WeightResults] objects.
    #' @param include_results `list` of [IncludeResults] objects.
    #' @return A new Solution object.
    initialize = function(id, name, variable, visible,
                          statistics,
                          parameters,
                          theme_results,
                          weight_results,
                          include_results) {
      # assert arguments are valid
      assertthat::assert_that(
        assertthat::is.string(id),
        assertthat::noNA(id),
        assertthat::is.string(name),
        assertthat::noNA(name),
        inherits(variable, "Variable"),
        assertthat::is.flag(visible),
        assertthat::noNA(visible),
        is.list(parameters),
        all_list_elements_inherit(parameters, "Parameter"),
        is.list(statistics),
        all_list_elements_inherit(statistics, "Statistic"),
        is.list(theme_results),
        all_list_elements_inherit(theme_results, "ThemeResults"),
        is.list(weight_results),
        all_list_elements_inherit(weight_results, "WeightResults"),
        is.list(include_results),
        all_list_elements_inherit(include_results, "IncludeResults")
      )
      # assign fields
      self$id <- id
      self$name <- name
      self$variable <- variable
      self$visible <- visible
      self$parameters <- parameters
      self$statistics <- statistics
      self$theme_results <- theme_results
      self$weight_results <- weight_results
      self$include_results <- include_results
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @param start `character` symbol used to start the setting list.
    #'   Defaults to `"["`.
    #' @param end `character` symbol used to start the setting list.
    #'   Defaults to `"]"`.
    #' @return `character` value.
    repr = function(start = "[", end = "]") {
      paste0(
        self$name,
        " ",
        start,
        paste(
          vapply(self$parameters, function(x) x$repr(), character(1)),
          collapse = ", "
        ),
        paste(
          vapply(self$statistics, function(x) x$repr(), character(1)),
          collapse = ", "
        ),
        end,
        nl(),
        "  variable: ", self$variable$repr()
      )
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Solution")
      message("  id:      ", self$id)
      message("  name:    ", self$name)
      invisible(self)
    },

    #' @description
    #' Get layer names.
    #' @return `character` vector.
    get_layer_name = function() {
      self$name
    },

    #' @description
    #' Get layer index values.
    #' @return `character` vector.
    get_layer_index = function() {
      self$variable$index
    },

    #' @description
    #' Get visible.
    #' @return `logical` value.
    get_visible = function() {
      self$visible
    },

    #' @description
    #' Get setting.
    #' @param name `character` setting name.
    #' Available options are `"visible"`.
    #' @return Value.
    get_setting = function(name) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c("visible")
      )
      if (identical(name, "visible")) {
        out <- self$get_visible()
      } else {
        stop(paste0("\"", name, "\" is not a setting"))
      }
      out
    },

    #' @description
    #' Set visible.
    #' @param value `logical` new value.
    set_visible = function(value) {
      assertthat::assert_that(
        assertthat::is.flag(value),
        assertthat::noNA(value)
      )
      self$visible <- value
      invisible(self)
    },

    #' @description
    #' Get summary results.
    #' @return [tibble::tibble()] object.
    get_summary_results_data = function() {
      # compile data
      rd <- tibble::as_tibble(plyr::ldply(
         self$statistics, function(x) x$get_results_data()))
      pd <- tibble::as_tibble(plyr::ldply(
         self$parameters, function(x) x$get_results_data()))
      # prepare statistics data
      rd$type <- "statistic"
      rd$hide <- FALSE
      # prepare parameters data
      pd$type <- "setting"
      pd$proportion <- NA_real_
      pd$proportion[pd$units == "%"] <- pd$value[pd$units == "%"]
      pd$value[pd$units == "%"] <- NA_real_
      # combine data
      x <- dplyr::bind_rows(pd, rd)
      x$value_text <- dplyr::if_else(
        is.na(x$value), rep("", nrow(x)), paste(round(x$value, 2), x$units)
      )
      x$value_text <- dplyr::if_else(x$hide, "None specified", x$value_text)
      x$proportion_text <- round(x$proportion * 100, 2)
      x$proportion_text <- dplyr::if_else(
        is.na(x$proportion_text), "", as.character(x$proportion_text)
      )
      x$proportion_text <- dplyr::if_else(
        x$hide, "None specified", x$proportion_text
      )
      # return formatted table
      tibble::tibble(
        `Name` = x$name,
        `Type` = x$type,
        `Value (%)` = x$proportion_text,
        `Value (units)` = x$value_text
      )
    },

    #' @description
    #' Get theme results.
    #' @return [tibble::tibble()] object.
    get_theme_results_data = function() {
      # compile data
      x <- tibble::as_tibble(plyr::ldply(
        self$theme_results, function(x) x$get_results_data()
      ))
      # return formatted table
      tibble::tibble(
        Theme = x$name,
        Feature = x$feature_name,
        Status = dplyr::if_else(x$feature_status, "Enabled", "Disabled"),
        `Total (units)` = paste(
          round(x$feature_total_amount, 2), x$units
        ),
        `Current (%)` = round(x$feature_current_held * 100, 2),
        `Current (units)` = paste(
          round(x$feature_current_held * x$feature_total_amount, 2),
          x$units
        ),
        `Goal (%)` = round(x$feature_status * x$feature_goal * 100, 2),
        `Goal (units)` = paste(
          round(
            x$feature_status * x$feature_goal * x$feature_total_amount, 2
          ),
          x$units
        ),
        `Solution (%)` = round(x$feature_solution_held * 100, 2),
        `Solution (units)` = paste(
          round(x$feature_solution_held * x$feature_total_amount, 2),
          x$units
        ),
        `Met` = dplyr::case_when(
          !x$feature_status ~ "NA",
          (x$feature_solution_held >=
            (x$feature_status * x$feature_goal)) ~ "Yes",
          TRUE ~ "No"
        )
      )
    },

    #' @description
    #' Get weight results.
    #' @return [tibble::tibble()] object.
    get_weight_results_data = function() {
      # compile results
      if (length(self$weight_results) > 0) {
        ## if weights are present, then use result
        ### extract results
        x <- tibble::as_tibble(plyr::ldply(
          self$weight_results, function(x) x$get_results_data()
        ))
        ### format results
        out <- tibble::tibble(
          Weight = x$name,
          Status = dplyr::if_else(x$status, "Enabled", "Disabled"),
          Factor = round(x$factor, 2),
          `Total (units)` = paste(round(x$total, 2), x$units),
          `Current (%)` = round(x$current * 100, 2),
          `Current (units)` = paste(round(x$current * x$total, 2), x$units),
          `Solution (%)` = round(x$held * 100, 2),
          `Solution (units)` = paste(round(x$held * x$total, 2), x$units),
        )
      } else {
        ## if no weights are present, then use return
        out <- tibble::tibble(
          `Description` = "No weights specified"
        )
      }
      # return results
      out
    },

    #' @description
    #' Get weight results.
    #' @return [tibble::tibble()] object.
    get_include_results_data = function() {
      # compile results
      if (length(self$include_results) > 0) {
        ## if weights are present, then use result
        ### extract results
        x <- tibble::as_tibble(plyr::ldply(
          self$include_results, function(x) x$get_results_data()
        ))
        ### format results
        out <- tibble::tibble(
          Include = x$name,
          Status = dplyr::if_else(x$status, "Enabled", "Disabled"),
          `Total (units)` = paste(round(x$total, 2), x$units),
          `Solution (%)` = round(x$held * 100, 2),
          `Solution (units)` = paste(round(x$held * x$total, 2), x$units)
        )
      } else {
        ## if no weights are present, then use return
        out <- tibble::tibble(
          `Description` = "No includes specified"
        )
      }
      # return results
      out
    },

    #' @description
    #' Render summary results.
    #' @return [DT::datatable()] object.
    render_summary_results = function() {
      ## generate results
      x <- self$get_summary_results_data()
      ## define JS for button
      action_js <- htmlwidgets::JS(
        "function ( e, dt, node, config ) {",
        "  $('#summary_results_button')[0].click();",
        "}"
      )
      ## render table
      DT::datatable(
        x,
        rownames = FALSE, escape = TRUE,
        editable = FALSE, selection = "none",
        fillContainer = TRUE, extensions = "Buttons",
        options = list(
          ### align columns
          columnDefs = list(
            list(className = "dt-left", targets = 0:1),
            list(className = "dt-center", targets = c(2:3))
          ),
          ### disable paging
          paging = FALSE,
          scrollY = "calc(100vh - 295px)",
          scrollCollapse = TRUE,
          ### download button
          dom = "Bfrtip",
          buttons = list(
            list(
              extend = "collection",
              text = as.character(shiny::icon("file-download")),
              title = "Download spreadsheet",
              action = action_js
            )
          )
        ),
        container = htmltools::tags$table(
          class = "display",
          htmltools::tags$thead(
            htmltools::tags$tr(
              htmltools::tags$th(rowspan = 2, "Name"),
              htmltools::tags$th(rowspan = 2, "Type"),
              htmltools::tags$th(
                class = "dt-center", colspan = 2, "Value"
              )
            ),
            htmltools::tags$tr(
              lapply(c("(%)", "(units)"), htmltools::tags$th)
            )
          )
        )
      )
    },

    #' @description
    #' Render theme results.
    #' @return [DT::datatable()] object.
    render_theme_results = function() {
      ## generate results
      x <- self$get_theme_results_data()
      # replace met column values with icons
      x$Met <- unname(vapply(x$Met, FUN.VALUE = character(1), function(i) {
        if (identical(i, "Yes")) {
          out <- htmltools::tags$i(
            class = "fa fa-check-circle",
            style = "color: #5cb85c"
          )
        } else if (identical(i, "No")) {
          out <- htmltools::tags$i(
            class = "fa fa-times-circle",
            style = "color: #dc3545"
          )
        } else {
          out <- htmltools::tags$i(
            class = "fa fa-dot-circle",
            style = "color: #adb5bd"
          )
        }
        as.character(out)
      }))
      ## add in extra column
      x$space1 <- " "
      x$space2 <- " "
      x <- x[, c(seq_len(6), 12, c(7, 8), 13, c(9, 10), 11), drop = FALSE]
      ## define JS for button
      action_js <- htmlwidgets::JS(
        "function ( e, dt, node, config ) {",
        "  $('#theme_results_button')[0].click();",
        "}"
      )
      ## wrap text columns
      x[[1]] <- wrap_text(x[[1]])
      x[[2]] <- wrap_text(x[[2]])
      ## render table
      DT::datatable(
        x,
        rownames = FALSE, escape = FALSE,
        editable = FALSE, selection = "none",
        fillContainer = TRUE, extensions = "Buttons",
        options = list(
          ### align columns
          columnDefs = list(
            list(className = "dt-left", targets = 0:1),
            list(className = "dt-center", targets = 2:12),
            list(
              className = "spacer", "sortable" = FALSE,  targets = c(6, 9)
            )
          ),
          ### disable paging
          paging = FALSE,
          scrollY = "calc(100vh - 295px)",
          scrollCollapse = TRUE,
          ### download button
          dom = "Bfrtip",
          buttons = list(
            list(
              extend = "collection",
              text = as.character(shiny::icon("file-download")),
              title = "Download spreadsheet",
              action = action_js
            )
          )
        ),
        container = htmltools::tags$table(
          class = "display",
          htmltools::tags$thead(
            htmltools::tags$tr(
              htmltools::tags$th(rowspan = 2, "Theme"),
              htmltools::tags$th(rowspan = 2, "Feature"),
              htmltools::tags$th(rowspan = 2, "Status"),
              htmltools::tags$th(rowspan = 2, "Total (units)"),
              htmltools::tags$th(
                class = "dt-center", colspan = 2, "Current"
              ),
              htmltools::tags$th(rowspan = 2),
              htmltools::tags$th(
                class = "dt-center", colspan = 2, "Goal"
              ),
              htmltools::tags$th(rowspan = 2),
              htmltools::tags$th(
                class = "dt-center", colspan = 2, "Solution"
              ),
              htmltools::tags$th(rowspan = 2, "Met"),
            ),
            htmltools::tags$tr(
              lapply(rep(c("(%)", "(units)"), 3), htmltools::tags$th
              )
            )
          )
        )
      )
    },

    #' @description
    #' Render weight results.
    #' @return [DT::datatable()] object.
    render_weight_results = function() {
      # generate table
      x <- self$get_weight_results_data()
      # wrap text columns
      x[[1]] <- wrap_text(x[[1]])
      # add in extra column
      if (ncol(x) > 1) {
        x$space1 <- " "
        x <- x[, c(seq_len(6), 9, c(7, 8)), drop = FALSE]
      }
      # define JS for button
      action_js <- htmlwidgets::JS(
        "function ( e, dt, node, config ) {",
        "  $('#weight_results_button')[0].click();",
        "}"
      )
      # define container
      if (ncol(x) > 1) {
         container <- htmltools::tags$table(
          class = "display",
          htmltools::tags$thead(
            htmltools::tags$tr(
              htmltools::tags$th(rowspan = 2, "Weight"),
              htmltools::tags$th(rowspan = 2, "Status"),
              htmltools::tags$th(rowspan = 2, "Factor"),
              htmltools::tags$th(rowspan = 2, "Total (units)"),
              htmltools::tags$th(
                class = "dt-center", colspan = 2, "Current"
              ),
              htmltools::tags$th(rowspan = 2),
              htmltools::tags$th(
                class = "dt-center", colspan = 2, "Solution"
              )
            ),
            htmltools::tags$tr(
              lapply(rep(c("(%)", "(units)"), 2), htmltools::tags$th)
            )
          )
        )
      } else {
        container <- rlang::missing_arg()
      }
      # define columns
      if (ncol(x) > 1) {
        column_defs <- list(
          list(className = "dt-left", targets = 0),
          list(className = "dt-center", targets = 1:8),
          list(className = "spacer", "sortable" = FALSE, targets = 6)
        )
      } else {
        column_defs <- list(
          list(className = "dt-left", targets = 0)
        )
      }
      # render table
      DT::datatable(
        x,
        rownames = FALSE,
        escape = TRUE,
        editable = FALSE,
        selection = "none",
        fillContainer = TRUE,
        extensions = "Buttons",
        container = container,
        options = list(
          ## align columns
          columnDefs = column_defs,
          ## disable paging
          paging = FALSE,
          scrollY = "calc(100vh - 295px)",
          scrollY = "100%",
          scrollCollapse = TRUE,
          ## download button
          dom = "Bfrtip",
          buttons = list(
            list(
              extend = "collection",
              text = as.character(shiny::icon("file-download")),
              title = "Download spreadsheet",
              action = action_js
            )
          )
        )
      )
    },

    #' @description
    #' Render include results.
    #' @return [DT::datatable()] object.
    render_include_results = function() {
      # generate table
      x <- self$get_include_results_data()
      # wrap text columns
      x[[1]] <- wrap_text(x[[1]])
      # define JS for button
      action_js <- htmlwidgets::JS(
        "function ( e, dt, node, config ) {",
        "  $('#include_results_button')[0].click();",
        "}"
      )
      # define container
      if (ncol(x) > 1) {
         container <- htmltools::tags$table(
          class = "display",
          htmltools::tags$thead(
            htmltools::tags$tr(
              htmltools::tags$th(rowspan = 2, "Include"),
              htmltools::tags$th(rowspan = 2, "Status"),
              htmltools::tags$th(rowspan = 2, "Total (units)"),
              htmltools::tags$th(
                class = "dt-center", colspan = 2, "Solution"
              )
            ),
            htmltools::tags$tr(
              lapply(c("(%)", "(units)"), htmltools::tags$th)
            )
          )
        )
      } else {
        container <- rlang::missing_arg()
      }
      # define columns
      if (ncol(x) > 1) {
        column_defs <- list(
          list(className = "dt-left", targets = 0),
          list(className = "dt-center", targets = 1:4)
        )
      } else {
        column_defs <- list(
          list(className = "dt-left", targets = 0)
        )
      }
      # render table
      DT::datatable(
        x,
        rownames = FALSE,
        escape = TRUE,
        editable = FALSE,
        selection = "none",
        fillContainer = TRUE,
        extensions = "Buttons",
        container = container,
        options = list(
          ## align columns
          columnDefs = column_defs,
          ## disable paging
          paging = FALSE,
          scrollY = "calc(100vh - 295px)",
          scrollY = "100%",
          scrollCollapse = TRUE,
          ## download button
          dom = "Bfrtip",
          buttons = list(
            list(
              extend = "collection",
              text = as.character(shiny::icon("file-download")),
              title = "Download spreadsheet",
              action = action_js
            )
          )
        )
      )
    },

    #' @description
    #' Set setting.
    #' @param name `character` setting name.
    #' Available options are `"visible"``.
    #' @param value `ANY` new value.
    set_setting = function(name, value) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        name %in% c("visible")
      )
      if (identical(name, "visible")) {
        self$set_visible(value)
      } else {
        stop(paste0("\"", name, "\" is not a setting"))
      }
      invisible(self)
    },

    #' @description
    #' Get data for displaying the theme in a [solutionResults()] widget.
    #' @return `list` with widget data.
    get_solution_results_widget_data = function() {
      list(
        id = self$id,
        name = self$name,
        parameters = lapply(
          self$parameters,
          function(x) x$get_widget_data()
        ),
        statistics = lapply(
          self$statistics,
          function(x) x$get_widget_data()
        ),
        theme_results = lapply(
          self$theme_results,
          function(x) x$get_widget_data()
        ),
        weight_results = lapply(
          self$weight_results,
          function(x) x$get_widget_data()
        ),
        include_results = lapply(
          self$include_results,
          function(x) x$get_widget_data()
        ),
        solution_color = scales::alpha(last(self$variable$legend$colors), 1)
      )
    },

    #' @description
    #' Get data for displaying the theme in a [mapManager()] widget.
    #' @return `list` with widget data.
    get_map_manager_widget_data = function() {
      list(
        id = self$id,
        name = self$name,
        visible = self$visible,
        legend = self$variable$legend$get_widget_data(),
        units = self$variable$units,
        type = "solution"
      )
    },

    #' @description
    #' Render on map.
    #' @param x [leaflet::leaflet()] object.
    #' @param zindex `numeric` z-index for ordering.
    #' @return [leaflet::leaflet()] object.
    render_on_map = function(x, zindex) {
      self$variable$render(x, self$id, zindex, self$visible)
    },

    #' @description
    #' Render on map.
    #' @param x [leaflet::leafletProxy()] object.
    #' @param zindex `numeric` z-index for ordering.
    #' @return [leaflet::leafletProxy()] object.
    update_on_map = function(x, zindex) {
      self$variable$update_render(x, self$id, zindex, self$visible)
    }

  )
)

#' New solution
#'
#' Create a new [Solution] object.
#'
#' @param name `character` name for new solution.
#'
#' @param variable [Variable] object with the solution.
#'
#' @param visible `logical` should the solution be visible on a map?
#'
#' @param parameters `list` of [Parameter] objects.
#'
#' @param statistics `list` of [Statistic] objects.
#'
#' @param theme_results `list` of [ThemeResults] objects.
#'
#' @param weight_results `list` of [WeightResults] objects.
#'
#' @param include_results `list` of [IncludeResults] objects.
#'
#' @inheritParams new_theme
#'
#' @examples
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
#' # create variables
#' v1 <- new_variable_from_auto(dataset = d, index = 1)
#' v2 <- new_variable_from_auto(dataset = d, index = 2)
#'
#' # create features using variables
#' f1 <- new_feature(
#'   name = "Possum", variable = v2,
#'   goal = 0.2, status = FALSE, current = 0.5, id = "F1"
#' )
#'
#' # create themes using the features
#' t1 <- new_theme("Species", f1, id = "T1")
#'
#' # create a feature results object to store results for the feature
#' fr1 <- new_feature_results(f1, held = 0.8)
#'
#' # create a theme results object to store results for the feature
#' tr1 <- new_theme_results(t1, fr1)
#'
#' # create parameters
#' p1 <- new_parameter(name = "Spatial clustering")
#' p2 <- new_parameter(name = "Optimality gap")
#'
#' # create a new solution
#' s <- new_solution(
#'   name = "solution001",
#'   variable = v2,
#'   visible = TRUE,
#'   parameters = list(p1, p2),
#'   statistics = list(),
#'   theme_results = list(tr1),
#'   weight_results = list(),
#'   include_results = list(),
#'   id = "solution1"
#' )
#'
#' @export
new_solution <- function(name, variable, visible,
                         parameters,
                         statistics,
                         theme_results,
                         weight_results,
                         include_results,
                         id = uuid::UUIDgenerate()) {
  Solution$new(
    name = name,
    variable = variable,
    visible = visible,
    parameters = parameters,
    statistics = statistics,
    theme_results = theme_results,
    weight_results = weight_results,
    include_results = include_results,
    id = id
  )
}

#' New solution from result
#'
#' Create a new [Solution] object using a [Result] object
#'
#' @inheritParams new_solution
#'
#' @param dataset [Dataset] object.
#'
#' @param settings [SolutionSettings] object.
#'
#' @param result [Result] object.
#'
#' @param legend [ManualLegend] object.
#'
#' @return A [Solution] object.
#'
#' @examples
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
#' # create variables
#' v1 <- new_variable_from_auto(dataset = d, index = 1)
#' v2 <- new_variable_from_auto(dataset = d, index = 2)
#'
#' # create features using variables
#' f1 <- new_feature(
#'   name = "Possum", variable = v2,
#'   goal = 0.2, status = FALSE, current = 0.5, id = "F1"
#' )
#'
#' # create themes using the features
#' t1 <- new_theme("Species", f1, id = "T1")
#'
#' # create parameters
#' p1 <- new_parameter(name = "Spatial clustering")
#' p2 <- new_parameter(name = "Optimality gap")
#'
#' # create solution settings using the themes and weight
#' ss <- new_solution_settings(
#'   themes = list(t1),
#'   weights = list(),
#'   includes = list(),
#'   parameters = list(p1, p2)
#' )
#'
#' # create solution values
#' values <- sample(
#'   c(0, 1), length(d$get_planning_unit_indices()), replace = TRUE
#' )
#'
#' # create result object
#' r <- new_result(
#'   values = values,
#'   area = 12,
#'   perimeter = 10,
#'   theme_coverage = calculate_coverage(values, ss$get_theme_data()),
#'   weight_coverage = calculate_coverage(values, ss$get_weight_data()),
#'   include_coverage = calculate_coverage(values, ss$get_include_data()),
#'   theme_settings = ss$get_theme_settings(),
#'   weight_settings = ss$get_weight_settings(),
#'   include_settings = ss$get_include_settings(),
#'   parameters = ss$parameters
#' )
#'
#' # create solution using result object
#' s <- new_solution_from_result(
#'   name = "solution001",
#'   visible = TRUE,
#'   dataset = d,
#'   settings = ss,
#'   result = r,
#'   legend =  new_manual_legend(
#'     colors = c("#00FFFF00", "#112233FF"),
#'     labels = c("not selected", "selected")
#'   )
#' )
#'
#' @export
new_solution_from_result <- function(name, visible, dataset, settings, result,
                                     legend, id = uuid::UUIDgenerate()) {
  # assert arguments are valid
  assertthat::assert_that(
    ## name
    assertthat::is.string(name),
    assertthat::noNA(name),
    ## visible
    assertthat::is.flag(visible),
    assertthat::noNA(visible),
    ## dataset
    inherits(dataset, "Dataset"),
    ## settings
    inherits(settings, "SolutionSettings"),
    ## result
    inherits(result, "Result"),
    ## legend
    inherits(legend, "ManualLegend"),
    ## id
    assertthat::is.string(id),
    assertthat::noNA(id)
  )

  # calculate statistics
  ## preliminary calculations
  area_data <- dataset$get_planning_unit_areas()
  reserve_sizes_m <-
    reserve_sizes(
      x = result$values,
      areas = area_data,
      boundary_matrix = dataset$get_boundary_data()
    )
  ## generate statistics
  statistics_results <-
    list(
      new_statistic(
        name = "Total number of planning units",
        value = sum(result$values, na.rm = TRUE),
        units = "",
        proportion = mean(result$values > 0.5, na.rm = TRUE)
      ),
      new_statistic(
        name = "Total area",
        value = result$area * 1e-6,
        units = stringi::stri_unescape_unicode("km\\u00B2"),
        proportion = result$area / sum(area_data)
      ),
      new_statistic(
        name = "Total perimeter",
        value = result$perimeter * 1e-3,
        units = "km"
      ),
      new_statistic(
        name = "Total number of reserves",
        value = length(reserve_sizes_m),
        units = ""
      ),
      new_statistic(
        name = "Smallest reserve size",
        value = min(reserve_sizes_m) * 1e-6,
        units = stringi::stri_unescape_unicode("km\\u00B2")
      ),
      new_statistic(
        name = "Average reserve size",
        value = mean(reserve_sizes_m) * 1e-6,
        units = stringi::stri_unescape_unicode("km\\u00B2")
      ),
      new_statistic(
        name = "Largest reserve size",
        value = max(reserve_sizes_m) * 1e-6,
        units = stringi::stri_unescape_unicode("km\\u00B2")
      )
    )

  # include results
  include_results <- lapply(seq_along(settings$includes), function(i) {
    ## copy the include object
    incl <- settings$includes[[i]]$clone(deep = FALSE)
    ## apply settings from weight_settings
    k <- which(result$include_settings$id == incl$id)
    assertthat::assert_that(assertthat::is.count(k))
    incl$set_status(result$include_settings$status[[k]])
    ## return weight results
    new_include_results(
      include = incl,
      held = result$include_coverage[[incl$id]]
    )
  })

  # weight results
  weight_results <- lapply(seq_along(settings$weights), function(i) {
    ## copy the weight object
    w <- settings$weights[[i]]$clone(deep = FALSE)
    ## apply settings from weight_settings
    k <- which(result$weight_settings$id == w$id)
    assertthat::assert_that(assertthat::is.count(k))
    w$set_status(result$weight_settings$status[[k]])
    w$set_factor(result$weight_settings$factor[[k]])
    ## return weight results
    new_weight_results(
      weight = w,
      held = result$weight_coverage[[w$id]]
    )
  })

  # theme results
  theme_results <- lapply(seq_along(settings$themes), function(i) {
    ## copy the theme object
    th <- settings$themes[[i]]$clone(deep = FALSE)
    ## apply feature settings
    th$feature <- lapply(seq_along(th$feature), function(j) {
      ## copy the feature object
      f <- th$feature[[j]]$clone(deep = FALSE)
      ## apply settings from theme_settings
      k <- which(result$theme_settings$id == f$id)
      f$set_status(result$theme_settings$status[k])
      f$set_goal(result$theme_settings$goal[k])
      ## return feature
      f
    })
    ### generate theme results
    new_theme_results(
      theme = th,
      feature_results = lapply(seq_along(th$feature), function(j) {
        new_feature_results(
          feature = th$feature[[j]],
          held = result$theme_coverage[[th$feature[[j]]$id]]
        )
      })
    )
  })

  # generate index for storing data
  idx <- last(make.names(c(dataset$get_names(), name), unique = TRUE))
  idx <- gsub(".", "_", idx, fixed = TRUE)

  # create variable to store solution
  dataset$add_index(index = idx, values = result$values)
  v <- new_variable(
    dataset = dataset,
    index = idx,
    total = sum(result$values),
    units = "",
    legend = legend
  )

  # return solution object
  new_solution(
    name = name,
    variable = v,
    visible = visible,
    parameters = result$parameters,
    statistics = statistics_results,
    theme_results = theme_results,
    weight_results = weight_results,
    include_results = include_results,
    id = id
  )

}
