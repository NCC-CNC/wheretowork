#' @include internal.R
NULL

#' Simulate themes
#'
#' This function simulates [Theme] objects.
#'
#' @param dataset [Dataset] object.
#'
#' @param n_single_themes `integer` number of themes with a single feature
#'   to simulate.
#'
#' @param n_multi_themes `integer` number of themes with a multiple features
#'   to simulate.
#'
#' @param lambda `numeric` lambda parameter for simulating the number
#'   of features for themes that contain multiple features.
#'   This parameter is used as a argument to `rpois(n, lambda = lambda)`.
#'   Defaults to 5.
#'   
#' @param continuous `logical` should the data be continuous?  
#' Defaults to `NA`.
#'
#' @return A `list` of simulated [Theme] objects.
#'
#' @seealso [new_theme].
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
#' # simulate data
#' if (requireNamespace("RandomFields")) {
#'  x <- simulate_themes(data = d, n_single_themes = 3, n_multi_themes = 2)
#'  # print results
#'  print(x)
#' }
#'
#' @export
simulate_themes <- function(dataset, n_single_themes, n_multi_themes,
                            lambda = 5, continuous = NA) {
 
  # assert arguments are valid
  assertthat::assert_that(
    ## data
    inherits(dataset, c("Dataset")),
    ## n_single_themes
    assertthat::is.number(n_single_themes),
    assertthat::noNA(n_single_themes),
    ## n_multi_themes
    assertthat::is.number(n_multi_themes),
    assertthat::noNA(n_multi_themes),
    n_single_themes + n_multi_themes > 0,
    ## lambda
    assertthat::is.number(lambda),
    assertthat::noNA(lambda),
    ## continuous
    class(continuous) == "logical"
  )

  # extract data
  data <- dataset$get_spatial_data()
  idx <- dataset$attribute_data[["_index"]]

  # simulate single themes
  ## simulate theme names
  tn <- example_theme_names()
  ## select names
  st_idx <- sample.int(nrow(tn), n_single_themes)
  st_names <- tn$feature[st_idx]
  ## set index names
  st_index <- make_valid_names(st_names)
  ## generate themes
  st <- list()
  for (i in seq_len(n_single_themes)) {
    ### simulate features for the theme
    if (is.na(continuous)) {
      ### Randomly choose which data type to simulate
      if (stats::runif(1) > 0.5) {
        #### simulate continuous feature data
        std <- simulate_continuous_spatial_data(data, 1)
      } else {
        #### simulate categorical feature data
        std <- simulate_categorical_spatial_data(data, 1)
      }      
    } else if (continuous) {
      #### simulate continuous feature data
      std <- simulate_continuous_spatial_data(data, 1)
    } else {
      #### simulate categorical feature data
      std <- simulate_categorical_spatial_data(data, 1)      
    }
    names(std)[1] <- st_index[i]
    ### append data to dataset
    if (inherits(data, "sf")) {
      dataset$add_index(st_index[i], std[[1]])
    } else {
      dataset$add_index(st_index[i], std[[1]][idx])
    }
    
    ### Check if data is categorical 
    if (sum(dataset$attribute_data[st_index[i]]) %% 1 == 0) {
      ### build variable with new manual legend
      d <- dataset$get_attribute_data()[[st_index[[i]]]]
      u <- c(na.omit(unique(d)))
      cp <- color_palette(x = "random", n = length(u))
      v <- new_variable(dataset = dataset, index = st_index[[i]], 
        units = "ha", legend = new_manual_legend(cp, paste("value:", u)), 
        total = sum(d, na.rm = TRUE),
        provenance = new_provenance_from_source("national"))       
    } else {
      ### build variable with auto legend (all data here should be continuous)
      v = new_variable_from_auto(
        dataset = dataset, index = st_index[[i]], units = "ha",
        provenance = sample(c("regional", "national"), 1)
      )
    }
    ### create theme
    st[[i]] <- new_theme(
      name = st_names[i],
      new_feature(
        name = paste0(st_names[i], " habitat"),
        goal = round(stats::runif(1, 0.5, 0.9), 2),
        current = round(stats::runif(1, 0.1, 0.6), 2),
        limit_goal = round(stats::runif(1, 0, 0.4), 2),
        variable = v
      )
    )
  }

  # simulate multi themes
  ## exclude names from tn object
  tn <- tn[-st_idx, , drop = FALSE]
  ## simulate number of features for each multi theme
  mt_n_features <- pmax(stats::rpois(n_multi_themes, lambda), 2)
  ## calculate number of features within each theme
  tn_meta <-
    tibble::as_tibble(
      as.data.frame(stats::aggregate(
        tn$theme,
        by = list(tn$theme), FUN = length
      ))
    )
  tn_meta <- stats::setNames(tn_meta, c("theme", "n"))
  ## select names
  mt_idx <- numeric(n_multi_themes)
  mt_names <- character(n_multi_themes)
  for (i in seq_len(n_multi_themes)) {
    ## set mt_idx as a randomly selected theme index,
    ## that has enough features for the theme
    mt_idx[i] <- sample(which(tn_meta$n >= mt_n_features[i]), 1)
    mt_names[i] <- tn_meta$theme[[mt_idx[i]]]
    ## remove the selected theme from the table so that it
    ## can't be selected again
    tn_meta <- tn_meta[-mt_idx[i], , drop = FALSE]
  }

  ## simulate underlying data
  mt <- list()
  for (i in seq_len(n_multi_themes)) {
    ### simulate underyling data for features within the theme
    curr_tn_names <-
      sample(tn$feature[tn$theme == mt_names[i]], mt_n_features[i])
    curr_tn_index <- make_valid_names(curr_tn_names)
    ### simulate features within the theme
    
    if (is.na(continuous)) {
      ### Randomly choose which data type to simulate
      if (stats::runif(1) > 0.5) {
        #### simulate continuous data
        curr_mtd <-
          simulate_continuous_spatial_data(
            data, mt_n_features[i]
          )
      } else {
        #### simulate categorical data
        curr_mtd <- simulate_categorical_spatial_data(
          data, mt_n_features[i]
        )
      }      
    } else if (continuous) {
      #### simulate continuous data
      curr_mtd <-
        simulate_continuous_spatial_data(
          data, mt_n_features[i]
        )
    } else {
      #### simulate categorical data
      curr_mtd <- simulate_categorical_spatial_data(
        data, mt_n_features[i]
      )   
    }
    # if (stats::runif(1) > 0.5) {
    #   #### simulate continuous data
    #   curr_mtd <-
    #     simulate_continuous_spatial_data(
    #       data, mt_n_features[i]
    #     )
    # } else {
    #   #### simulate categorical data
    #   curr_mtd <- simulate_categorical_spatial_data(
    #     data, mt_n_features[i]
    #   )
    # }
    names(curr_mtd)[seq_len(mt_n_features[i])] <- curr_tn_index
    ### add theme data to dataset
    if (inherits(data, "sf")) {
      for (j in seq_along(curr_tn_index)) {
        dataset$add_index(curr_tn_index[[j]], curr_mtd[[curr_tn_index[[j]]]])
      }
    } else {
      for (j in seq_along(curr_tn_index)) {
        dataset$add_index(
          curr_tn_index[[j]], curr_mtd[[curr_tn_index[[j]]]][idx]
        )
      }
    }
    ### Check if data is categorical 
    if (sum(dataset$attribute_data[curr_tn_index[j]]) %% 1 == 0) {
      ### build variable with new manual legend
      d <- dataset$get_attribute_data()[[curr_tn_index[[j]]]]
      u <- c(na.omit(unique(d)))
      cp <- color_palette(x = "random", n = length(u))
      v <- new_variable(dataset = dataset, index = curr_tn_index[[j]], 
                        units = "ha", legend = new_manual_legend(cp, paste("value:", u)), 
                        total = sum(d, na.rm = TRUE),
                        provenance = new_provenance_from_source("national"))       
    } else {
      ### build variable with auto legend (all data here should be continuous)
      v = new_variable_from_auto(
        dataset = dataset, index = curr_tn_index[[j]], units = "ha",
        provenance = sample(c("regional", "national"), 1)
      )
    }    
    ### create features
    curr_fts <- lapply(seq_len(mt_n_features[i]), function(j) {
      new_feature(
        name = curr_tn_names[[j]],
        goal = round(stats::runif(1, 0.5, 0.9), 2),
        current = round(stats::runif(1, 0.1, 0.6), 2),
        limit_goal = round(stats::runif(1, 0, 0.4), 2),
        variable = v
      )
    })
    #### generate theme
    mt[[i]] <-
      new_theme(
        name = mt_names[i],
        feature = curr_fts
      )
  }

  # return results
  append(st, mt)
}
