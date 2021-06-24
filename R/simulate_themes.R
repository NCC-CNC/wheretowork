#' @include internal.R
NULL

#' Simulate themes
#'
#' This function simulates [SingleTheme] and [MultiTheme] objects.
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
#'   of features for each [MultiTheme] object.
#'   This parameter is used as a argument to `rpois(n, lambda = lambda)`.
#'   Defaults to 5.
#'
#' @return A `list` of simulated [Theme] objects.
#'
#' @seealso [new_single_theme], [new_multi_theme].
#'
#' @examples
#' # import data
#' f <- system.file("extdata", "sim_raster_data.tif", package = "locationmisc")
#' d <- new_dataset(raster::raster(f))
#'
#' # simulate data
#' x <- simulate_themes(data = d, n_single_themes = 3, n_multi_themes = 2)
#'
#' # print results
#' print(x)
#'
#' @export
simulate_themes <- function(
  dataset, n_single_themes, n_multi_themes, lambda = 5) {
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
    assertthat::noNA(lambda))

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
    if (stats::runif(1) > 0.5) {
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
    ### create theme
    st[[i]] <-
      new_single_theme(
        name = st_names[i],
        mandatory = stats::runif(1) > 0.5,
        new_feature(
          name = paste0(st_names[i], " habitat"),
          initial_goal = round(stats::runif(1, 0.5, 0.9), 2),
          current = round(stats::runif(1, 0.1, 0.6), 2),
          limit_goal = round(stats::runif(1, 0, 0.4), 2),
          icon = example_feature_icon(),
          variable =
            new_variable_from_auto(
              dataset = dataset, index = st_index[[i]], units = "ha"
            )
        )
      )
  }

  # simulate multi themes
  ## exclude names from tn object
  tn <- tn[-st_idx, , drop = FALSE]
  ## simulate number of features for each multi theme
  mt_n_features <- stats::rpois(n_multi_themes, lambda) + 1
  ## calculate number of features within each theme
  tn_meta <-
    tibble::as_tibble(
      as.data.frame(stats::aggregate(
        tn$theme, by = list(tn$theme), FUN = length)))
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
    if (stats::runif(1) > 0.5) {
      #### simulate continuous data
      curr_mtd <-
        simulate_continuous_spatial_data(
          data, mt_n_features[i])
    } else {
      #### simulate categorical data
      curr_mtd <- simulate_categorical_spatial_data(
        data, mt_n_features[i])
    }
    names(curr_mtd)[seq_len(mt_n_features[i])] <- curr_tn_index
    ### add theme data to dataset
    if (inherits(data, "sf")) {
      for (j in seq_along(curr_tn_index)) {
        dataset$add_index(curr_tn_index[[j]], curr_mtd[[curr_tn_index[[j]]]])
      }
    } else {
      for (j in seq_along(curr_tn_index)) {
        dataset$add_index(
          curr_tn_index[[j]], curr_mtd[[curr_tn_index[[j]]]][idx])
      }
    }
    ### create features
    curr_fts <- lapply(seq_len(mt_n_features[i]), function(j) {
      new_feature(
        name = curr_tn_names[[j]],
        initial_goal = round(stats::runif(1, 0.5, 0.9), 2),
        current = round(stats::runif(1, 0.1, 0.6), 2),
        limit_goal = round(stats::runif(1, 0, 0.4), 2),
        icon = example_feature_icon(),
        variable =
          new_variable_from_auto(
            dataset = dataset, index = curr_tn_index[[j]], units = "ha"
          )
      )
    })
    #### generate theme
    mt[[i]] <-
      new_multi_theme(
        name = mt_names[i],
        feature = curr_fts,
        icon = example_theme_icon(),
      )
  }

  # return results
  append(st, mt)
}
