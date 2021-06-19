#' @include internal.R
NULL

#' Simulate data
#'
#' This function simulates [Weight], [SingleTheme], and [MultiTheme] objects.
#'
#' @param data [sf::st_sf()] or [raster::raster()] object containing
#'   the spatial data that underpin the planning units.
#'
#' @param n_single_themes `integer` number of themes with a single feature
#'   to simulate.
#'
#' @param n_multi_themes `integer` number of themes with a multiple features
#'   to simulate.
#'
#' @param n_weights `integer` number of weights to simulate.
#'
#' @param lambda `numeric` lambda parameter for simulating the number
#'   of features for each [MultiTheme] object.
#'   This parameter is used as a argument to `rpois(n, lambda = lambda)`.
#'   Defaults to 5.
#'
#' @return A `list` containing the simulated objects. It has `themes` and
#'  `weights` elements that contain [Theme] and [Weight] objects (respectively).
#'
#' @seealso [new_weight], [new_single_theme], [new_multi_theme].
#'
#' @examples
#' # import data
#' f <- system.file("extdata", "sim_raster_data.tif", package = "locationmisc")
#' d <- raster::raster(f)
#'
#' # simulate data
#' x <- simulate_data(
#'   data  = d, n_single_themes = 3, n_multi_themes = 2, n_weights = 1)
#'
#' # print results
#' print(x)
#'
#' @export
simulate_data <- function(
  data, n_single_themes, n_multi_themes, n_weights, lambda = 5) {
  # assert arguments are valid
  assertthat::assert_that(
    ## data
    inherits(data, c("sf", "RasterLayer")),
    ## n_single_themes
    assertthat::is.number(n_single_themes),
    assertthat::noNA(n_single_themes),
    ## n_multi_themes
    assertthat::is.number(n_multi_themes),
    assertthat::noNA(n_multi_themes),
    n_single_themes + n_multi_themes > 0,
    ## n_weights
    assertthat::is.number(n_weights),
    assertthat::noNA(n_weights),
    n_weights > 0,
    ## lambda
    assertthat::is.number(lambda),
    assertthat::noNA(lambda))

  # simulate dataset
  d <- new_dataset(data)

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
      d$data <- cbind(d$data, sf::st_drop_geometry(std))
    } else {
      d$data <- raster::stack(d$data, std)
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
              dataset = d, index = st_index[[i]], units = "ha"
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
    tn_meta <- tn_meta[-i, , drop = FALSE]
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
      d$data <- cbind(d$data, sf::st_drop_geometry(curr_mtd))
    } else {
      d$data <- raster::stack(d$data, curr_mtd)
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
            dataset = d, index = curr_tn_index[[j]], units = "ha"
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

  # simulate weights
  ## set weight names
  wn <- example_weight_names()
  wn <- wn[sample.int(nrow(wn)), , drop = FALSE]
  wn <- wn[seq_len(n_weights), , drop = FALSE]
  ## assert that there are sufficient example names
  assertthat::assert_that(
    n_weights <= nrow(wn),
    msg = "insufficient example names for this many weights"
  )
  ## set index names
  wn_index <- make_valid_names(wn[[1]])
  ## simulate underlying data values
  wd <- simulate_proportion_spatial_data(data, n_weights)
  names(wd)[seq_len(n_weights)] <- wn_index
  if (inherits(data, "sf")) {
    d$data <- cbind(d$data, sf::st_drop_geometry(wd))
  } else {
    d$data <- raster::stack(d$data, wd)
  }
  ## generate weights
  w <- lapply(seq_len(n_weights), function(i) {
    new_weight(
      name = wn[[1]][i],
      variable =
        new_variable_from_auto(
          dataset = d, index = wn_index[[i]], units = "ha"
        )
    )
  })

  # return results
  list(themes = append(st, mt), weights = w)
}
