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
#' # simulate data
#' x <- simulate_data(
#'   data  = simulate_simple_vector_dataset(),
#'   n_single_themes = 3, n_multi_themes = 2, n_weights = 1)
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
  ## simulate underlying data values
  std <- simulate_spatial_autocorrelated_values(data, n_single_themes)
  names(std)[seq_len(n_single_themes)] <- st_index
  if (inherits(data, "sf")) {
    d$data <- cbind(d$data, sf::st_drop_geometry(std))
    std_min <- apply(sf::st_drop_geometry(std), 2, min, na.rm = TRUE)
    std_max <- apply(sf::st_drop_geometry(std), 2, max, na.rm = TRUE)
    std_total <- colSums(sf::st_drop_geometry(std))
  } else {
    d$data <- raster::stack(d$data, std)
    std_min <- raster::cellStats(std, "min")
    std_max <- raster::cellStats(std, "max")
    std_total <- raster::cellStats(std, "sum")
  }

  ## generate themes
  st <- lapply(seq_len(n_single_themes), function(i) {
    new_single_theme(
      name = st_names[i],
      mandatory = runif(1) > 0.5,
      new_feature(
        name = paste0(st_names[i], " habitat"),
        initial_goal = stats::runif(1, 0.5, 0.9),
        current = stats::runif(1, 0.1, 0.6),
        limit_goal = stats::runif(1, 0, 0.4),
        icon = example_feature_icon(),
        variable =
          new_variable(
            dataset = d,
            index = st_index[[i]],
            total = std_total[[i]],
            units = "ha",
            legend = new_continuous_legend(
              min_value = std_min[[i]],
              max_value = std_max[[i]],
              colors = color_palette("random")
            )
          )
      )
    )
  })

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
    curr_mtd <- simulate_spatial_autocorrelated_values(data, mt_n_features[i])
    names(curr_mtd)[seq_len(mt_n_features[i])] <- curr_tn_index
    if (inherits(data, "sf")) {
      d$data <- cbind(d$data, sf::st_drop_geometry(curr_mtd))
      curr_mtd_min <-
        apply(sf::st_drop_geometry(curr_mtd), 2, min, na.rm = TRUE)
      curr_mtd_max <-
        apply(sf::st_drop_geometry(curr_mtd), 2, max, na.rm = TRUE)
      curr_mtd_total <-
        colSums(sf::st_drop_geometry(curr_mtd))
    } else {
      d$data <- raster::stack(d$data, curr_mtd)
      curr_mtd_min <- raster::cellStats(curr_mtd, "min")
      curr_mtd_max <- raster::cellStats(curr_mtd, "max")
      curr_mtd_total <- raster::cellStats(curr_mtd, "sum")
    }
    ### create features
    curr_fts <- lapply(seq_len(mt_n_features[i]), function(j) {
      new_feature(
        name = curr_tn_names[[j]],
        initial_goal = stats::runif(1, 0.5, 0.9),
        current = stats::runif(1, 0.1, 0.6),
        limit_goal = stats::runif(1, 0, 0.4),
        icon = example_feature_icon(),
        variable =
          new_variable(
            dataset = d,
            index = curr_tn_index[[j]],
            total = curr_mtd_total[[j]],
            units = "ha",
            legend = new_continuous_legend(
              min_value = curr_mtd_min[[j]],
              max_value = curr_mtd_max[[j]],
              colors = color_palette("random")
            )
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
  wd <- simulate_spatial_autocorrelated_values(data, n_weights)
  names(wd)[seq_len(n_weights)] <- wn_index
  if (inherits(data, "sf")) {
    d$data <- cbind(d$data, sf::st_drop_geometry(wd))
    w_min <- apply(sf::st_drop_geometry(wd), 2, min, na.rm = TRUE)
    w_max <- apply(sf::st_drop_geometry(wd), 2, max, na.rm = TRUE)
    w_total <- colSums(sf::st_drop_geometry(wd))
  } else {
    d$data <- raster::stack(d$data, wd)
    w_min <- raster::cellStats(wd, "min")
    w_max <- raster::cellStats(wd, "max")
    w_total <- raster::cellStats(wd, "sum")
  }
  ## generate weights
  w <- lapply(seq_len(n_weights), function(i) {
    new_weight(
      name = wn[[1]][i],
      variable =
        new_variable(
          dataset = d,
          index = wn_index[i],
          total = w_total[[i]],
          units = wn[[2]][i],
          legend = new_continuous_legend(
            min_value = w_min[[i]],
            max_value = w_max[[i]],
            colors = color_palette("random")
          )
        )
    )
  })

  # return results
  list(themes = append(st, mt), weights = w)
}
