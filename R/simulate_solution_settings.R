#' @include internal.R SolutionSettings-class.R
NULL

#' Simulate a solution settings object
#'
#' This function simulates a solution settings object. It is primarily used
#' for testing the package.
#'
#' @param n_single_themes `integer` number of themes with a single feature
#'   to simulate. Defaults to 5.
#'
#' @param n_multi_themes `integer` number of themes with a multiple features
#'   to simulate. Defaults to 15.
#'
#' @param n_weights `integer` number of weights to simulate.
#'   Defaults to 5.
#'
#' @param lambda `numeric` lambda parameter for simulating the number
#'   of features for each theme.
#'   This parameter is used as a argument to `rpois(n, lambda = lambda)`.
#'   Defaults to 5.
#'
#' @return A [SolutionSettings] object.
#'
#' @seealso [new_solution_settings()].
#'
#' @examples
#' # simulate solution settings
#' x <- simulate_solution_settings(
#'   n_single_themes = 3, n_multi_themes = 2, n_weights = 1)
#'
#' # print results
#' print(x)
#'
#' @export
simulate_solution_settings <- function(
  n_single_themes = 5, n_multi_themes = 15, n_weights = 5, lambda = 5) {
  # assert arguments are valid
  assertthat::assert_that(
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

  # load example names
  ## weight names
  wn <- example_weight_names()
  ## theme names
  tn <- example_theme_names()

  # simulate number of features for each multi theme
  ## initialize variables
  n_features <- numeric(0)
  still_need <- n_multi_themes
  ## loop until data has been simulated
  while (still_need > 0.5) {
    ### simulate a set of integers
    n <- stats::rpois(n_multi_themes * 100, lambda)
    ### exclude values equal to 1
    n <- n[n > 1]
    ### append values to main variables
    n_features <-
      c(
        n_features,
        n[seq_len(min(length(n), still_need))]
      )
    ## update variable to keep track of remaining simulated values needed
    still_need <- n_multi_themes - length(n_features)
  }

  # simulate weights
  ## shuffle weight names
  wn <- wn[[1]][sample.int(nrow(wn))]
  ## assert that we have enough example names to simulate the data
  assertthat::assert_that(
    n_weights <= length(wn),
    msg = "insufficient example names for this many weights"
  )
  ## generate weights
  w <- lapply(seq_len(n_weights), function(i) {
    new_weight(
      name = wn[i],
      variable =
        new_variable(
          dataset = new_dataset(tempfile()),
          index = 1,
          total = stats::runif(1, 1e+2, 1e+4),
          units = "???",
          legend = simulate_continuous_legend())
    )
  })

  # simulate single themes
  ## select names
  st_idx <- sample.int(nrow(tn), n_single_themes)
  st_names <- tn$feature[st_idx]
  ## generate themes
  st <- lapply(seq_len(n_single_themes), function(i) {
    new_single_theme(
      name = st_names[i],
      new_feature(
        name = paste0(st_names[i], " habitat"),
        initial_goal = stats::runif(1, 0.5, 0.9),
        current = stats::runif(1, 0.1, 0.6),
        limit_goal = stats::runif(1, 0, 0.4),
        icon = example_feature_icon(),
        variable =
          new_variable(
            dataset = new_dataset(tempfile()),
            index = 1,
            total = stats::runif(1, 1e+2, 1e+4),
            units = "ha",
            legend = simulate_continuous_legend())
      )
    )
  })
  ## exclude names from tn object
  tn <- tn[-st_idx, , drop = FALSE]

  # simulate multi themes
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
    mt_idx[i] <- sample(which(tn_meta$n >= n_features[i]), 1)
    mt_names[i] <- tn_meta$theme[[mt_idx[i]]]
    ## remove the selected theme from the table so that it
    ## can't be selected again
    tn_meta <- tn_meta[-i, , drop = FALSE]
  }
  ## generate themes
  mt <- lapply(seq_len(n_multi_themes), function(i) {
    ### generate features within the theme
    fts <-
      lapply(
        sample(tn$feature[tn$theme == mt_names[i]], n_features[i]),
        function(x) {
          new_feature(
            name = x,
            initial_goal = stats::runif(1, 0.5, 0.9),
            current = stats::runif(1, 0.1, 0.6),
            limit_goal = stats::runif(1, 0, 0.4),
            icon = example_feature_icon(),
            variable =
              new_variable(
                dataset = new_dataset(tempfile()),
                index = 1,
                total = stats::runif(1, 1e+2, 1e+4),
                units = "ha",
                legend = simulate_continuous_legend())
          )
        }
      )
    #### generate theme
    new_multi_theme(
      name = mt_names[i],
      feature = fts,
      icon = example_theme_icon(),
    )
  })

  # return solution settings
  new_solution_settings(themes = append(st, mt), weights = w)
}
