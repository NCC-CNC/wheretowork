#' @include internal.R
NULL

#' Calculate locked in/out planning units and area budget data
#'
#' Calculate which planning units are locked in/out given the selected
#' Includes and Excludes (accounting for the overlap setting), the cost
#' and area budget values derived from these, and whether the total area
#' budget is too low given the locked in planning units. This mirrors the
#' calculations performed when solving the minimum shortfall formulation
#' of the problem (see [min_shortfall_result()]), so that the same values
#' can be produced before a solution is generated (e.g. to check
#' feasibility in the user interface).
#'
#' @inheritParams min_shortfall_result
#'
#' @return `list` with the following elements:
#' \describe{
#' \item{locked_in}{`logical` vector indicating which planning units are
#'   locked in.}
#' \item{locked_out}{`logical` vector indicating which planning units are
#'   locked out.}
#' \item{cost}{`numeric` vector of rescaled planning unit costs.}
#' \item{total_budget}{`numeric` total area budget.}
#' \item{initial_budget}{`numeric` area budget available prior to spatial
#'   clustering.}
#' \item{exceeded}{`logical` indicating if the cost of the locked in
#'   planning units exceeds the area budget.}
#' }
#'
#' @noRd
calculate_area_budget_data <- function(area_data,
                                        include_data,
                                        include_settings,
                                        exclude_data,
                                        exclude_settings,
                                        overlap,
                                        area_budget_proportion,
                                        boundary_gap) {
  # calculate locked in values
  if (nrow(include_data) > 0) {
    ## if includes present, then use data and settings
    locked_in <- matrix(
      include_settings$status,
      byrow = FALSE,
      nrow = nrow(include_data), ncol = ncol(include_data)
    )
    locked_in <- as.logical(Matrix::colSums(locked_in * include_data) > 0)
  } else {
    ## if no includes present, then lock nothing in
    locked_in <- rep(FALSE, ncol(include_data))
  }

  # calculate locked out values
  if (nrow(exclude_data) > 0) {
    ## if excludes present, then use data and settings
    locked_out <- matrix(
      exclude_settings$status,
      byrow = FALSE,
      nrow = nrow(exclude_data), ncol = ncol(exclude_data)
    )
    locked_out <- as.logical(Matrix::colSums(locked_out * exclude_data) > 0)
  } else {
    ## if no excludes present, then lock nothing out
    locked_out <- rep(FALSE, ncol(exclude_data))
  }

  ## locked-out takes precedence if overlap is TRUE
  idx <- which(locked_in & locked_out)
  if (!overlap) {
    locked_out[idx] <- FALSE
  } else {
    locked_in[idx] <- FALSE
  }

  # calculate cost values
  cost <- scales::rescale(area_data, to = c(0.01, 1))

  # calculate budgets for multi-objective optimization
  total_budget <- sum(cost) * area_budget_proportion
  if (boundary_gap >= 1e-5) {
    initial_budget <- (1 - boundary_gap) * total_budget
  } else {
    initial_budget <- total_budget
  }

  # return result
  list(
    locked_in = locked_in,
    locked_out = locked_out,
    cost = cost,
    total_budget = total_budget,
    initial_budget = initial_budget,
    exceeded = sum(cost[locked_in]) > min(initial_budget, total_budget)
  )
}
