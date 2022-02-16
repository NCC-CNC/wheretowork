#' @include internal.R
NULL

#' Shapefile field names
#'
#' Quickly extract the field names in a shapefile.
#'
#' @param x `character` path.
#'
#' @param inherits `character` class to return only column names that
#' contain a certain type of data.
#' Defaults to `NULL` such that all column names are returned.
#'
#' @return `character` vector of field names.
#'
#' @examples
#' shapefile_field_names(system.file("shape/nc.shp", package = "sf"))
#' @export
shapefile_field_names <- function(x, inherits = NULL) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(x),
    endsWith(x, ".shp")
  )
  # find layer name
  l <- sf::st_layers(x)$name[[1]]
  # prepare query
  qu <- paste0("SELECT * FROM \"", l, "\" WHERE FID <= 2")
  # import shapefile
  s <- sf::read_sf(dsn = x, query = qu)
  # drop geometry data
  s <- sf::st_drop_geometry(s)
  # if inherits is specified, then subset columns with specified data type
  if (!is.null(inherits)) {
    s <- dplyr::select_if(s, function(x) inherits(x, inherits))
  }
  # return column names
  names(s)
}

#' Repair spatial data
#'
#' Repair geometry in a spatial dataset.
#'
#' @param x [sf::st_sf()] object.
#'
#' @return [sf::st_sf()] object.
#'
#' @examples
#' path <- system.file("shape/nc.shp", package = "sf")
#' g <- sf::read_sf(path)
#' repair_spatial_data(g)
#' @export
repair_spatial_data <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "sf")
  )

  # repair geometry
  x <- sf::st_make_valid(x)

  # return result
  x
}

#' Reserve sizes
#'
#' Calculate the size of individual reserves in a solution.
#'
#' @param x `numeric` vector containing the solution values for each planning
#'   unit.
#'
#' @param areas `numeric` vector containing the area of each planning unit.
#'
#' @param boundary_matrix [Matrix::sparseMatrix()] object with boundary length
#'   data for the planning units.
#'
#' @return A `numeric` vector containing the total area of each reserve.
#'
#' @export
reserve_sizes <- function(x, areas, boundary_matrix) {
  # assert the argument are valid
  assertthat::assert_that(
    ## x
    is.numeric(x),
    assertthat::noNA(x),
    ## areas
    is.numeric(areas),
    assertthat::noNA(areas),
    length(areas) == length(x),
    ## boundary matrix
    inherits(boundary_matrix, c("dsCMatrix", "dgCMatrix")),
    nrow(boundary_matrix) == ncol(boundary_matrix),
    nrow(boundary_matrix) == length(x)
  )

  # return NA if no planning units selected, then return NA
  if (sum(x) < 0.5) {
    return(NA_real_)
  }

  # create adjacency matrix to describe relationships among units
  idx <- which(x > 0.5)
  adj_matrix <- boundary_matrix[idx, idx]
  adj_matrix@x <- rep(1, length(adj_matrix@x))
  Matrix::diag(adj_matrix) <- 1

  # subset areas to contain only selected planning units
  areas <- areas[idx]

  # create graph
  g <- igraph::graph_from_adjacency_matrix(adj_matrix, mode = "undirected")

  # identify components
  clu <- igraph::components(g)

  # calculate total size of each reserve
  vapply(seq_len(clu$no), FUN.VALUE = numeric(1), function(i) {
    sum(areas[clu$membership == i])
  })
}

#' Calculate coverage
#'
#' Calculate how well a solution covers data.
#'
#' @param x `numeric` solution values.
#'
#' @param data [Matrix::sparseMatrix()] object.
#'
#' @return `numeric` vector.
#'
#' @examples
#' # load dependency
#' library(Matrix)
#'
#' # simulate solution values for 10 planning units
#' solution_values <- sample(c(0, 1), 10, replace = TRUE)
#'
#' # simulate data for 5 features in those 10 planning units
#' feature_data <- as(matrix(runif(10 * 5), ncol = 10), "dgCMatrix")
#'
#' # calculate coverage
#' calculate_coverage(solution_values, feature_data)
#' @export
calculate_coverage <- function(x, data) {
  assertthat::assert_that(
    is.numeric(x),
    inherits(data, "dgCMatrix")
  )
  if (nrow(data) > 0) {
    out <- matrix(x, byrow = TRUE, ncol = ncol(data), nrow = nrow(data))
    out <- rowSums(out * data) / Matrix::rowSums(data)
    names(out) <- rownames(data)
  } else {
    out <- numeric(0)
  }
  out
}
