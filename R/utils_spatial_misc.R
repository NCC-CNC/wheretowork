#' @include internal.R
NULL

#' Shapefile field names
#'
#' Quickly extract the field names in a shapefile.
#'
#' @param x `character` path.
#'
#' @return `character` vector of field names.
#'
#' @examples
#' shapefile_field_names(system.file("shape/nc.shp", package = "sf"))
#' @export
shapefile_field_names <- function(x) {
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
  s <- sf::read_sf(dsn = x, layer = l, query = qu)
  # return field names (excluding geometry column)
  setdiff(names(s), attr(s, "sf_column"))
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