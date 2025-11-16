#' Extract a Giant Component
#'
#' This function Extracts the giant component of a network
#'
#' @param graph an igraph object
#'
#' @return The giant component of the provided graph
#' @examples
#' # Create a random graph with multiple components
#' set.seed(123)
#' g <- igraph::sample_gnp(20, 0.1) # 20 nodes, edge probability 0.1
#' # Extract the giant component using your function
#' g_giant <- extract_giant_component(g)
#' @export
extract_giant_component <- function(graph) {
  # Check that the input is an igraph object
  if (!inherits(graph, "igraph")) {
    stop("Input must be an igraph object.")
  }

  # Identify the connected components in the graph
  components <- igraph::clusters(graph)

  # Find the largest component (the giant component)
  giant_component_id <- base::which.max(components$csize)

  # Extract the vertices that belong to the giant component
  giant_component_vertices <- base::which(components$membership == giant_component_id)

  # Subgraph induced by the vertices of the giant component
  giant_component <- igraph::induced_subgraph(graph, giant_component_vertices)

  return(giant_component)
}
