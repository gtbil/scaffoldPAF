#' Title
#'
#' @param contigs Data frame with the following fields:
#'  - qname
#'  - start
#'  - end
#'  - str (should it be flipped?)
#'  - contiglen
#'
#' @return A data frame with the selected contigs in the order that minimizes gaps
#' @export
#'
#' @examples
#' \dontrun{ }
minimize_gaps <- function(contigs) {

  # get the node number
  # which will be two times the number of contigs + the start and end
  nnode <- 2L * nrow(contigs) + 2L

  # short circuit if there's zero or one contig
  if (nnode <= 4L) {
    return(contigs)
  }

  # calculate some values that will be useful later
  chrlen <- max(contigs$end) + 1000000L

  # get an inverse measure for the "coverage" of a contig on the genome
  coverage <- with(contigs, contiglen - (end - start + 1))

  # realign so it starts at zero
  coverage <- coverage - min(coverage)

  # and flip so the longest insert is aligned at zero
  coverage <- max(coverage) - coverage

  # make a heuristic for the max overlap
  # 5% of the chrlen?
  # the issue is dealing with things "within inversions"
  # this could induce significant overlap
  maxoverlap <- floor(0.05 * chrlen) |> as.integer()

  # add the node number to the contigs
  # these will be the IN and OUT edges
  contigs$node    <- seq(from = 2L, to = nrow(contigs) + 1L)
  contigs$nodeout <- seq(from = nrow(contigs) + 2L, to = nnode - 1L)

  graph <- igraph::make_empty_graph(nnode, directed = TRUE)

  # now construct the edges and weights
  edges <- list()
  weights <- list()

  edges$starts <- sapply(contigs$node, \(x) c(1L, x)) |> c()
  edges$withins <- rbind(contigs$node, contigs$nodeout) |> c()
  edges$ends   <- sapply(contigs$nodeout, \(x) c(x, nnode)) |> c()

  # generate all the combinations of contigs
  # going both ways
  combn.idx  <- cbind(utils::combn(1:nrow(contigs), 2),
                      utils::combn(nrow(contigs):1, 2))

  # allowing a small amount of overlap based on the total chromosome size
  # find permitted connections
  combn.vals <- apply(combn.idx, 2, \(x) contigs[x[1], "end"] < contigs[x[2], "start"] + maxoverlap) |>
    which()

  # grab the permitted values
  # TODO: what to do if this is empty? nothing?
  combn.idx <- combn.idx[, combn.vals, drop = FALSE]

  # get the distances
  combn.sizes <- apply(combn.idx, 2, \(x) contigs[x[2], "start"] - contigs[x[1], "end"]) |>
    unlist() |> unname()

  # now reindex the first row so that we're using the correct values as the right end of each contig node
  combn.idx[] <- combn.idx + 1L
  combn.idx[1, ] <- combn.idx[1, ] + nrow(contigs)

  edges$betweens <- c(combn.idx)

  # if negative (means overlap, just set to distance of 1L
  combn.sizes <- ifelse(combn.sizes < 0, 1L, combn.sizes)

  weights$starts <- contigs$start
  weights$withins <- coverage
  weights$ends <- chrlen - contigs$end
  weights$betweens <- combn.sizes

  graph <- igraph::add_edges(graph, purrr::flatten_int(edges))

  igraph::E(graph)$weight <- purrr::flatten_int(weights)

  path <- igraph::shortest_paths(graph,
                                 from = 1,
                                 to = nnode,
                                 algorithm = "dijkstra",
                                 mode = "out",
                                 predecessors = TRUE)

  path.vec <- as.vector(path$vpath[[1]])
  path.vec <- path.vec[seq(from = 2L, to = length(path.vec) - 1L, by = 2L)] - 1L

  return(contigs[path.vec, 1:5])
}
