#' Import both haplotypes
#'
#' @param root Root name of the file, without ".hapX.paf"
#'
#' @return Data frame with the realigned data
#' @export
#'
#' @examples
#' \dontrun{ }
do_both_haps <- function(root) {
  tname <- tstart <- qname <- hap <- NULL

  hap1 <- read_and_flip(paste0(root, ".hap1.paf")) |>
    dplyr::mutate("hap" = 1, .before = 1)
  hap2 <- read_and_flip(paste0(root, ".hap2.paf")) |>
    dplyr::mutate("hap" = 2, .before = 1)

  return(rbind(hap1, hap2) |>
           dplyr::distinct() |>
           dplyr::arrange(tname, tstart) |>
           dplyr::mutate(qname = as.character(qname)) |>
           dplyr::mutate(qname = factor(qname, ordered = TRUE, levels = rev(unique(qname)))) |>
           dplyr::mutate(hap = factor(hap)))
}


#' Make the output of do_both_haps into chromosomes
#'
#' @param df A data frame with with at least:
#'  - tname
#'  - qlen
#'  - tstart
#'  - tend
#'  - flip
#'  - qlen
#'
#' @return A data frame with the contigs spanning the chromosomes with minimal gaps
#' @export
#'
#' @examples
#' \dontrun{ }
make_chr <- function(df) {
  qlen <- qname <- tstart <- tend <- flip <- start <- contiglen <- NULL
  df.split <- split(df, df$tname) |>
    lapply(\(x) dplyr::arrange(x, qlen) |>
             dplyr::group_by(qname) |>
             dplyr::summarize(start = min(tstart),
                              end = max(tend),
                              str = unique(flip)[1],
                              contiglen = max(qlen)) |>
             dplyr::arrange(start, contiglen)) |>
    lapply(minimize_gaps)

  return(do.call(rbind, df.split))
}

#' Wrapped to run the entire pipeline
#'
#' @param fname Root file name
#'
#' @return The oriented, minimal spanning contigs, taken from both haplotypes
#' @export
#'
#' @examples
#' \dontrun{ }
run_pipeline <- function(fname) {
  return(fname |>
           do_both_haps() |>
           make_chr())
}
