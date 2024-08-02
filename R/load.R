
#' Read a PAF file and flip to match reference orientation
#'
#' @param fname Root of file name
#'
#' @return A data frame with most of the helpful PAF columns retained
#' @export
#'
#' @examples
#' \dontrun{ }
read_and_flip <- function(fname) {
  # bind the NSE names to NULL
  qname <- tname <- alen <- len <- tstart <- strand <- prop <- flip <- qlen <- qstart <- qend <- qswap <- NULL

  paf <- pafr::read_paf(fname)

  paf.grouped <- paf |>
    dplyr::group_by(qname, tname) |>
    dplyr::summarize(len = sum(alen), .groups = "drop_last") |>
    dplyr::mutate(len = len/sum(len)) |>
    dplyr::ungroup() |>
    dplyr::filter(len > 0.01) |>
    dplyr::arrange(qname, dplyr::desc(len)) |>
    dplyr::distinct(qname, .keep_all = TRUE) |>
    dplyr::filter(! tname %in% c("MT", "Pltd"))

  paf.grouped.long <- paf.grouped |>
    dplyr::left_join(paf[, c("qname", "tname", "qstart", "qend", "tstart", "tend", "alen",
                             "strand", "qlen")],
                     by = c("qname", "tname")) |>
    dplyr::filter(alen > 100000) |>
    dplyr::arrange(tname, tstart) |>
    dplyr::mutate(qname = factor(qname, ordered = TRUE, levels = rev(unique(qname))))

  # determine overall orientation
  paf.grouped.long <- paf.grouped.long |>
    dplyr::group_by(qname) |>
    dplyr::summarize("prop" = sum(ifelse(strand == "+", alen, 0))/sum(alen)) |>
    dplyr::mutate("flip" = ifelse(prop < 1/2, TRUE, FALSE)) |>
    dplyr::select(qname, flip) |>
    dplyr::right_join(paf.grouped.long, by = "qname") |>
    dplyr::mutate(qstart = ifelse(flip, qlen - qstart, qstart),
                  qend = ifelse(flip, qlen - qend, qend)) |>
    dplyr::mutate(qswap = ifelse(flip, qstart, qend),
                  qstart = ifelse(flip, qend, qstart),
                  qend = qswap) |>
    dplyr::mutate(strand = ifelse(flip, ifelse(strand == "+", "-", "+"), strand)) |>
    dplyr::mutate(qstart2 = ifelse(strand == "-", qend, qstart),
                  qend2 = ifelse(strand == "-", qstart, qend))

  return(paf.grouped.long)
}
