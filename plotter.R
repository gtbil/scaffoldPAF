devtools::load_all()

x <-  do_both_haps("./tests/testthat/testdata/hard")

root_ <- "~/Documents/projects/assemblies/pafs/13_PIMA-S7"
root_ <- "~/Documents/projects/assemblies/pafs/6_PD4461Q"
root_ <- "~/Documents/projects/assemblies/pafs/17_GB-289"
x <- do_both_haps(root_)

ggplot2::ggplot(x,
                ggplot2::aes(x = qname,
                             y = tstart,
                             yend = tend,
                             col = hap)) +
  ggplot2::geom_segment(linewidth = 3, alpha = 0.8) +
  ggplot2::scale_color_manual(values = c("yellow", "pink")) +
  ggplot2::facet_wrap(~tname, nrow = 2, scales = "free") +
  ggplot2::theme_linedraw() +
  ggplot2::theme(axis.text.x= ggplot2::element_blank(),
                 axis.ticks.x= ggplot2::element_blank(),
                 axis.text.y= ggplot2::element_blank(),
                 axis.ticks.y= ggplot2::element_blank(),
                 panel.spacing.x = ggplot2::unit(0.1, "lines"),
                 panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(fill = "grey20"))

x2 <- run_pipeline(root_)

x3 <- x |>
  dplyr::filter(as.character(qname) %in% as.character(x2$qname))

ggplot2::ggplot(x3,
                ggplot2::aes(x = qname,
                             y = tstart,
                             yend = tend,
                             col = hap)) +
  ggplot2::geom_segment(linewidth = 3, alpha = 0.8) +
  ggplot2::scale_color_manual(values = c("yellow", "pink")) +
  ggplot2::facet_wrap(~tname, nrow = 2, scales = "free") +
  ggplot2::theme_linedraw() +
  ggplot2::theme(axis.text.x= ggplot2::element_blank(),
                 axis.ticks.x= ggplot2::element_blank(),
                 axis.text.y= ggplot2::element_blank(),
                 axis.ticks.y= ggplot2::element_blank(),
                 panel.spacing.x = ggplot2::unit(0.1, "lines"),
                 panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(fill = "grey20"))
