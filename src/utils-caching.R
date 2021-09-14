library(grid)
library(readr)

save_figure <- function(plot, path, filename, p_width, p_height, units = "in",
                        margin = theme_gray()$plot.margin[1],
                        limitsize = FALSE, device = cairo_pdf, ...) {

  # Much of the following shamelessly plagiarized from set_panel_size() in the
  # egg package.
  g <- ggplotGrob(plot) # gtable

  panels <- grep("panel", g$layout$name)
  panel_index_w <- unique(g$layout$l[panels])
  panel_index_h <- unique(g$layout$t[panels])
  nw <- length(panel_index_w)
  nh <- length(panel_index_h)
  g$widths[panel_index_w] <- rep(unit(p_width, units), nw)
  g$heights[panel_index_h] <- rep(unit(p_height, units), nh)

  if (!is.null(path)) {
    dir.create(dirname(file.path(path, filename)),
      showWarnings = FALSE, recursive = TRUE
    )
    ggsave(file.path(path, filename), g, device,
      width = convertWidth(sum(g$widths) + margin,
        unitTo = "in", valueOnly = TRUE
      ),
      height = convertHeight(sum(g$heights) + margin,
        unitTo = "in", valueOnly = TRUE
      ),
      limitsize = limitsize, ...
    )
    if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
  }

  plot # NULL
}

save_object <- function(x, path, filename) {
  if (!is.null(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    saveRDS(x, file.path(path, filename))
  }
  x
}

save_table <- function(x, path, filename) {
  if (!is.null(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    # browser()
    write_csv(x, file.path(path, filename))
  }
  x
}

get_filename <- function(x, ext, ...) {
  sprintf("%s.%s", paste(gsub("\\.", "-", x), ..., sep = "-"), ext)
}
