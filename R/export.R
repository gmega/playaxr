.export_folder <- new.env()
.dpi <- 384

#' @export
set_export_folder <- function(path) {
  .export_folder$folder <- path
}

#' @export
export_plot <- function(a_plot, name, w = 1, h = 1, tee = TRUE) {
  attributes(a_plot) <- c(attributes(a_plot), name = name)
  export_plots(named_plots = l(a_plot), w = w, h = h)
  if (tee) a_plot else invisible(a_plot)
}

#' @export
export_plots <- function(named_plots, w = 1, h = 1) {
  for (named_plot in named_plots) {
    target <- file.path(.export_folder$folder,
                        g('{attributes(named_plot)$name}.png'))
    message(g('write {target}'))
    png(filename = target, width = w * .dpi, height = h * .dpi)
    print(named_plot)
    dev.off()
  }
}
