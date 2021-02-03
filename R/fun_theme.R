
#' Custom theme with grey background
#'
#' @param grid Show panel grid major?
#' @param ... Arguments to theme_* from ggplot.
#'
#' @return None
#' @export
theme_custom <- function(grid = FALSE, ...) {
  this_theme <- ggplot2::theme_test(...) +
    ggplot2::theme(
      legend.position = "none",
      axis.ticks.length = ggplot2::unit(1.5, units = "mm"),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(
        colour = "black",
        size = 0.2),
      strip.background = ggplot2::element_rect(colour = NA,
                                      fill = "lightgrey")
      # panel.background = ggplot2::element_rect(
      #   fill = "grey99"
      # )
    )

  if (grid) {
    this_theme <- this_theme +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(
          colour = "grey70",
          size = 0.2
        )
      )
  }

  return(this_theme)
}

