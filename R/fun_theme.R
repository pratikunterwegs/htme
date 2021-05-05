
#' Custom theme with grey background
#'
#' @param grid Show panel grid major?
#' @param ... Arguments to theme_* from ggplot.
#'
#' @return None
#' @export
theme_custom <- function(grid = FALSE, border_size = 2.5, ...) {
  this_theme <-
    ggplot2::theme_test(...) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(size = border_size),
      legend.position = "none",
      axis.ticks.length = ggplot2::unit(1.5, units = "mm"),
        axis.title = element_blank(),
        axis.text.y = element_text(
          angle = 90,
          hjust = 0.5
        ),
      strip.background = ggplot2::element_rect(colour = NA,
                                      fill = "lightgrey")
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

  this_theme
}

