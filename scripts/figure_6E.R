#' ---
#' title: "Exploratory Data Analysis"
#' author: Pratik R Gupte
#' date: May 2021
#' output: html_document
#' editor_options:
#'   chunk_output_type: console
#' ---
#'

#' # Figure 6E: Egyptian Fruit Bat EDA

#+
# data processing
library(data.table)

# plotting
library(ggplot2)
library(ggspatial)
library(htme)

#' ## Prepare bat data
#'
#+
# get data
data <- fread("data/bat_data.csv")
# filter on SD
data[, SD := sqrt(VARX + VARY + (2 * COVXY))]
data <- data[SD < 20, ]
# get summary
data[, c("xround", "yround") := list(round(X, -2), round(Y, -2))]
data_summary <- data[, .N, by = c("xround", "yround")]

#' ## Read trees and roosts
#'
#+
# read from local files
trees <- fread("data/trees_update_clusters_Aug2020.csv")
roosts <- fread("data/Roosts.csv")

#' ## Save as Rdata
#'
#+
# save elements for plot as rdata
save(data_summary, trees, roosts,
     file = "data/data_fig_6E.Rdata")

# load saved Rdata
load("data/data_fig_6E.Rdata")

#' ## Make Figure 6E
#'
#+ save as object
fig_6E <-
  ggplot(data_summary) +
  geom_point(
    data = trees,
    aes(X, Y,
      col = "trees",
      shape = "trees"
    ),
    size = 0.05
  ) +
  geom_tile(aes(xround, yround,
    fill = N
  ),
  alpha = 0.8
  ) +
  geom_point(
    data = roosts,
    aes(X, Y,
      col = "Roost",
      shape = "Roost"
    ),
    size = 2,
    stroke = 0.6
  ) +
  scale_colour_manual(
    values = c(
      "trees" = "seagreen",
      "Roost" = "black"
    ),
    labels = c("Roost", "Tree")
  ) +
  scale_shape_manual(
    values = c(
      "Roost" = 5,
      "trees" = 16
    ),
    labels = c("Roost", "Tree")
  ) +
  scale_fill_viridis_c(
    option = "F",
    breaks = c(10, 100, 1000),
    labels = c("100", "1,000", "10,000"),
    trans = "log10",
    direction = -1
  ) +
  guides(shape = guide_legend(override.aes = list(size = 2, 1))) +
  scale_y_continuous(breaks = c(33.05, 33.10)) +
  scale_x_continuous(breaks = c(35.6)) +
  annotation_scale(
    bar_cols = c("grey20"),
    height = unit(1, units = "mm"),
    style = "ticks",
    line_width = 1,
    text_face = "bold",
    line_col = "grey20",
    text_col = "grey20",
    width_hint = 0.175,
    location = "br"
  ) +
  ggplot2::coord_sf(
    crs = 2039,
    xlim = range(data$X),
    ylim = range(data$Y),
    expand = F
  ) +
  labs(
    fill = "Fixes",
    shape = NULL,
    colour = NULL
  )+
  htme::theme_custom(base_size = 7, base_family = "Arial")+
  theme(
    legend.position = c(0.85, 0.75),
    legend.text.align = 1,
    legend.key.width = unit(2, units = "mm"),
    legend.key.height = unit(3, units = "mm"),
    axis.text = element_text(
      face = "bold"
    )
  )

#+ echo=FALSE
fig_6E

#' ## Save Figure 6E

#+
# save plots
ggsave(
  fig_6E,
  dpi = 350,
  filename = "figures/fig_6E.png",
  height = 3, width = 3
)

#' Show Figure 6E
#'
#'
