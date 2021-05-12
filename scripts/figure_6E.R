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
library(sf)

# plotting
library(ggplot2)
library(ggspatial)
library(OpenStreetMap)
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

#' ## Convert coordinates
data_summary = st_as_sf(data_summary, coords = c("xround", "yround"),
                        crs= 2039)
data_summary = cbind(
  st_drop_geometry(data_summary),
  round(
    st_coordinates(
      st_transform(data_summary, crs = 4326)
  ), 3)
)

# sum again
setDT(data_summary)
data_summary = data_summary[, .(N = mean(N)), by = c("X", "Y")]

#' ## Get basemap
basemap = openmap(
  upperLeft = c(max(data_summary$Y), min(data_summary$X)),
  lowerRight = c(min(data_summary$Y), max(data_summary$X)),
  type = "bing"
)

# project basemap
basemap = openproj(basemap, projection = st_crs(4326)$proj4string)

# convert all to transparency
# set transparency level
alphalevel = 0.3
basemap$tiles[[1]]$colorData = alpha(basemap$tiles[[1]]$colorData, alphalevel)

#' ## Read trees and roosts
#'
#+
# read from local files
trees <- fread("data/trees_update_clusters_Aug2020.csv")
roosts <- fread("data/Roosts.csv")

# transform to wgs84
trees = st_as_sf(trees, coords = c("X", "Y"), crs = 2039) %>%
  st_transform(4326)
trees = cbind(st_drop_geometry(trees), st_coordinates(trees))
roosts = st_as_sf(roosts, coords = c("X", "Y"), crs = 2039) %>%
  st_transform(4326)
roosts = cbind(st_drop_geometry(roosts), st_coordinates(roosts))
# remake data summary
# data_summary <- data[, .N, by = c("xround", "yround")]

#' ## Save as Rdata
#'
#+
# save elements for plot as rdata
save(data_summary, trees, roosts,
     file = "data/data_fig_6E.Rdata",
     basemap)

# load saved Rdata
load("data/data_fig_6E.Rdata")

#' ## Make Figure 6E
#'
#+ save as object
fig_6E <-
autoplot(basemap)+
# ggplot()+
  geom_point(
    data = trees,
    aes(X, Y,
        col = "trees",
        shape = "trees"
    ),
    stroke = 0.3
  )+
  geom_tile(
    data = data_summary,
    aes(X, Y,
        fill = N
    )
  )+
  geom_point(
    data = roosts,
    aes(X, Y,
      col = "Roost",
      shape = "Roost"
    ),
    size = 2
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
      "trees" = 2
    ),
    labels = c("Roost", "Tree")
  ) +
  scale_fill_gradientn(
    colours = pals::jet(25),
    # option = "F",
    breaks = c(10, 100, 1000),
    labels = c("100", "1,000", "10,000"),
    trans = "log10"
    # direction = -1
  ) +
  guides(shape = guide_legend(override.aes = list(size = 2, 1))) +
  scale_y_continuous(
    breaks = seq(33.05, 33.12, 0.03),
    labels = function(x) sprintf("%.3f°N", x)
  ) +
  scale_x_continuous(
    breaks = seq(35.57, 35.63, 0.02),
    labels = function(x) sprintf("%.3f°E", x)) +
  annotation_scale(
    bar_cols = c("grey20"),
    height = unit(1, units = "mm"),
    style = "ticks",
    line_width = 1,
    text_face = "bold",
    line_col = "grey20",
    text_col = "grey20",
    width_hint = 0.175,
    location = "tl"
  ) +
  ggplot2::coord_sf(
    crs = 4326,
    xlim = range(data_summary$X),
    ylim = range(data_summary$Y),
    expand = F
  ) +
  labs(
    fill = "Fixes",
    shape = NULL,
    colour = NULL
  ) +
  htme::theme_custom(base_size = 7, base_family = "Arial")+
  theme(
    legend.position = c(0.85, 0.8),
    legend.background = element_blank(),
    legend.key = element_rect(
      fill = NA
    ),
    legend.text = element_text(
      size = 10.5
    ),
    legend.title = element_text(
      size = 10.5
    ),
    legend.text.align = 1,
    legend.key.width = unit(2, units = "mm"),
    legend.key.height = unit(3, units = "mm"),
    axis.text = element_text(
      size = 10,
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
  height = 6, width = 4.5
)

#' Show Figure 6E
#'
#'
