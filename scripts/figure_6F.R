#' ---
#' title: "Data Cleaning"
#' author: Pratik R Gupte
#' date: May 2021
#' output: html_document
#' editor_options:
#'   chunk_output_type: console
#' ---
#'

#' # Figure 6F: Red Knot Data Cleaning
#'
# data processing
library(data.table)
library(atlastools)
library(sf)

# plotting
library(ggplot2)
library(ggspatial)
library(magick)

#' ## Get data
# get data
data <- fread("data/whole_season_tx_0435.csv")
# convert time to seconds from ms
data[, time := round(TIME / 1000)]
data[, time := as.POSIXct(time, tz = "Amsterdam", origin = "1970-01-01")]

#' ## Filter data
# filter out before specific time
data_filter <- atl_filter_covariates(
  data = data,
  filters = "time > '2018-08-19 18:31:00 Amsterdam'"
)
# select one day after start time
data_filter <- atl_filter_covariates(
  data = data_filter,
  filters = c("time < (time[1] + (18 * 3600))")
)

#' ## Basic metrics
# add speed
data_filter[, c(
  "speed_in", "speed_out",
  "angle"
) := list(
  atl_get_speed(data_filter,
                x = "X", y = "Y", time = "time",
                type = "in"
  ),
  atl_get_speed(data_filter,
                x = "X", y = "Y", time = "time",
                type = "out"
  ),
  atl_turning_angle(data_filter, x = "X", y = "Y", time = "time")
)]

#' ## Filter sd and speed
# filter with atlastools
data_filter_sd_speed <- atl_filter_covariates(
  data = data_filter,
  filters = c(
    "SD < 100",
    "((speed_in < 20 & speed_out < 20))"
  )
)

#' ## Median smooth
#'
data_smooth <- copy(data_filter_sd_speed)

atl_median_smooth(
  data = data_smooth,
  x = "X", y = "Y", time = "TIME", moving_window = 11
)

#' ## Save as Rdata
save(data_filter, data_smooth, file = "data/data_figure_6F.Rdata")

#' ## Make figure 6F
#'
# set limits
xlim <- c(650600, 650850)
ylim <- c(5901815, 5902000)

legend_label <- c(
  "raw" = "Raw data",
  "outliers" = "Unrealistic\nmovement",
  "filtered" = "Filtered data",
  "smooth" = "Median smooth\n(K = 11)"
)

# explore raw
fig_6F =
ggplot() +
  geom_path(
    data = data_filter[time < time[1] + 12*3600 &
                         time > time[1] + 4*3600,],
    aes(X, Y,
        col = "raw"
    ),
    size = 0.3
  ) +
  geom_path(
    data = data_smooth[time < time[1] + 12*3600 &
                         time > time[1] + 4*3600,],
    aes(X, Y,
        col = "smooth"
    )
    # size = 0.3
  ) +
  scale_size_area() +
  scale_colour_manual(
    name = NULL,
    values = c(
      "raw" = "grey",
      "outliers" = "red",
      "filtered" = "seagreen",
      "smooth" = "slateblue"
    ),
    labels = legend_label,
    breaks = names(legend_label)
  ) +
  scale_shape_manual(
    name = NULL,
    values = c(
      "raw" = NA,
      "outliers" = 4,
      "filtered" = 1,
      "smooth" = NA
    ),
    labels = legend_label,
    breaks = names(legend_label)
  ) +
  scale_y_continuous(
    breaks = c(53.245)
  ) +
  coord_sf(
    xlim = xlim, ylim = ylim,
    crs = 32631
  ) +
  annotation_scale(
    bar_cols = c("grey20"),
    height = unit(1, units = "mm"),
    style = "ticks",
    line_width = 1,
    text_face = "bold",
    line_col = "grey20",
    text_col = "grey20",
    width_hint = 0.175
  )+
  htme::theme_custom(base_size = 7, base_family = "Arial")+
  theme(
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(
      fill = "white",
      color = NA
    )
  )

# save image as prelim
ggsave(
  fig_6F,
  filename = "figures/fig_6F.png",
  height = 3, width = 4
)

#' ## Add red knot
#'
# get figure
im <- image_read("figures/fig_6F.png")

# get red knot
knot <- image_read("figures/knots/knot_poster_picture.png")

# combine knot and figure
im <-
  im %>%
  image_composite(
    image_scale(knot, "60%x"),
    operator = "Atop",
    gravity = "SouthWest",
    offset = "+100+120"
  )

# write
image_write(
  im,
  path = "figures/fig_6F.png"
)
