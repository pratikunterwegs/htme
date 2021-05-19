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
library(OpenStreetMap)
library(htme)

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

#' ## Convert to geographic coordinates
#'
# write a custom function here to wrap st_transform
add_lat_long = function(df) {
  coords = st_as_sf(df[, c("X", "Y")],
                    coords = c("X", "Y"),
                    crs = 32631) %>%
    st_transform(4326) %>%
    st_coordinates()
  colnames(coords) = c("long", "lat")
  df <- cbind(df, coords)
  df
}

# apply function to data
data_filter = add_lat_long(data_filter)
data_filter_sd_speed = add_lat_long(data_filter_sd_speed)
data_smooth = add_lat_long(data_smooth)

#' ## Make figure 6F
#'
# set limits
xlim <- c(650620, 650950)
ylim <- c(5901790, 5902100)

#' ### Get basemap
#'
#' First get coordinates from xlim and ylim, in WGS84.

basemap_extent = st_coordinates(
  st_as_sf(
    as.data.frame(cbind(xlim, ylim)),
    coords = c("xlim", "ylim"),
    crs = 32631
  ) %>%
    st_transform(4326)
) %>%
  as.data.frame()

basemap = openmap(
  upperLeft = c(max(basemap_extent$Y), min(basemap_extent$X)),
  lowerRight = c(min(basemap_extent$Y), max(basemap_extent$X)),
  type = "bing"
)

# project basemap
basemap = openproj(basemap, projection = st_crs(4326)$proj4string)

# convert all to transparency
# set transparency level
alphalevel = 0.3
basemap$tiles[[1]]$colorData = alpha(basemap$tiles[[1]]$colorData, alphalevel)

#' ## Save as Rdata
save(data_filter, data_filter_sd_speed, data_smooth, basemap_extent,
     basemap, file = "data/data_fig_6F.Rdata")

legend_label <- c(
  "raw" = "Raw data",
  "outliers" = "Unrealistic movement",
  "filtered" = "Filtered data",
  "smooth" = "Median smooth (K = 11)"
)

# explore raw
fig_6F =
autoplot(basemap)+
# ggplot()+
  geom_path(
    data = data_filter[!data_filter_sd_speed, on = c("time")
                       ][time < time[1] + 7*3600 &
                         time > time[1] + 5*3600,],
    aes(
      long, lat,
      colour = "outliers",
      shape = "outliers"
    )
  )+
  geom_point(
    data = data_filter_sd_speed[time < time[1] + 9*3600 &
                         time > time[1] + 5*3600,],
    aes(long, lat,
        col = "filtered",
        shape = "filtered"
    ),
    size = 0.5
  )+
  geom_path(
    data = data_smooth[time < time[1] + 9*3600 &
                         time > time[1] + 5*3600,],
    aes(long, lat,
        col = "smooth",
        shape = "smooth"
    )
  ) +
  scale_shape_manual(
    name = NULL,
    values = c(
      "outliers" = NA,
      "filtered" = 4,
      "smooth" = NA
    ),
    labels = legend_label,
    breaks = names(legend_label)
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "outliers" = "darkorange",
      "filtered" = "mediumseagreen",
      "smooth" = "dodgerblue4"
    ),
    labels = legend_label,
    breaks = names(legend_label)
  ) +
  scale_y_continuous(
    breaks = seq(53.245, 53.250, 0.001),
    labels = function(x) sprintf("%.3f°N", x)
  ) +
  scale_x_continuous(
    breaks = seq(5.258, 5.262, 0.002),
    labels = function(x) sprintf("%.3f°E", x)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        linetype = c(1, 0, 1)
      )
    )
  )+
  ggplot2::coord_sf(
    crs = 4326,
    xlim = basemap_extent$X,
    ylim = basemap_extent$Y,
    expand = F
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
    legend.position = c(0.7, 0.85),
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
    legend.text.align = 0,
    legend.key.width = unit(5, units = "mm"),
    legend.key.height = unit(5, units = "mm"),
    axis.text = element_text(
      size = 10,
      face = "bold"
    )
  )

# view figure
fig_6F

# save image as prelim
ggsave(
  fig_6F,
  dpi = 350,
  filename = "figures/fig_6F.png",
  height = 4, width = 4
)

#' ## Add red knot
#'
# get figure
im <- image_read("figures/fig_6F.png")

# get red knot
knot <- image_read("figures/knots/knot_poster_picture.png") %>%
  image_flop()

# combine knot and figure
im <-
  im %>%
  image_composite(
    image_scale(knot, "80%x"),
    operator = "Atop",
    gravity = "NorthEast",
    offset = "+100+350"
  )

# write
image_write(
  im,
  path = "figures/fig_6F.png"
)
