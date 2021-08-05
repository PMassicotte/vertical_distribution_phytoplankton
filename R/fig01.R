# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Figure 1: Map of the sampling stations with OWD.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# CTD data ----------------------------------------------------------------

ctd <- read_csv(here::here("data", "clean", "ctd.csv")) %>%
  distinct(station, transect, longitude, latitude, owd)


# Word map ----------------------------------------------------------------

ctd_df <- ctd %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

bbox_baffin_bay <- st_read(here("data", "raw", "bbox_baffin_bay.geojson"))

wm <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>%
  st_crop(bbox_baffin_bay)

# Transect labels ---------------------------------------------------------

transect <- tibble::tribble(
  ~transect, ~latitude, ~longitude,
  100L, 68.4991666666666, -56,
  200L, 68.75, -57.2,
  300L, 69.0004833333333, -56,
  400L, 68.1099166666667, -56.2,
  500L, 70.0002916666666, -56.2,
  600L, 70.5012833333333, -58,
  700L, 69.5008833333333, -57
)

# Plot --------------------------------------------------------------------

p <- ggplot() +
  geom_sf(data = wm, size = 0.1) +
  geom_sf(data = ctd_df, aes(color = owd), size = 2) +
  geom_text(
    data = transect,
    aes(
      x = longitude,
      y = latitude,
      label = transect
    ),
    inherit.aes = FALSE,
    size = 2.5,
    color = "#3c3c3c"
  ) +
  ggspatial::annotation_scale(
    location = "br",
    width_hint = 0.25,
    height = unit(0.1, "cm"),
    line_width = 0.25
  ) +
  ggspatial::annotation_north_arrow(
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    style = ggspatial::north_arrow_nautical(text_size = 6)
  ) +
  scale_colour_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    breaks = scales::breaks_pretty(n = 6),
    midpoint = 0,
    guide =
      guide_colorbar(
        barwidth = unit(6, "cm"),
        barheight = unit(0.2, "cm"),
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  labs(
    color = "Open water days (OWD)"
  ) +
  coord_sf(
    xlim = c(-68, -52),
    ylim = c(67, 71)
  ) +
  theme(
    legend.justification = c(0.5, 0.5),
    legend.position = c(0.5, 0.1),
    legend.background = element_blank(),
    axis.title = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#B4C6D8")
  )

filename <- here("graphs", "fig01.pdf")

ggsave(
  filename = filename,
  device = cairo_pdf,
  width = 6,
  height = 6
)

knitr::plot_crop(filename)
