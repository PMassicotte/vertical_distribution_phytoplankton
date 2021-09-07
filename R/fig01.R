



# Stations ----------------------------------------------------------------

stations <- read_csv(here::here("data", "clean", "ctd.csv")) %>%
  distinct(station, transect, longitude, latitude, owd)

stations %>%
  count(station, sort = TRUE)

# SIC ---------------------------------------------------------------------

sic <- read_csv(here(
  "data",
  "clean",
  "ice_concentration_history_amundsen.csv"
)) %>%
  select(station, date = date_utc, ice_date, sic) %>%
  filter(str_starts(station, "G")) %>%
  mutate(station = parse_number(station)) %>%
  filter(date == ice_date) %>%
  distinct(station, .keep_all = TRUE)

sic

sic %>%
  count(station, sort = TRUE)

sic <- sic %>%
  mutate(is_open_water = case_when(
    sic <= 0.15 ~ "Open water",
    sic > 0.15 ~ "Ice covered"
  ))

# Merge stations and sic --------------------------------------------------

stations <- stations %>%
  inner_join(sic, by = "station")

# Word map ----------------------------------------------------------------

baffin <- ne_countries(scale = "large", returnclass = "sf") %>%
  st_crop(c(
    xmin = -65,
    ymin = 60,
    xmax = -45,
    ymax = 85
  )) %>%
  st_transform(crs = "+proj=stere +lat_0=90 +lat_ts=75 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  as("Spatial")

bb <- raster::raster(here("data/raw/IBCAO_V3_500m_RR.tif")) %>%
  raster::crop(baffin)

bb2 <- bb %>%
  raster::sampleRegular(size = 1e4, asRaster = TRUE) %>%
  raster::projectRaster(crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  raster::rasterToPoints() %>%
  as_tibble() %>%
  rename(z = 3)

wm <-
  ne_download(
    scale = "large",
    type = "countries",
    returnclass = "sf"
  )

# Map ---------------------------------------------------------------------

p <- bb2 %>%
  mba.surf(no.X = 600, no.Y = 600, sp = TRUE) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(xyz.est.z = ifelse(xyz.est.z >= 0, 0, xyz.est.z)) %>%
  ggplot(aes(xyz.est.x, xyz.est.y, fill = xyz.est.z, z = xyz.est.z)) +
  ggisoband::geom_isobands(bins = 25, color = NA) +
  scale_color_viridis_c(
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    labels = scales::label_percent(),
    guide = guide_colorbar(
      barwidth = unit(2, "cm"),
      barheight = unit(0.1, "cm"),
      direction = "horizontal",
      title.position = "top",
      order = 1
    )
  ) +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Blue",
    direction = -1,
    limits = c(-2500, 0),
    oob = scales::squish,
    breaks = -c(0, 1000, 2000, 3000),
    guide = guide_colorbar(
      barwidth = unit(2, "cm"),
      barheight = unit(0.1, "cm"),
      direction = "horizontal",
      title.position = "top",
      order = 2
    )
  ) +
  scale_shape(guide = guide_legend(
    ncol = 1,
    title = element_blank(),
    override.aes = list(size = 1.5),
    order = 3
  )) +
  geom_sf(
    data = wm,
    size = 0.1,
    inherit.aes = FALSE,
    fill = "white"
  ) +
  coord_sf(
    xlim = c(-70.5, -44),
    ylim = c(65, 72)
  ) +
  annotate(
    geom = "text",
    x = -63.5,
    y = 67.5,
    label = "Qikiqtarjuaq",
    vjust = -0.25,
    hjust = 0,
    size = 2,
    family = "Poppins"
  ) +
  annotate(
    geom = "text",
    x = -50,
    y = 71,
    label = "Greenland",
    vjust = 0,
    hjust = 0,
    size = 3,
    family = "Poppins",
    fontface = 2
  ) +
  annotate(
    geom = "text",
    x = -68,
    y = 71.5,
    label = "Baffin Bay",
    vjust = 0,
    hjust = 0,
    size = 3,
    family = "Poppins",
    fontface = 2
  ) +
  annotate(
    geom = "text",
    x = -71,
    y = 67,
    label = "Baffin Island",
    vjust = 0,
    hjust = 0,
    size = 3,
    family = "Poppins",
    fontface = 2
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.08,
    height = unit(0.1, "cm"),
    line_width = 0.1,
    text_cex = 0.5
  ) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    height = unit(0.75, "cm"),
    width = unit(0.75, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book",
      text_size = 6,
      line_width = 0.5
    )
  ) +
  geom_point(
    data = stations,
    aes(
      x = longitude,
      y = latitude,
      color = sic,
      shape = is_open_water
    ),
    size = 1,
    inherit.aes = FALSE
  ) +
  annotate("point",
    x = -63.78953333,
    y = 67.47973333,
    size = 0.5
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Depth (m)",
    color = "Sea ice concentration"
  ) +
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.97, 0.2),
    legend.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "#B9DDF1"),
    legend.key = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.title = element_text(size = 6, family = "Exo"),
    legend.text = element_text(size = 6, family = "Exo"),
    legend.key.size = unit(0.25, "cm"),
    legend.margin = margin(t = 0, unit = "cm")
  )

filename <- here("graphs/fig01.pdf")

ggsave(
  filename,
  device = cairo_pdf,
  width = 12,
  height = 12 / 1.229081,
  units = "cm"
)

knitr::plot_crop(filename)
