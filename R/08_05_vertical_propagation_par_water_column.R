# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Propagate hourly PAR into the water column.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

hourly_par_sbdart <- read_csv(here::here("data/clean/sbdart_hourly_par_0p.csv"))
transmittance <- read_csv(here::here("data/clean/cops_kd_par_transmittance.csv"))

# Merge SBDART and COPS data based on geo positions -----------------------

hourly_par_sbdart <- hourly_par_sbdart %>%
  rename(station_sbdart = station) %>%
  select(-transect) %>%
  group_nest(station_sbdart, longitude, latitude, .key = "data_sbdart")

transmittance <- transmittance %>%
  rename(station_cops = station) %>%
  select(-transect) %>%
  group_nest(station_cops, longitude, latitude, .key = "data_transmittance")

hourly_par_sbdart
transmittance

# SBDAR (PAR0+) and COPS (KdPAR) data need to be merged. This will be done based
# on geographic coordinates and a maximum distance of 5 km.

df <- hourly_par_sbdart %>%
  fuzzyjoin::geo_left_join(
    transmittance,
    c("longitude", "latitude"),
    unit = "km",
    distance_col = "distance_between_sbdart_cops_km",
    max_dist = 5
  ) %>%
  group_by(station_sbdart, longitude.x, latitude.x) %>%
  filter(distance_between_sbdart_cops_km == min(distance_between_sbdart_cops_km)) %>%
  select(-contains(".y")) %>%
  ungroup() %>%
  rename_with(~ str_remove(., ".x"), contains(".x"))

df

# Calculate hourly PAR in the water column --------------------------------

df <- df %>%
  crossing(depth_m = 1:100) %>%
  relocate(depth_m, .before = 1) %>%
  unnest(data_sbdart) %>%
  unnest(data_transmittance)

df

df <- df %>%
  mutate(hourly_par_0m_umol_m2_s1 = hourly_par_0p_umol_m2_s1 * par_transmittance) %>%
  mutate(hourly_par_z_umol_m2_s1 = hourly_par_0m_umol_m2_s1 * exp(-kd_par * depth_m))

# Plot --------------------------------------------------------------------

p <- df %>%
  filter(depth_m <= 50) %>%
  ggplot(aes(x = hourly_par_z_umol_m2_s1, y = depth_m, group = hour)) +
  geom_line(aes(color = deployement), size = 0.1) +
  scale_y_reverse() +
  facet_wrap(~glue("Station SBDART: {station_sbdart}\nStation COPS: {station_cops}\nDistance: {round(distance_between_sbdart_cops_km, digits = 2)} km"), ncol = 6) +
  labs(
    title = "Vertical profiles of hourly PAR (Amundsen stations)",
    subtitle = str_wrap("SBDART (PAR 0+) and COPS (Kd, transmittance) were merged together based on their geographic coordinates. A maximum distance of 5 km was used to merge the data. Each vertical profile corresponds to hours of the days.", 140),
    x = bquote("Hourly PAR"~(mu*mol~m^{-2}~s^{-1})),
    y = "Depth (m)"
  ) +
  theme(
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    plot.subtitle = element_text(lineheight = 1.25)
  ) +
  paletteer::scale_color_paletteer_d(
    "ggthemes::wsj_rgby",
    guide = guide_legend(
      override.aes = list(size = 2),
      label.position = "top",
      keywidth = unit(4, "cm"),
      label.theme = element_text(size = 10, face = "bold")
    )
  )

ggsave(
  here::here("graphs/085_01_hourly_par_z_umol_m2_s1.pdf"),
  device = cairo_pdf,
  width = 12,
  height = 26
)

# Average by station ------------------------------------------------------

df <- df %>%
  relocate(starts_with("hourly"), .after = -1)

df <- df %>%
  group_by(station = station_sbdart, deployement, depth_m, hour) %>%
  summarise(
    hourly_par_z_umol_m2_s1 = mean(hourly_par_z_umol_m2_s1)
  ) %>%
  ungroup()

# Make sure we have 24 hours of data at each depth
df %>%
  count(station, deployement, depth_m) %>%
  assertr::verify(n == 24)

p <- df %>%
  filter(depth_m <= 50) %>%
  ggplot(aes(x = hourly_par_z_umol_m2_s1, y = depth_m, group = hour)) +
  geom_line(aes(color = deployement), size = 0.1) +
  scale_y_reverse() +
  facet_wrap(~station) +
  theme(
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    plot.subtitle = element_text(lineheight = 1.25)
  ) +
  labs(
    title = "Vertical profiles of hourly PAR (Amundsen stations)",
    x = bquote("Hourly PAR"~(mu*mol~m^{-2}~s^{-1})),
    y = "Depth (m)"
  ) +
  paletteer::scale_color_paletteer_d(
    "ggthemes::wsj_rgby",
    guide = guide_legend(
      override.aes = list(size = 2),
      label.position = "top",
      keywidth = unit(4, "cm"),
      label.theme = element_text(size = 10, face = "bold")
    )
  )

ggsave(
  here::here("graphs/085_02_ourly_par_z_umol_m2_s1_averaged_by_station.pdf"),
  device = cairo_pdf,
  width = 10,
  height = 10
)

# Export ------------------------------------------------------------------

df %>%
  write_csv(here::here("data/clean/propagated_hourly_par_water_column.csv"))
