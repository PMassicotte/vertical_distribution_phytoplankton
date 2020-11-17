# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Quenching correction for in vivo chla fluorescence measured by
# the CTD.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

ctd <- fread("data/raw/ctd.csv") %>%
  as_tibble() %>%
  filter(mission == "amundsen_2016") %>%
  filter(str_starts(station, "G")) %>%
  select(
    station,
    cast,
    date_time,
    longitude,
    latitude,
    depth_m,
    flor_mg_m3,
    sigt_kg_m3
  ) %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100, .after = station)

ctd

# Convert UTC time to local time ------------------------------------------

ctd <- ctd %>%
  mutate(
    date_time_local = format(date_time, tz = "UTC-4"),
    .after = date_time
  ) %>%
  mutate(date_time_local = parse_datetime(date_time_local))

ctd %>%
  distinct(date_time_local) %>%
  count(hour = lubridate::hour(date_time_local)) %>%
  ggplot(aes(x = hour, y = n)) +
  geom_col()

# Determine if the measurement was done during day or night ---------------

# Note: There is no such thing as sunrise/sunset in the Baffin Bay at this time
# of the year.

#TODO: xxx
sunpos <- ctd %>%
  distinct(date_time, longitude, latitude) %>%
  # slice(1) %>%
  rowwise() %>%
  mutate(suntimes = list(suncalc::getSunlightPosition(
    date = date_time,
    lat = latitude,
    lon = longitude
  ))) %>%
  unnest(suntimes) %>%
  mutate(altitude_degrees = units::as_units(altitude, "radians")) %>%
  mutate(altitude_degrees = units::set_units(altitude_degrees, "degrees")) %>%
  mutate(altitude_degrees = as.numeric(altitude_degrees))

p <- sunpos %>%
  ggplot(aes(x = altitude_degrees)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Sun altitude when the CTD was deployed",
    subtitle = str_wrap("Altitude above the horizon in degrees.", 100),
    y = "Number of observations"
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  here::here("graphs/10_01_histogram_sun_altitude.pdf"),
  device = cairo_pdf,
  width = 7.44,
  height = 5.2
)

# Rolling median ----------------------------------------------------------

n_pts <- 11

ctd <- ctd %>%
  group_by(station, cast, date_time, longitude, latitude) %>%
  mutate(across(
    c(flor_mg_m3, sigt_kg_m3),
    ~ RcppRoll::roll_median(.x, n = n_pts, fill = NA, align = "left"),
    .names = "{.col}_rollmedian"
  ))

ctd

plot_function <- function(df, station) {
  p <- df %>%
    # drop_na() %>%
    ggplot(aes(x = flor_mg_m3, y = depth_m, group = cast)) +
    geom_point(color = "grey75") +
    geom_path(aes(x = flor_mg_m3_rollmedian)) +
    scale_y_reverse() +
    facet_wrap(~glue("Cast: {cast}")) +
    labs(
      title = glue("Station: {station}"),
      subtitle = str_wrap("The grays dots are the raw data whereas the black lines represent a rolling median (n = 11).", 80)
    ) +
    theme(
      panel.border = element_blank(),
      axis.ticks = element_blank()
    )

  return(p)
}

# Find the MLD ------------------------------------------------------------

# https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/Randelhoff-et-al-2019_GreenEdge_per-station_v1.0.csv

mld <- ctd %>%
  ungroup() %>%
  select(station, cast, depth_m, sigt_kg_m3_rollmedian) %>%
  group_by(station, cast) %>%
  mutate(density_at_10_m = sigt_kg_m3_rollmedian[which.min(abs(depth_m - 10))]) %>%
  mutate(density_potential = abs(sigt_kg_m3_rollmedian - density_at_10_m)) %>%
  slice(which.min(abs(density_potential - 0.03))) %>%
  ungroup() %>%
  mutate(mld = 0.9 * depth_m) %>%
  select(station, cast, mld)

ctd %>%
  # ungroup() %>%
  anti_join(mld, by = c("station", "cast")) %>%
  distinct(station, cast)

df <- ctd %>%
  inner_join(mld)

# df <- df %>%
#   mutate(mld = ifelse(mld < min(depth_m, na.rm = TRUE), min(depth_m, na.rm = TRUE), mld))

# Determine is the CTD profile was performed during daytime ---------------

#TODO: xxx

# Extrapolate fluorescence within MLD * 0.9 -------------------------------

# 4) The method is simple, you find the maximal fluorescence within MLD*0.9, and extrapolate the maximal values towards surface.

# Find the maximum fluorescence in the MLD layer
df <- df %>%
  drop_na(flor_mg_m3_rollmedian) %>%
  group_by(station, transect, cast) %>%
  mutate(max_fluorescence_in_mld = max(flor_mg_m3_rollmedian[between(depth_m, 0, mld)],
    na.rm = TRUE
  ))

# Find the depth at which the this max fluorescence is
df <- df %>%
  mutate(max_fluorescence_depth_in_mld = depth_m[which.max(flor_mg_m3_rollmedian[between(depth_m, 0, mld)])])

# Propagate the maximum fluorescence from the depth found in the MLD up to the
# surface
df <- df %>%
  mutate(
    flor_mg_m3_rollmedian_npq_corrected = if_else(
      between(depth_m, 0, max_fluorescence_depth_in_mld),
      max_fluorescence_in_mld,
      flor_mg_m3_rollmedian
    )
  )

df

plot_function2 <- function(df, station) {
  p <- df %>%
    ggplot(aes(x = flor_mg_m3_rollmedian, y = depth_m)) +
    geom_point(aes(x = flor_mg_m3, color = "Raw data"), key_glyph = "rect") +
    geom_path(aes(color = "Rolling median"), size = 1) +
    geom_path(aes(
      x =
        flor_mg_m3_rollmedian_npq_corrected,
      color = "NPQ corrected"
    ),
    size = 0.25
    ) +
    geom_hline(aes(yintercept = mld), lty = 2, size = 0.25) +
    scale_y_reverse() +
    facet_wrap(~ glue("Cast: {cast}"), scales = "free") +
    labs(
      title = glue("Station: {station}"),
      subtitle = glue("- The horizontal dashed line corresponds to 0.9*MLD.\n- Rolling median with k = {n_pts}."),
      x = bquote("Rollmedian CTD fluorescence"~(mg~m^{-3})),
      y = "Depth (m)"
    ) +
    theme(
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      legend.title = element_blank(),
      legend.position = "top"
    ) +
    guides(color = guide_legend(
      override.aes = list(size = 3),
      title.position = "top"
    )) +
    scale_color_manual(
      values = c(
        "NPQ corrected" = "red",
        "Raw data" = "gray75",
        "Rolling median" = "#3c3c3c"
      )
    )

  return(p)
}

pp <- df %>%
  filter(depth_m <= 100) %>%
  ungroup() %>%
  group_nest(station) %>%
  mutate(p = map2(data, station, plot_function2))

pdf(
  here::here("graphs/10_02_ctd_fluorescence_npq_corrected.pdf"),
  width = 6,
  height = 5
)

walk(pp$p, print)

dev.off()

# Export ------------------------------------------------------------------

pp %>%
  select(-p) %>%
  unnest(data) %>%
  select(-max_fluorescence_in_mld, -max_fluorescence_depth_in_mld) %>%
  write_csv(here::here("data/clean/ctd_fluorescence_npq_corrected.csv"))
