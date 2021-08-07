# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Some cleanup of the CTD data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

ctd <- fread(here::here("data/raw/ctd.csv")) %>%
  as_tibble()

ctd <- ctd %>%
  filter(str_detect(mission, "amundsen")) %>%
  filter(str_detect(station, "^G\\d*")) %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100, .after = station) %>%
  filter(between(transect, 100, 700))

ctd

ctd %>%
  distinct(station)

# Select a subset variables needed for this study -------------------------

ctd <- ctd %>%
  select(
    station,
    transect,
    date_time,
    latitude,
    longitude,
    cast,
    depth_m,
    # starts_with("sig"),
    flor_mg_m3,
    tran_percent
  )

ctd

# Visualize the raw data --------------------------------------------------

# This is a subset of stations I will use to visualize the effect of
# preprocessing steps.

subset_stations <- c(108, 300, 107, 207, 501, 413, 301, 321, 702)

p <- ctd %>%
  # filter(station %in% subset_stations) %>%
  group_by(station) %>%
  mutate(n_cast = n_distinct(cast)) %>%
  ungroup() %>%
  mutate(station = glue("{station} (# of cast: {n_cast})")) %>%
  mutate(station = fct_reorder(
    station,
    cast,
    .fun = n_distinct,
    .desc = TRUE
  )) %>%
  ggplot(aes(
    x = flor_mg_m3,
    y = depth_m,
    group = cast,
    color = factor(cast)
  )) +
  geom_path(size = 0.1) +
  scale_y_reverse() +
  facet_wrap(~station, scales = "free") +
  theme(legend.position = "none")

ggsave(
  here("graphs/01_ctd_raw_fluorescence_vertical_profiles.pdf"),
  device = cairo_pdf,
  height = 20,
  width = 20
)

# Select the cast closest to the middle day -------------------------------

# TODO:
# 1 - Find a better strategy to deal with multiple casts at the same station.
# 2 - If we merge cast, which depth grid should be used?

# 195 stations/casts
ctd %>%
  distinct(station, date_time, cast) %>%
  add_count(station)

# After filtering, there are 136 stations (only 1 cast per station)
ctd <- ctd %>%
  mutate(middle_day_time = as.Date(date_time) + hms("12:00:00")) %>%
  mutate(timediff = abs(date_time - middle_day_time) ) %>%
  group_by(station) %>%
  filter(timediff == min(timediff)) %>%
  ungroup() %>%
  select(-middle_day_time, -timediff)

ctd

ctd %>%
  distinct(station, cast)

# Make sure there is now only 1 cast per station/day
ctd %>%
  distinct(station, date_time, cast) %>%
  add_count(station) %>%
  assertr::verify(n == 1)

# Smooth the vertical profiles --------------------------------------------

ctd <- ctd %>%
  # filter(depth_m <= 100) %>%
  group_by(station, cast) %>%
  mutate(across(
    c(flor_mg_m3, tran_percent),
    ~ RcppRoll::roll_median(., n = 25, fill = NA, na.rm = TRUE, align = "center"),
    .names = "roll_{.col}"
  )) %>%
  arrange(station, cast, depth_m) %>%
  ungroup()


# Check the effect of smoothing -------------------------------------------

p1 <- ctd %>%
  filter(station %in% subset_stations) %>%
  ggplot(aes(
    x = tran_percent,
    y = depth_m,
    group = interaction(station, cast)
  )) +
  geom_path(size = 0.25) +
  geom_path(
    aes(x = roll_tran_percent),
    color = "red",
    size = 0.25
  ) +
  scale_y_reverse() +
  facet_wrap(~ glue("station {station} (cast {cast})"), scales = "free", ncol = 3) +
  labs(
    x = "Transmittance (%)",
    y = "Depth (m)",
    title = "Transmittance"
  )

p2 <- ctd %>%
  filter(station %in% subset_stations) %>%
  ggplot(aes(
    x = flor_mg_m3,
    y = depth_m
  )) +
  geom_path(size = 0.25) +
  geom_path(aes(x = roll_flor_mg_m3), size = 0.25, color = "red") +
  scale_y_reverse() +
  facet_wrap(~ glue("station {station} (cast {cast})"), scales = "free", ncol = 3) +
  labs(
    x = "Fluorescence",
    y = "Depth (m)",
    title = "Fluorescence"
  )

p <- p1 / p2 +
  plot_annotation(
    tag_levels = "A",
    title = "Effect of vertical smoothing",
    theme = theme(
      plot.title = element_text(size = 30))
  ) &
  theme(plot.tag = element_text(face = "bold", size = 26))

ggsave(
  here("graphs/01_ctd_smoothed_fluorescence_vertical_profiles.pdf"),
  device = cairo_pdf,
  height = 20,
  width = 10
)

# Rename smoothed columns to the original ones ----------------------------

ctd <- ctd %>%
  select(-flor_mg_m3, -tran_percent) %>%
  rename(
    flor_mg_m3 = roll_flor_mg_m3,
    tran_percent = roll_tran_percent
  )

ctd

# Add OWD information -----------------------------------------------------

owd <- read_csv(
  "https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/Randelhoff-et-al-2019_GreenEdge_per-station_v1.0.csv",
  na = "NaN"
) %>%
  janitor::clean_names() %>%
  select(station, owd)

ctd <- ctd %>%
  inner_join(owd, by = "station")

ctd <- ctd %>%
  group_by(station, cast) %>%
  arrange(depth_m) %>%
  ungroup()

ctd

# Correct transmittance data ----------------------------------------------

# A lot of transmittance >= 100, problem! Email sent to Pascal Guillot to
# understand what is going on.

ctd %>%
  drop_na(tran_percent) %>%
  mutate(
    tran_class = case_when(
      tran_percent > 100 ~ "Transmittance above 100%",
      tran_percent <= 100 ~ "Transmittance below 100%",
      TRUE ~ NA_character_
    )
  ) %>%
  ggplot(aes(x = tran_percent)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~tran_class, scales = "free") +
  labs(
    title = "CTD transmittance",
    subtitle = glue("Range of transmittance {paste(range(ctd$tran_percent, na.rm = TRUE), collapse = ' - ')}")
  )

# Because of this, rescale the transmittance values. After a discussion with
# Pascal, he told me to re-scale the transmittance data between xxx-100%.

range(ctd$tran_percent, na.rm = TRUE)

mint <- min(ctd$tran_percent, na.rm = TRUE)

ctd <- ctd %>%
  mutate(tran_percent = scales::rescale(tran_percent, to = c(mint, 99.999)))

# Just the maximum value of the transmittance has changed
range(ctd$tran_percent, na.rm = TRUE)

# Particle beam attenuation coefficient (CP) ------------------------------

# ftp://ftp.nodc.noaa.gov/nodc/archive/arc0022/0001155/1.1/data/1-data/docs/PI-NOTES/arabian/Gardner-beamcp.htm

# Beam transmission was converted to beam attenuation coefficients using
# c=-(1/r)*ln(%Tr/100) where c=beam attenuation coefficient (m^-1), r=beam path length (m), and Tr=% beam transmission.

# Check Table 1 for an idea of the range of Cp.
# https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2015JC010878

# 25 cm beam, confirmed by Pascal Guillot
r <- 0.25

# From Pascal:

# Voici les infos (Thomas, un des techs de AS a contacté SeaBird):
#
# All the instruments listed have a wavelength of 657nm (using a red LED):
#
# CST-558DR – 657nm
# CST-671DR – 657nm
# CST-2021 – 657nm
# CST-2022 - 657nm

ctd <- ctd %>%
  mutate(cp = -(1 / r) * log10(tran_percent / 100))

fwrite(ctd, here("data/clean/ctd.csv"))

# Check that we have all the stations -------------------------------------

ctd %>%
  distinct(station, longitude, latitude, .keep_all = TRUE) %>%
  ggplot(aes(x = longitude, y = latitude, color = owd)) +
  geom_point(size = 2) +
  scale_color_viridis_c()

ctd %>%
  distinct(station, transect, longitude, latitude, depth_m) %>%
  ggplot(aes(x = longitude, y = depth_m, group = station)) +
  geom_line() +
  scale_y_reverse() +
  facet_wrap(~transect, scales = "free_x")

ctd %>%
  filter(transect == 200) %>%
  distinct(station, transect, longitude, latitude, depth_m) %>%
  ggplot(aes(x = longitude, y = depth_m)) +
  geom_line(aes(color = factor(station))) +
  scale_y_reverse() +
  facet_wrap(~transect, scales = "free_x")

ctd %>%
  count(station, depth_m) %>%
  assertr::verify(n == 1)
