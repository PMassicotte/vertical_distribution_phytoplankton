# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Tidy the UVP data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

uvp <- read_csv(here("data","raw","greenedge_uvp_particles.csv")) %>%
  filter(mission == "amundsen_2016")

uvp

uvp %>%
  distinct(sample_type)

uvp %>%
  distinct(site)

uvp <- uvp %>%
  select(
    station = site,
    date,
    date_time,
    depth_m,
    particle_size_range,
    count_per_liter,
    biovolume_ppm,
    longitude,
    latitude
  )

# Select only station that start with gxxx
uvp <- uvp %>%
  filter(str_starts(station, "g\\d{3}")) %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100, .after = station) %>%
  filter(between(transect, 100, 700))

uvp %>%
  distinct(station) %>%
  pull(station)

uvp

uvp %>%
  distinct(particle_size_range) %>%
  pull()

# Visualize the geographic positions --------------------------------------

uvp %>%
  filter(depth_m == 2.5) %>%
  distinct(station, longitude, latitude) %>%
  add_count(station) %>%
  filter(n > 1) %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point() +
  facet_wrap(~station, scales = "free")

uvp %>%
  filter(depth_m == 2.5) %>%
  distinct(station, longitude, latitude) %>%
  add_count(station) %>%
  filter(n > 1) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  group_by(station) %>%
  mutate(distance = list(as.numeric(st_distance(geometry, geometry)))) %>%
  unnest(distance) %>%
  filter(distance != 0) %>%
  as_tibble() %>%
  distinct(station, distance) %>%
  ggplot(aes(x = station, y = distance)) +
  geom_point() +
  facet_wrap(~station, scales = "free")

# Select the cast closest to the middle day -------------------------------

# Looks like there are more than 1 measurement by station. It is because the
# long/lat coordinates change slightly for a same station, probably reflecting
# that there were many cast at each station.

uvp %>%
  dtplyr::lazy_dt() %>%
  count(station, transect, depth_m, particle_size_range, sort = TRUE) %>%
  as_tibble()

uvp %>%
  distinct(station)

uvp <- uvp %>%
  mutate(middle_day_time = as.Date(date_time) + hms("12:00:00")) %>%
  mutate(timediff = abs(date_time - middle_day_time)) %>%
  group_by(station) %>%
  filter(timediff == min(timediff)) %>%
  ungroup() %>%
  select(-middle_day_time, -timediff)

uvp %>%
  distinct(station, date_time) %>%
  mutate(h = hms::as_hms(date_time)) %>%
  mutate(station = fct_reorder(as.character(station), h)) %>%
  ggplot(aes(x = station, y = h)) +
  geom_point() +
  geom_hline(yintercept = hms::as_hms("12:00:00"), lty = 2, color = "red")

# Extract particle sizes --------------------------------------------------

uvp_clean <- uvp %>%
  filter(particle_size_range != ">26 mm") %>%
  separate(
    particle_size_range,
    into = c("particle_size_min", "particle_size_max", "particle_size_unit"),
    sep = "-| ",
    remove = FALSE,
    convert = TRUE
  ) %>%
  mutate(particle_size_unit = str_replace(particle_size_unit, "µ", "u"))

uvp_clean %>%
  distinct(particle_size_unit)

# Convert all particle size in mm (um -> mm)
uvp_clean <- uvp_clean %>%
  mutate(across(
    c(particle_size_min, particle_size_max),
    ~ {
      case_when(
        particle_size_unit == "um" ~ . / 1000,
        TRUE ~ .
      )
    }
  ))

# Change the unit since everything is now in mm

uvp_clean <- uvp_clean %>%
  rename_with(~glue("{.}_mm"), .cols = c(particle_size_min, particle_size_max)) %>%
  select(-particle_size_unit)

uvp_clean

# Add size range label ----------------------------------------------------

uvp_clean <- uvp_clean %>%
  mutate(
    particle_size_range_mm = glue("{particle_size_min_mm}-{particle_size_max_mm} mm"),
    .after = particle_size_range
  )

# Visualize the particle size distribution --------------------------------

p <- uvp_clean %>%
  mutate(
    particle_size_range_mm = fct_reorder(
      particle_size_range_mm, particle_size_min_mm)
  ) %>%
  filter(count_per_liter != 0) %>%
  count(particle_size_range_mm) %>%
  ggplot(aes(x = n, y = fct_rev(particle_size_range_mm))) +
  geom_col() +
  labs(
    x = "Number of observation",
    y = NULL
  )

ggsave(
  here("graphs/30_uvp_number_observations_by_classe_range.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 5
)

# Export ------------------------------------------------------------------

fwrite(uvp_clean, here("data/clean/uvp_tidy.csv"))

# Keep only a subset of class size ----------------------------------------

# Here are the class sizes Marcel wants to examine first:

# 102um -323um
# 323 um - 1.02 mm
# 1.02 mm - 26 mm

uvp_clean <- uvp_clean %>%
  mutate(
    particle_size_class = case_when(
      particle_size_min_mm >= 0.102 & particle_size_max_mm <= 0.323 ~ "particle_class_small",
      particle_size_min_mm >= 0.323 & particle_size_max_mm <= 1.02 ~ "particle_class_medium",
      particle_size_min_mm >= 1.02 & particle_size_max_mm <= 26 ~ "particle_class_large",
      TRUE ~ NA_character_
    )
  ) %>%
  drop_na(particle_size_class) # Drop unused classes

# Looks good! There are 5 small classes, 5 medium classes and 14 large classes.
# For a total of 24 classes.
uvp_clean %>%
  distinct(particle_size_range, particle_size_class)

# Make sure we have 24 classes per measurement.
uvp_clean %>%
  count(station, transect, date, date_time, longitude, latitude, depth_m) %>%
  assertr::verify(n == 24)

# Sum the particle concentration for all the class size we just de --------

uvp_clean

uvp_clean <- uvp_clean %>%
  group_by(
    station,
    transect,
    date,
    date_time,
    longitude,
    latitude,
    depth_m,
    particle_size_class
  ) %>%
  summarise(across(c(count_per_liter, biovolume_ppm), ~ sum(., na.rm = TRUE))) %>%
  ungroup()

uvp_clean

# Add a column with the size range of the end-member classes
uvp_clean <- uvp_clean %>%
  mutate(particle_size_range = case_when(
    str_detect(particle_size_class, "small") ~ "0.102-0.323 mm",
    str_detect(particle_size_class, "medium") ~ "0.323-1.02 mm",
    str_detect(particle_size_class, "large") ~ "1.02-26 mm",
    TRUE ~ NA_character_
  ))

uvp_clean

# OWD ---------------------------------------------------------------------

owd <-
  read_csv(
    "https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/Randelhoff-et-al-2019_GreenEdge_per-station_v1.0.csv"
  ) %>%
  janitor::clean_names() %>%
  select(station, owd, starts_with("isolume")) %>%
  select(-isolume_m_at_0_415_einm2_d1)

uvp_clean %>%
  select(station, transect, longitude) %>%
  anti_join(owd, by = "station")

uvp_clean <- uvp_clean %>%
  inner_join(owd, by = "station")

uvp_clean

# Export ------------------------------------------------------------------

write_csv(uvp_clean, here("data/clean/uvp_small_medium_large_class_size.csv"))

