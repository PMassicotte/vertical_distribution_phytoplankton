# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Some cleanup of the CTD data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

owd <- read_csv("https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/Randelhoff-et-al-2019_GreenEdge_per-station_v1.0.csv", na = "NaN") %>%
  janitor::clean_names() %>%
  select(station, owd)

ctd <- fread(here::here("data/raw/ctd.csv")) %>%
  as_tibble()

ctd <- ctd %>%
  filter(str_detect(mission, "amundsen")) %>%
  filter(str_detect(station, "^G\\d*")) %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100, .after = station)

ctd

ctd %>%
  distinct(station)

ctd <- ctd %>%
  inner_join(owd, by = "station")

# There are at least two cast per station, lets average the numerical variables.
ctd %>%
  count(station, depth_m, sort = TRUE)

ctd <- ctd %>%
  # lazy_dt() %>%
  # sample_n(1e5) %>%
  group_by(station, transect, longitude, latitude, depth_m) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  select(-cast)

fwrite(ctd, here::here("data/clean/ctd.csv"))

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
