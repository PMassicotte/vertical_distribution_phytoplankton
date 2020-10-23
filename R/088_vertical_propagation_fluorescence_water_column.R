# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Propagate CTD fluorescence into the water column.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here::here("R/propagate_fun.R"))

depth_m <- seq(1, 50, by = 1)

df <- fread(here::here("data/clean/ctd_fluorescence_npq_corrected.csv")) %>%
  as_tibble() %>%
  filter(depth_m <= 50)

df

df <- df %>%
  select(station, transect, cast, depth_m, flor_mg_m3_rollmedian_npq_corrected)

df

res <- df %>%
  group_nest(station, transect, cast) %>%
  mutate(interpolated_flor_mg_m3_rollmedian_npq_corrected = map(
    data,
    ~ propagate_vertically(., depth_m, flor_mg_m3_rollmedian_npq_corrected, depth_m = depth_m)
  )) %>%
  mutate(depth_grid_m = list(depth_m)) %>%
  unnest(cols = c(depth_grid_m, starts_with("interpolated"))) %>%
  relocate(depth_grid_m, .after = transect) %>%
  rename(depth_m = depth_grid_m)

res

# Plot --------------------------------------------------------------------

res %>%
  ggplot(aes(x = flor_mg_m3_rollmedian_npq_corrected, y = depth_m, group = cast)) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~station, scales = "free_x")

# Export ------------------------------------------------------------------

res %>%
  add_count(station, transect, cast, depth_m) %>%
  assertr::verify(n == 1) %>%
  select(-data, -n) %>%
  relocate(cast, .after = transect) %>%
  write_csv(here::here("data/clean/propagated_fluorescence_water_column.csv"))

