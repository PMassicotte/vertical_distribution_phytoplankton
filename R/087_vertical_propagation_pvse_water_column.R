# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Propagate variables/parameters needed to calculate primary
# production into the water column.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here::here("R/propagate_fun.R"))

depth_m <- seq(1, 50, by = 1)

station <- fread("data/clean/ctd.csv") %>%
  distinct(station, transect, longitude, latitude) %>%
  as_tibble()

# PvsE parameters ---------------------------------------------------------

# Keep observations where ek was calculated (model1, i.e. with photoinhibition)
pvse <-
  read_csv(
    "/media/4TB/work-ulaval/projects/green_edge/green_edge/data/pe-curves/photosynthetic_parameters_amundsen_2016.csv"
  ) %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100) %>%
  inner_join(station, by = c("station", "transect")) %>%
  select(station, transect, depth_m = depth, model_type, ps, alpha_b, pb_max, ek) %>%
  filter(model_type == "model1")

pvse

pvse <- pvse %>%
  add_count(station, depth_m) %>%
  filter(n == 1) %>%
  select(-n)

pvse %>%
  ggplot(aes(x = station, y = depth_m)) +
  geom_point() +
  scale_y_reverse() +
  facet_wrap(~transect, scales = "free_x")

res <- pvse %>%
  group_nest(station, transect) %>%
  mutate(interpolated_ps = map(data, ~ propagate_vertically(., depth_m, ps, depth_m = depth_m))) %>%
  mutate(interpolated_alpha_b = map(
    data,
    ~ propagate_vertically(., depth_m, alpha_b, depth_m = depth_m)
  )) %>%
  mutate(interpolated_pb_max = map(
    data,
    ~ propagate_vertically(., depth_m, pb_max, depth_m = depth_m)
  )) %>%
  mutate(interpolated_ek = map(data, ~ propagate_vertically(., depth_m, ek, depth_m = depth_m))) %>%
  mutate(depth_grid_m = list(depth_m)) %>%
  unnest(cols = c(depth_grid_m, starts_with("interpolated"))) %>%
  relocate(depth_grid_m, .after = transect) %>%
  rename(depth_m = depth_grid_m)

# Plot --------------------------------------------------------------------

pvse %>%
  ggplot(aes(x = pb_max, y = depth_m)) +
  geom_point(size = 5) +
  facet_wrap(~station, scales = "free_x") +
  scale_y_reverse() +
  geom_path(data = res, aes(x = pb_max, y = depth_m), color = "red") +
  labs(
    title = "Vertical propagation into the water column",
    y = "Depth (m)"
  ) +
  theme(
    plot.subtitle = element_text(lineheight = 1.25)
  )

# Export ------------------------------------------------------------------

res %>%
  select(-data) %>%
  write_csv(here::here("data/clean/propagated_pvse_water_column.csv"))
