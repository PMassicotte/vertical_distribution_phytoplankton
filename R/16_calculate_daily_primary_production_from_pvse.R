# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Calculate primary production from PvsE parameters derived from
# the incubation.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

pvse <- read_csv(here::here("data","clean","propagated_pvse_water_column.csv"))
ctd <- read_csv(here::here("data","clean","propagated_fluorescence_water_column.csv"))
hourly_par <- read_csv(here::here("data","clean","propagated_hourly_par_water_column.csv"))
isolume <- read_csv(here::here("data","clean","isolume_0.1_mol_day.csv"))


# Keep the observations within the 0.1 mol day isolume --------------------

hourly_par <- hourly_par %>%
  inner_join(isolume) %>%
  group_by(station) %>%
  filter(between(depth_m, 0, maximum_integration_depth_m)) %>%
  ungroup() %>%
  select(-maximum_integration_depth_m)

# Convert planar PAR to scalar PAR ----------------------------------------

# See ref in the paper. The value of 1.2 was discussed with the co-authors.

hourly_par <- hourly_par %>%
  mutate(hourly_par_z_umol_m2_s1 = 1.2 * hourly_par_z_umol_m2_s1)

# Overview of the data ----------------------------------------------------

pvse %>%
  distinct(station)

ctd %>%
  distinct(station, cast)

hourly_par %>%
  distinct(station, deployement)

# Combine the data --------------------------------------------------------

df <- pvse %>%
  inner_join(ctd, by = c("station", "transect", "depth_m")) %>%
  inner_join(hourly_par, by = c("station", "depth_m"))

df %>%
  distinct(station, transect, cast)

# Verify that there is only 1 measurement per station/cast/depth/hour
df %>%
  count(station, transect, cast, depth_m, hour) %>%
  assertr::verify(n == 1)

# Compute hourly primary production ---------------------------------------

df <- df %>%
  mutate(hourly_primary_production = pb_max  * (1 - exp(
    -alpha_b * (hourly_par_z_umol_m2_s1 / pb_max)
  ))) %>%
  mutate(hourly_primary_production = hourly_primary_production * flor_mg_m3_rollmedian)

df

# Compute daily primary production ----------------------------------------

df <- df %>%
  group_nest(station, transect, deployement, cast, depth_m) %>%
  mutate(
    daily_primary_production_at_depth =
      map_dbl(data, ~ pracma::trapz(.$hour, .$hourly_primary_production))
  )

df

# Export the data
df %>%
  select(-data) %>%
  write_csv(here::here("data","clean","daily_primary_production_at_depth_from_pvse.csv"))

# Vertically integrated primary production --------------------------------

daily_pp <- df %>%
  group_nest(station, transect, cast, deployement) %>%
  mutate(
    daily_primary_production_m2 =
      map_dbl(data, ~ pracma::trapz(.$depth_m, .$daily_primary_production_at_depth))
  )

daily_pp

daily_pp %>%
  select(-data) %>%
  write_csv(here::here("data","clean","daily_integrated_primary_production_from_pvse.csv"))

p <- daily_pp %>%
  ggplot(aes(x = daily_primary_production_m2)) +
  geom_histogram(bins = 20) +
  scale_x_log10() +
  facet_wrap(~deployement) +
  annotation_logticks(sides = "b") +
  labs(
    x = bquote("Primary production" ~ (mgC ~ m^{-2} ~ d^{-1})),
    y = "Count",
    title = "Daily primary production calculated from PvsE",
    subtitle = "Primary production was integrated between 1 meter and the depth of 0.1 mol day isolume."
  ) +
  scale_y_continuous(breaks = seq(0, 1e4)) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  here::here("graphs","16_01_histograms_daily_primary_production.pdf"),
  device = cairo_pdf,
  height = 3.5,
  width = 7
)

# Exploration -------------------------------------------------------------

df2 <- daily_pp %>%
  slice_max(daily_primary_production_m2, n = 10) %>%
  distinct(station, .keep_all = TRUE) %>%
  mutate(
    station =
      fct_reorder(as.character(station),
        daily_primary_production_m2,
        .desc = TRUE
      )
  )

df2

# pdftools::pdf_subset("/media/4TB/work-ulaval/projects/green_edge/green_edge/graphs/pe-curves/ge2016-amundsen_fitted.pdf",
#   pages = c(1, 24),
#   output = "~/Desktop/pvse_amundsen_stations_102_115.pdf"
# )

p1 <- df2 %>%
  ggplot(aes(x = station, y = daily_primary_production_m2, fill = station)) +
  geom_col() +
  labs(
    x = "Station",
    title = "Daily primary production",
    subtitle = "Integrated over 50 meters along the water column."
  )

p2 <- df2 %>%
  unnest(data) %>%
  ggplot(aes(x = daily_primary_production_at_depth, y = depth_m, color = station)) +
  geom_path() +
  scale_y_reverse() +
  labs(
    title = "Daily PP(z)",
    y = "Depth (m)"
  )

p3 <- df2 %>%
  unnest(data) %>%
  unnest(data) %>%
  filter(depth_m == 1) %>%
  ggplot(aes(x = hour, y = hourly_primary_production, color = station)) +
  geom_line() +
  labs(
    title = "Hourly PP at 1 meter"
  )

p4 <- df2 %>%
  unnest(data) %>%
  unnest(data) %>%
  filter(depth_m == 1) %>%
  ggplot(aes(x = hour, y = hourly_par_z_umol_m2_s1, color = station)) +
  geom_line() +
  labs(
    title = "Hourly PAR at 1 meter"
  )

p5 <- df2 %>%
  unnest(data) %>%
  unnest(data) %>%
  distinct(station, depth_m, alpha_b, pb_max, ek) %>%
  pivot_longer(alpha_b:ek) %>%
  ggplot(aes(x = value, y = depth_m, color = factor(station))) +
  geom_path() +
  scale_y_reverse() +
  labs(
    title = "Vertical profiles PvsE parameters"
  ) +
  facet_wrap(~name, scales = "free_x")

p6 <- df2 %>%
  unnest(data) %>%
  unnest(data) %>%
  distinct(station, cast, depth_m, flor_mg_m3_rollmedian) %>%
  ggplot(aes(x = flor_mg_m3_rollmedian, y = depth_m, color = station)) +
  geom_path() +
  scale_y_reverse() +
  labs(
    title = "Vertical profils of Chla (from the CTD)",
    y = "Depth (m)"
  )

p <- wrap_plots(p1, p2, p3, p4, p5, p6, ncol = 2) +
  plot_annotation(
    tag_levels = "A",
    title = "Stations with the highest daily PP rate",
    theme = theme(plot.title = element_text(size = 24))
  ) &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here::here("graphs","16_02_stations_with_highest_daily_pp.pdf"),
  device = cairo_pdf,
  height = 10,
  width = 12
)
