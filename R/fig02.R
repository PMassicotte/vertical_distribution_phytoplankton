# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore the vertical distribution of the phytoplankton biomass
# using different kind of proxy.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","interpolate_fun.R"))
source(here("R","plot_funs.R"))
source(here("R","utils.R"))

breaks <- c(-30, -10, 10, 40)

# Isolume data ------------------------------------------------------------

isolume <-
  read_csv(
    "https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/FIGURE_9-c-d-e.csv"
  ) %>%
  janitor::clean_names() %>%
  select(owd, isolume_01) %>%
  pivot_longer(starts_with("isolume"), names_to = "isolume", values_to = "depth_m")

# CTD data ----------------------------------------------------------------

ctd <- read_csv(here::here("data/clean/ctd.csv")) %>%
  filter(depth_m <= 100)

# Summarize by depth and open water day -----------------------------------

df_mean <- ctd %>%
  dtplyr::lazy_dt() %>%
  group_by(owd, depth_m) %>%
  summarise(across(c(flor_mg_m3, cp), ~mean(., na.rm = TRUE), .names = "mean_{.col}")) %>%
  as_tibble() %>%
  drop_na()

df_mean

# CTD fluorescence --------------------------------------------------------

## Interpolation and plot ----

df_viz <- df_mean %>%
  nest(data = everything()) %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, mean_flor_mg_m3, h = 7)) %>%
  unnest(res) %>%
  rename(owd = x, depth_m = y, mean_flor_mg_m3 = z) %>%
  select(-data) %>%
  mutate(mean_flor_mg_m3 = ifelse(mean_flor_mg_m3 < 0, 0, mean_flor_mg_m3)) %>%
  drop_na(mean_flor_mg_m3)

p1 <- gg3d(
  df = df_viz,
  x = owd,
  y = depth_m,
  z = mean_flor_mg_m3,
  iso_breaks = seq(0, 20, by = 0.25),
  fill_text = expression("Chlorophyll-a"~(mg~m^{-3})),
  isolume = isolume,
  nbreaks = 8
)

## Average vertical profiles ----

df_average_profiles <- average_vertical_profiles(df_viz, mean_flor_mg_m3, breaks = breaks)

p2 <- gg2dprofiles(df_average_profiles, mean_flor_mg_m3, depth_m, owd_bin)

# Particle beam attenuation (CP) ------------------------------------------

df_viz <- df_mean %>%
  nest(data = everything()) %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, mean_cp, h = 7)) %>%
  unnest(res) %>%
  rename(owd = x, depth_m = y, mean_cp = z) %>%
  select(-data) %>%
  mutate(mean_cp = ifelse(mean_cp < 0, 0, mean_cp)) %>%
  drop_na(mean_cp)

p3 <- gg3d(
  df = df_viz,
  x = owd,
  y = depth_m,
  z = mean_cp,
  iso_breaks = seq(0, 2, by = 0.02),
  fill_text = expression(CP[657]~(m^{-1})),
  isolume = isolume,
  nbreaks = 7
)

## Average vertical profiles ----

df_average_profiles <- average_vertical_profiles(df_viz, mean_cp, breaks = breaks)

p4 <- gg2dprofiles(df_average_profiles, mean_cp, depth_m, owd_bin)


# Ratio chla/cp -----------------------------------------------------------

df_mean

## Interpolation and plot ----

df_viz <- df_mean %>%
  mutate(mean_chla_cp_ratio = mean_flor_mg_m3 / mean_cp) %>%
  nest(data = everything()) %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, mean_chla_cp_ratio, h = 7)) %>%
  unnest(res) %>%
  rename(owd = x, depth_m = y, mean_chla_cp_ratio = z) %>%
  select(-data) %>%
  mutate(mean_chla_cp_ratio = ifelse(mean_chla_cp_ratio < 0, 0, mean_chla_cp_ratio)) %>%
  drop_na(mean_chla_cp_ratio)

df_viz

range(df_viz$mean_chla_cp_ratio, na.rm = TRUE)

p5 <- gg3d(
  df = df_viz,
  x = owd,
  y = depth_m,
  z = mean_chla_cp_ratio,
  iso_breaks = seq(0, 50, by = 2),
  fill_text = expression("Chlorophyll-a / CP[657]~(mg~m^{-2})"),
  isolume = isolume,
  nbreaks = 10
)

p5 <- p5 +
  labs(
    x = "Number of open water days (OWD)"
  )

## Average vertical profiles ----

df_average_profiles <- average_vertical_profiles(df_viz, mean_chla_cp_ratio, breaks = breaks)

p6 <- gg2dprofiles(df_average_profiles, mean_chla_cp_ratio, depth_m, owd_bin)

# Save plots --------------------------------------------------------------

p <- p1 + p2 + p3 + p4 + p5 + p6 +
  plot_layout(ncol = 2, widths = c(0.7, 0.3)) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here::here("graphs","fig02.pdf"),
  device = cairo_pdf,
  width = 9,
  height = 8
)

