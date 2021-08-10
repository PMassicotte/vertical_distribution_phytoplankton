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

# bbp at 470 nm -----------------------------------------------------------

hydroscat <- read_csv(here("data","clean","hydroscat.csv")) %>%
  filter(wavelength == 470) %>%
  rename(depth_m = depth) %>%
  filter(depth_m <= 100)

# Summarize by depth and open water day -----------------------------------

hydroscat_mean <- hydroscat %>%
  dtplyr::lazy_dt() %>%
  group_by(owd, depth_m) %>%
  summarise(across(c(bbp), ~mean(., na.rm = TRUE), .names = "mean_{.col}")) %>%
  as_tibble() %>%
  drop_na()

hydroscat_mean

## Interpolation and plot ----

df_viz <- hydroscat_mean %>%
  nest(data = everything()) %>%
  mutate(res = map(
    data,
    interpolate_2d,
    owd,
    depth_m,
    mean_bbp,
    h = 7,
    m = 1,
    n = 1
  )) %>%
  unnest(res) %>%
  rename(
    owd = x,
    depth_m = y,
    mean_bbp = z
  ) %>%
  select(-data) %>%
  mutate(mean_bbp = ifelse(mean_bbp < 0, 0, mean_bbp)) %>%
  drop_na(mean_bbp)

df_viz

range(df_viz$mean_bbp, na.rm = TRUE)

p5 <- gg3d(
  df = df_viz,
  x = owd,
  y = depth_m,
  z = mean_bbp,
  iso_breaks = seq(0, 0.01, length.out = 50),
  fill_text = expression("italic(b[bp])~(470)~(m^{-1})"),
  isolume = isolume,
  nbreaks = 5
)

p5 <- p5 +
  labs(
    x = "Number of open water days (OWD)"
  )

## Average vertical profiles ----

df_average_profiles <- average_vertical_profiles(df_viz, mean_bbp, breaks = breaks)

p6 <- gg2dprofiles(df_average_profiles, mean_bbp, depth_m, owd_bin)

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

