# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  3D plots of the MVP data as a function of OWD.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","interpolate_fun.R"))
source(here("R","plot_funs.R"))
source(here("R","utils.R"))

# Isolume data ------------------------------------------------------------

isolume <- read_csv(here(
  "data",
  "raw",
  "randelhoff2019",
  "FIGURE_9-c-d-e.csv"
)) %>%
  janitor::clean_names() %>%
  select(owd, h_bd, nitracline, isolume_01)

# MVP ---------------------------------------------------------------------

mvp <- read_csv(here("data","clean","mvp_with_owd.csv")) %>%
  arrange(measurement_id, pres) %>%
  select(
    measurement_id,
    owd,
    depth_m = pres,
    fluo
  ) %>%
  filter(depth_m <= 100)

mvp

# Summarize by depth and open water day -----------------------------------

df_mean <- mvp %>%
  dtplyr::lazy_dt() %>%
  group_by(owd, depth_m) %>%
  summarise(across(c(fluo), ~mean(., na.rm = TRUE), .names = "mean_{.col}")) %>%
  as_tibble() %>%
  drop_na()

df_mean

# Fluorescence ------------------------------------------------------------

## Interpolation and plot ----

df_viz <- df_mean %>%
  nest(data = everything()) %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, mean_fluo, h = 6)) %>%
  unnest(res) %>%
  rename(owd = x, depth_m = y, mean_fluo = z) %>%
  select(-data) %>%
  mutate(mean_fluo = ifelse(mean_fluo < 0, 0, mean_fluo)) %>%
  drop_na(mean_fluo)

range(df_viz$mean_fluo)

p1 <- gg3d(
  df = df_viz,
  x = owd,
  y = depth_m,
  z = mean_fluo,
  iso_breaks = seq(0, 20, by = 0.25),
  fill_text = expression("Chlorophyll-a"~(mg~m^{-3})),
  isolume = isolume,
  nbreaks = 8
) +
  labs(
    x = "Number of open water days (OWD)"
  )

## Average vertical profiles ----

df_average_profiles <- average_vertical_profiles(df_viz, mean_fluo, 2)

p2 <- gg2dprofiles(df_average_profiles, mean_fluo, depth_m, owd_bin)

# Save plots --------------------------------------------------------------

p <- p1 + p2 +
  plot_layout(ncol = 2, widths = c(0.7, 0.3)) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here::here("graphs","appendix01.pdf"),
  device = cairo_pdf,
  width = 9,
  height = 4
)

