# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  plot 2-D de log(bbp(532)/bbp(700)) / 0,274 (ici, je suppose que
# bbp varie spectralement suivant une loi de puissance lambda**-n, et on veut
# retrouver n qui fournit un indice de taille).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "interpolate_fun.R"))
source(here("R", "plot_funs.R"))
source(here("R", "utils.R"))

breaks <- c(-30, -10, 10, 40)

# Hydroscat data ----------------------------------------------------------

hydroscat <- read_csv(here::here("data/clean/hydroscat.csv")) %>%
  rename(depth_m = depth) %>%
  select(-fchla) %>%
  filter(wavelength %in% c(532, 700)) %>%
  filter(bbp >= 0)

hydroscat

df <- hydroscat %>%
  pivot_wider(names_from = wavelength, values_from = bbp, names_prefix = "bbp_") %>%
  mutate(index_marcel = log(bbp_532 / bbp_700) / 0.274)

range(df$index_marcel)

df %>%
  ggplot(aes(x = index_marcel)) +
  geom_histogram(binwidth = 0.1)

df %>%
  ggplot(aes(x = owd, y = depth_m, fill = index_marcel)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_viridis_c()

# Isolume data ------------------------------------------------------------

isolume <-
  read_csv(
    "https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/FIGURE_9-c-d-e.csv"
  ) %>%
  janitor::clean_names() %>%
  select(owd, isolume_01) %>%
  pivot_longer(starts_with("isolume"), names_to = "isolume", values_to = "depth_m")

# The isolume data goes further in time (OWD) than de hydroscat data
isolume <- isolume %>%
  filter(owd <= max(df$owd, na.rm = TRUE))

# 3D plot of bbp/chla -----------------------------------------------------

df

df_mean <- df %>%
  dtplyr::lazy_dt() %>%
  group_by(owd, depth_m) %>%
  summarise(mean_index_marcel = mean(index_marcel, na.rm = TRUE), n = n()) %>%
  as_tibble() %>%
  drop_na() %>%
  filter(depth_m <= 100)

range(df_mean$mean_index_marcel)

df_viz <- df_mean %>%
  nest(data = everything()) %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, mean_index_marcel, h = 5)) %>%
  unnest(res) %>%
  rename(owd = x, depth_m = y, mean_index_marcel = z) %>%
  select(-data) %>%
  # mutate(mean_index_marcel = ifelse(mean_index_marcel < 0, 0, mean_index_marcel)) %>%
  drop_na(mean_index_marcel)

range(df_viz$mean_index_marcel)

p1 <- gg3d(
  df = df_viz,
  x = owd,
  y = depth_m,
  z = mean_index_marcel,
  iso_breaks = seq(-5, 10, by = 0.05),
  fill_text = expression("Band~ratio~(Marcel~index)"),
  isolume = isolume,
  nbreaks = 8,
  trans_fun = scales::trans_new("shift", function(x) x, identity)
)

## Average vertical profiles ----

df_average_profiles <- average_vertical_profiles(df_viz, mean_index_marcel, breaks = breaks)

p2 <- gg2dprofiles(df_average_profiles, mean_index_marcel, depth_m, owd_bin)

# Save plots --------------------------------------------------------------

p <- p1 + p2 +
  plot_layout(ncol = 2, widths = c(0.7, 0.3)) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here::here("graphs","fig05.pdf"),
  device = cairo_pdf,
  width = 9,
  height = 4
)
