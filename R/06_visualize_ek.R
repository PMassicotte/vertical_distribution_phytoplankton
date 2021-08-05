# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  2D visualization of Ek.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/interpolate_fun.R")

pvse <- read_csv("/media/LaCie16TB/work/projects/green_edge/green_edge/data/pe-curves/photosynthetic_parameters_amundsen_2016.csv")

glimpse(pvse)

pvse <- pvse %>%
  filter(str_starts(station, "G")) %>%
  mutate(station = parse_number(station)) %>%
  rename(depth_m = depth)

pvse %>%
  ggplot(aes(x = r2)) +
  geom_histogram(binwidth = 0.1)

pvse <- pvse %>%
  filter(r2 >= 0.8)

station <- read_csv(
    here::here("data/clean/ctd.csv"),
    altrep = TRUE,
    col_select = c(station, transect, longitude, latitude)
  ) %>%
  distinct()

pvse %>%
  anti_join(station, by = "station")

pvse <- pvse %>%
  inner_join(station, by = "station")

# Overview ----------------------------------------------------------------

pvse %>%
  ggplot(aes(x = longitude, y = depth_m)) +
  geom_point(aes(size = ek)) +
  scale_y_reverse() +
  facet_wrap(~transect, scales = "free_x")

# OWD ---------------------------------------------------------------------

isolume <-
  read_csv(
    "https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/Randelhoff-et-al-2019_GreenEdge_per-station_v1.0.csv"
  ) %>%
  janitor::clean_names() %>%
  select(station, owd, starts_with("isolume")) %>%
  pivot_longer(starts_with("isolume"), names_to = "isolume", values_to = "isolume_depth_m") %>%
  drop_na()

# Two stations without calculated owd...
pvse %>%
  anti_join(isolume, by = "station") %>%
  distinct(station)

owd <- isolume %>%
  distinct(station, owd)

pvse <- pvse %>%
  inner_join(owd, by = "station")

# Isolume data ------------------------------------------------------------

isolume <-
  read_csv(
    "https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/FIGURE_9-c-d-e.csv"
  ) %>%
  janitor::clean_names() %>%
  select(owd, starts_with("isolume")) %>%
  pivot_longer(starts_with("isolume"), names_to = "isolume", values_to = "depth_m")

isolume <- isolume %>%
  filter(between(owd, min(pvse$owd), max(pvse$owd)))

# Interpolation -----------------------------------------------------------

df_viz <- pvse %>%
  # filter(ek <= 100) %>%
  # filter(depth_m <= 75) %>%
  nest(data = everything()) %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, ek)) %>%
  unnest(res) %>%
  rename(owd = x, depth_m = y, ek = z) %>%
  select(-data) %>%
  mutate(ek = ifelse(ek < 0, 0, ek)) %>%
  drop_na()

range(df_viz$ek, na.rm = TRUE)

df_viz %>%
  ggplot(aes(x = ek)) +
  geom_histogram()

p <- df_viz %>%
  ggplot(aes(x = owd, y = depth_m, z = ek, fill = ek)) +
  geom_isobands(color = NA, breaks = seq(0, 300, by = 5)) +
  paletteer::scale_fill_paletteer_c(
    "oompaBase::jetColors",
    # trans = "log",
    limits = c(0, 150),
    oob = scales::squish,
    breaks = scales::breaks_pretty(n = 10),
    guide =
      guide_colorbar(
        barwidth = unit(8, "cm"),
        barheight = unit(0.2, "cm"),
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
  ) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.05)),
    breaks = scales::breaks_pretty(n = 8)
  ) +
  geom_line(data = isolume, size = 1, aes(x = owd, y = depth_m, color = isolume), inherit.aes = FALSE) +
  paletteer::scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  labs(
    y = "Depth (m)",
    x = "OWD",
    fill = bquote("Ek" ~ (mu*mol~quanta~m^{-2}~s^{-1})),
    title = "Irradiance at which the onset of saturation occurs (Ek)"
  ) +
  theme(
    panel.grid = element_line(color = "gray60", size = 0.1),
    panel.background = element_rect(fill = NA),
    panel.ontop = TRUE,
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 10, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  )

ggsave(
  here::here("graphs/06_ek.pdf"),
  device = cairo_pdf,
  height = 6,
  width = 8
)

