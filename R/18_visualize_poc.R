# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore the spatio/temporal distribution of particulate organic
# carbon (POC).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/interpolate_fun.R")

# Isolume data ------------------------------------------------------------

isolume <-
  read_csv(
    "https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/FIGURE_9-c-d-e.csv"
  ) %>%
  janitor::clean_names() %>%
  select(owd, starts_with("isolume")) %>%
  pivot_longer(starts_with("isolume"), names_to = "isolume", values_to = "depth_m")

# OWD ---------------------------------------------------------------------

owd <- read_csv("https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/Randelhoff-et-al-2019_GreenEdge_per-station_v1.0.csv", na = "NaN") %>%
  janitor::clean_names() %>%
  select(station, owd)

# POC data ----------------------------------------------------------------

col_names <-
  readxl::read_excel(
    here::here("data/raw/database 13C15N amundsen2016-210318.xlsx"),
    skip = 9,
    n_max = 1
  ) %>%
  janitor::clean_names() %>%
  names()

# TODOL: select the right column for the depth...
poc <- readxl::read_excel(
  here::here("data/raw/database 13C15N amundsen2016-210318.xlsx"),
  skip = 12,
  col_names = FALSE
) %>%
  janitor::remove_empty(which = "cols") %>%
  set_names(col_names) %>%
  select(
    station_1,
    station = station_8,
    date,
    depth_m = prof,
    longitude,
    latitude,
    poc_umol_l = poc_19
  ) %>%
  drop_na(poc_umol_l)

poc

# Fill missing depth...

poc <- poc %>%
  group_by(station_1) %>%
  fill(station) %>%
  ungroup() %>%
  select(-station_1)

poc <- poc %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100, .after = station) %>%
  arrange(station, depth_m)

poc

poc %>%
  ggplot(aes(x = poc_umol_l, y = depth_m)) +
  geom_path() +
  facet_wrap(~station) +
  scale_y_reverse()

# Associate OWD to POC data -----------------------------------------------

poc %>%
  anti_join(owd) %>%
  assertr::verify(nrow(.) == 0)

poc <- poc %>%
  left_join(owd)

poc %>%
  write_csv(here("data/clean/poc.csv"))

# Average by depth and OWD ------------------------------------------------

poc %>%
  count(owd, depth_m, sort = TRUE)

poc <- poc %>%
  group_by(depth_m, owd) %>%
  summarise(across(where(is.numeric), ~mean(., na.rm = TRUE))) %>%
  ungroup()

# 2D interpolation --------------------------------------------------------

df_viz <- poc %>%
  filter(depth_m <= 100)

df_viz <- df_viz %>%
  nest(data = everything()) %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, poc_umol_l))

# Plot --------------------------------------------------------------------

p <- df_viz %>%
  unnest(res) %>%
  drop_na(z) %>%
  rename(owd = x, depth_m = y, poc_umol_l = z) %>%
  select(-data) %>%
  mutate(poc_umol_l = ifelse(poc_umol_l < 0, 0, poc_umol_l)) %>%
  ggplot(aes(x = owd, y = depth_m, z = poc_umol_l, fill = poc_umol_l)) +
  geom_isobands(color = NA, breaks = seq(0, 200, by = 1)) +
  paletteer::scale_fill_paletteer_c(
    "oompaBase::jetColors",
    trans = "sqrt",
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
    fill = bquote("POC"~(mg~m^{-3}))
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
  here::here("graphs/18_poc_vs_owd.pdf"),
  device = cairo_pdf,
  height = 6,
  width = 8
)
