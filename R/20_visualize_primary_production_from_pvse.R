# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Different visualisations of PP calculated from PvsE parameters.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","interpolate_fun.R"))

isolume <-
  read_csv(here("data", "raw", "randelhoff2019", "FIGURE_9-c-d-e.csv")) %>%
  janitor::clean_names() %>%
  select(owd, starts_with("isolume")) %>%
  pivot_longer(starts_with("isolume"),
    names_to = "isolume",
    values_to = "depth_m"
  )

daily_pp_at_depth <-
  read_csv(here::here(
    "data",
    "clean",
    "daily_primary_production_at_depth_from_pvse.csv"
  ))

daily_pp_at_depth

owd <-
  read_csv(
    here(
      "data",
      "raw",
      "randelhoff2019",
      "Randelhoff-et-al-2019_GreenEdge_per-station_v1.0.csv"
    ),
    na = "NaN"
  ) %>%
  janitor::clean_names() %>%
  select(station, owd)

daily_pp_at_depth <- daily_pp_at_depth %>%
  inner_join(owd, by = "station")

daily_pp_at_depth

# Overview of the data ----------------------------------------------------

daily_pp_at_depth %>%
  count(station, cast, depth_m, owd)

owd <- owd %>%
  mutate(transect = station %/% 100 * 100) %>%
  filter(station %in% unique(daily_pp_at_depth$station))

daily_pp_at_depth %>%
  ggplot(aes(x = owd, y = depth_m, size = daily_primary_production_at_depth)) +
  geom_point() +
  scale_y_reverse(expand = expansion(mult = c(1, 0.2))) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  facet_wrap(~transect) +
  ggrepel::geom_text_repel(
    data = owd,
    aes(x = owd, y = -10, label = station),
    inherit.aes = FALSE,
    size = 3,
    color = "red"
  ) +
  labs(
    title = "Overview of the daily PP calculated at each depth",
    size = bquote("PP"~(mgC~d^{-1}~m^{-3})),
    subtitle = "Each panel is a transect and the red numbers are the stations.",
    y = "Depth (m)"
  ) +
  theme(
    legend.position = "top"
  )

ggsave(
  here::here("graphs", "20_01_primary_production_pvse_vs_owd.pdf"),
  device = cairo_pdf,
  width = 10,
  height = 7
)

# Interpolation -----------------------------------------------------------

df_viz <- daily_pp_at_depth %>%
  # filter(station != 102) %>%
  group_by(depth_m, owd) %>%
  summarise(across(where(is.numeric), ~mean(., na.rm = TRUE))) %>%
  ungroup()

df_viz <- df_viz %>%
  nest(data = everything()) %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, daily_primary_production_at_depth))

# Plot --------------------------------------------------------------------

p <- df_viz %>%
  unnest(res) %>%
  drop_na(z) %>%
  rename(owd = x, depth_m = y, daily_primary_production_at_depth = z) %>%
  select(-data) %>%
  mutate(daily_primary_production_at_depth =
           ifelse(daily_primary_production_at_depth < 0, 0, daily_primary_production_at_depth)) %>%
  ggplot(aes(x = owd, y = depth_m, z = daily_primary_production_at_depth, fill = daily_primary_production_at_depth)) +
  geom_isobands(color = NA, breaks = seq(0, 500, by = 5)) +
  paletteer::scale_fill_paletteer_c(
    "oompaBase::jetColors",
    # oob = scales::squish,
    # limits = c(0, 500),
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
    fill = bquote("PP"~(mgC~d^{-1}~m^{-3})),
    title = "Daily primary production calculated from PvsE parameters"
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
  here::here("graphs","20_02_primary_production_pvse_vs_owd.pdf"),
  device = cairo_pdf,
  height = 6,
  width = 8
)
