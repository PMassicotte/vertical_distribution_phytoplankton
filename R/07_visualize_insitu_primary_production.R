# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  2D visualization of in-situ primary production.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","interpolate_fun.R"))

pp <- read_csv(here::here("data","raw","greenedge_primary_prod.csv"))

pp %>%
  count(mission, sample_type, sample_source, method)

pp <- pp %>%
  filter(mission == "amundsen_2016" & sample_source == "rosette")

pp <- pp %>%
  select(station, depth_m, date, cast, bottle, longitude, latitude, pp_mean_ugc_l_24h) %>%
  drop_na(station) %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100, .after = station)

pp

# Make sure there is only 1 observation per station/depth
pp %>%
  count(station, depth_m, sort = TRUE)

pp %>%
  group_by(station) %>%
  summarise(n = n_distinct(cast)) %>%
  assertr::verify(n == 1)

# Overview of the data ----------------------------------------------------

pp %>%
  ggplot(aes(x = longitude, y = depth_m)) +
  geom_point(aes(color = pp_mean_ugc_l_24h, size = pp_mean_ugc_l_24h)) +
  scale_y_reverse() +
  facet_wrap(~transect, scales = "free_x")

# OWD ---------------------------------------------------------------------

owd <- read_csv(
  here(
    "data",
    "raw",
    "randelhoff2019",
    "Randelhoff-et-al-2019_GreenEdge_per-station_v1.0.csv"
  )
) %>%
  janitor::clean_names() %>%
  select(station, owd, starts_with("isolume")) %>%
  pivot_longer(starts_with("isolume"),
    names_to = "isolume",
    values_to = "isolume_depth_m"
  ) %>%
  drop_na()

pp %>%
  select(station, transect, longitude) %>%
  anti_join(owd, by = "station")

owd <- pp %>%
  select(station, transect, longitude) %>%
  inner_join(owd, by = "station")

owd <- owd %>%
  distinct(station, owd)

pp %>%
  anti_join(owd)

pp <- pp %>%
  inner_join(owd, by = "station")

# Average by owd ----------------------------------------------------------

# Looks like we do not have to average
pp %>%
  count(depth_m, owd, sort = TRUE) %>%
  assertr::verify(n == 1)

# Isolume data ------------------------------------------------------------

isolume <- read_csv(here("data", "raw", "randelhoff2019", "FIGURE_9-c-d-e.csv")) %>%
  janitor::clean_names() %>%
  select(owd, starts_with("isolume")) %>%
  pivot_longer(starts_with("isolume"),
    names_to = "isolume",
    values_to = "depth_m"
  )

# Interpolation -----------------------------------------------------------

df_viz <- pp %>%
  nest(data = everything()) %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, pp_mean_ugc_l_24h)) %>%
  unnest(res) %>%
  rename(owd = x, depth_m = y, pp_mean_ugc_l_24h = z) %>%
  select(-data) %>%
  mutate(pp_mean_ugc_l_24h = ifelse(pp_mean_ugc_l_24h < 0, 0, pp_mean_ugc_l_24h))

range(df_viz$pp_mean_ugc_l_24h, na.rm = TRUE)

p <- df_viz %>%
  ggplot(aes(x = owd, y = depth_m, z = pp_mean_ugc_l_24h, fill = pp_mean_ugc_l_24h)) +
  geom_isobands(color = NA, breaks = seq(0, 100, by = 3)) +
  paletteer::scale_fill_paletteer_c(
    "oompaBase::jetColors",
    trans = "sqrt",
    # oob = scales::squish,
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
  # facet_wrap(~transect, scales = "free_x") +
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
    fill = bquote("Primary production" ~ (mu*g*C~l^{-1}~d^{-1})),
    title = "Insitu primary production",
    subtitle = "Data from Patrick Rimbault"
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
  here::here("graphs","07_insitu_primary_production.pdf"),
  device = cairo_pdf,
  height = 6,
  width = 8
)
