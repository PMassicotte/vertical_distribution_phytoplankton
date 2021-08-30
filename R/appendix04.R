# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Compare modeled PP (using PvsE curves), insitu PP vs CTD derived
# chla.
#
# See Fig. 6 in Hill et al., Synthesis of Integrated Primary Production in the
# Arctic Ocean.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","interpolate_fun.R"))
source(here("R","zzz_colors.R"))

# 2D visualization of insitu PP -------------------------------------------

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

## Overview of the data ----

pp %>%
  ggplot(aes(x = longitude, y = depth_m)) +
  geom_point(aes(color = pp_mean_ugc_l_24h, size = pp_mean_ugc_l_24h)) +
  scale_y_reverse() +
  facet_wrap(~transect, scales = "free_x")

## OWD ----

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

## Average by owd ----

# Looks like we do not have to average
pp %>%
  count(depth_m, owd, sort = TRUE) %>%
  assertr::verify(n == 1)

## Isolume data ----

isolume <- read_csv(here("data", "raw", "randelhoff2019", "FIGURE_9-c-d-e.csv")) %>%
  janitor::clean_names() %>%
  select(owd, isolume_01) %>%
  pivot_longer(starts_with("isolume"),
    names_to = "isolume",
    values_to = "depth_m"
  )

## Interpolation ----

df_viz <- pp %>%
  nest(data = everything()) %>%
  mutate(res = map(
    data,
    interpolate_2d,
    owd,
    depth_m,
    pp_mean_ugc_l_24h
  )) %>%
  unnest(res) %>%
  rename(
    owd = x,
    depth_m = y,
    pp_mean_ugc_l_24h = z
  ) %>%
  select(-data) %>%
  mutate(pp_mean_ugc_l_24h = ifelse(pp_mean_ugc_l_24h < 0, 0, pp_mean_ugc_l_24h))

range(df_viz$pp_mean_ugc_l_24h, na.rm = TRUE)

p1 <- df_viz %>%
  ggplot(aes(
    x = owd,
    y = depth_m,
    z = pp_mean_ugc_l_24h,
    fill = pp_mean_ugc_l_24h
  )) +
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
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.05)),
    breaks = scales::breaks_pretty(n = 8)
  ) +
  geom_line(
    data = isolume,
    size = 1,
    aes(x = owd, y = depth_m, color = isolume),
    inherit.aes = FALSE,
    show.legend = FALSE,
  ) +
  paletteer::scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  labs(
    y = "Depth (m)",
    x = "OWD",
    fill = quote("Insitu primary production" ~ (mu*g*C~l^{-1}~d^{-1}))
  ) +
  theme(
    panel.grid = element_line(color = "gray60", size = 0.1),
    panel.background = element_rect(fill = NA),
    panel.ontop = TRUE,
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 10, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.box = "vertical",
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  )

# Insitu PP vs chla -------------------------------------------------------

ctd <- read_csv(here("data", "clean", "ctd.csv"))

insitu_pp <- read_csv(here("data", "raw", "greenedge_primary_prod.csv")) %>%
  filter(mission == "amundsen_2016" & sample_source == "rosette") %>%
  filter(str_detect(station, "^G\\d{3}")) %>%
  mutate(station = parse_number(station)) %>%
  select(station, depth_m, pp_mean_ugc_l_24h)

insitu_pp

setDT(ctd)
setDT(insitu_pp)

ctd[, ctd_depth_m := depth_m]

df <- ctd[insitu_pp, on = c("station", "depth_m"), roll = "nearest"]

p2 <- df %>%
  as_tibble() %>%
  filter(abs(depth_m - ctd_depth_m) <= 0.1) %>%
  mutate(is_open_water = is_open_water(owd)) %>%
  ggplot(aes(
    x = flor_mg_m3,
    y = pp_mean_ugc_l_24h,
    color = is_open_water
  )) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_owd() +
  geom_smooth(method = "lm", show.legend = FALSE) +
  annotation_logticks(sides = "bl", size = 0.1) +
  ggpubr::stat_regline_equation(
    label.y = log10(0.07),
    label.x.npc = 0.6,
    size = 3,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))
  ) +
  labs(
    x = quote("CTD fluorescence" ~ (mg~m^{-3})),
    y = quote("Primary production" ~ (mu*g*C~l^{-1}~d^{-1}))
  ) +
  facet_wrap(~is_open_water, scales = "free_x") +
  theme(
    strip.text = element_blank(),
    legend.justification = c(0, 1),
    legend.position = c(0.01, 0.99),
    legend.key = element_rect(fill = NA),
    legend.background = element_rect(fill = NA)
  )

# Combine plots -----------------------------------------------------------

p <- p1 / p2 +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here("graphs","appendix04.pdf"),
  device = cairo_pdf,
  width = 10,
  height = 8
)
