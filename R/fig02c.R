# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  xxx
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# CTD ---------------------------------------------------------------------

ctd <- read_csv(here("data","clean","ctd.csv"))

stations <- ctd %>%
  distinct(station, transect, cast, owd)

# Integration depths ------------------------------------------------------

integration_depths <- read_csv(here(
  "data",
  "raw",
  "randelhoff2019",
  "Randelhoff-et-al-2019_GreenEdge_per-station_v1.0.csv"
), na = "NaN") %>%
  janitor::clean_names() %>%
  select(station, owd, h_bd_m, nitracline_m, isolume_m_at_0_1_einm_2d_1)

integration_depths

# Nutrients ---------------------------------------------------------------

nutrients <- read_csv(here("data", "raw", "greenedge_nutrients.csv")) %>%
  filter(mission == "amundsen_2016" & sample_source == "rosette")

nutrients

nutrients %>%
  count(filter_type)

nutrients <- nutrients %>%
  select(station, cast, depth_m, contains("um_l")) %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100, .after = station) %>%
  distinct()

nutrients %>%
  count(station, depth_m, sort = TRUE) %>%
  assertr::verify(n == 1)

nutrients

 nutrients <- nutrients %>%
  inner_join(stations, by = c("station", "transect", "cast")) %>%
  inner_join(integration_depths, by = c("station", "owd"))

nutrients

## Average nutrients ----

df_viz <- nutrients %>%
  filter(depth_m <= h_bd_m) %>%
  group_by(station, cast, owd) %>%
  summarize(across(contains("um_l"), ~pracma::trapz(depth_m, .))) %>%
  ungroup() %>%
  # Use 12 classes as Achim did?
  mutate(owd_class = santoku::chop_evenly(owd, intervals = 12)) %>%
  group_by(owd_class) %>%
  summarise(across(
    c(owd, contains("um_l")),
    .fns = list(mean = mean, sd = sd),
    na.rm = TRUE
  ))

p1 <- df_viz %>%
  ggplot(aes(x = owd_mean, y = no3_um_l_mean)) +
  geom_linerange(aes(
    ymin = no3_um_l_mean - no3_um_l_sd,
    ymax = no3_um_l_mean + no3_um_l_sd
  ),
  color = "black",
  size = 0.2
  )+
  geom_point(size = 2, color = "#3c3c3c") +
  geom_smooth(method = "gam", alpha = 0.2, color = "#d64933") +
  geom_vline(xintercept = 0, lty = 2) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 6), limits = c(-30, 35)) +
  scale_y_continuous() +
  labs(
    x = "OWD",
    y = quote(NO[3]~(mol~m^{-2}))
  )

# CTD by OWD --------------------------------------------------------------

ctd

chla <- ctd %>%
  drop_na(flor_mg_m3) %>%
  inner_join(integration_depths, by = c("station", "owd"))

df_viz <- chla %>%
  filter(depth_m <= isolume_m_at_0_1_einm_2d_1) %>%
  group_by(station, cast, owd) %>%
  summarize(across(c(flor_mg_m3, cp), ~pracma::trapz(depth_m, .))) %>%
  ungroup() %>%
  # Use 12 classes as Achim did?
  mutate(owd_class = santoku::chop_evenly(owd, intervals = 12)) %>%
  group_by(owd_class) %>%
  summarise(across(
    c(owd, flor_mg_m3, cp),
    .fns = list(mean = mean, sd = sd),
    na.rm = TRUE
  ))

p2 <- df_viz %>%
  ggplot(aes(x = owd_mean, y = flor_mg_m3_mean)) +
  geom_linerange(
    aes(
      ymin = flor_mg_m3_mean - flor_mg_m3_sd,
      ymax = flor_mg_m3_mean + flor_mg_m3_sd
    ),
    color = "black",
    size = 0.2
  ) +
  geom_point(size = 2, color = "#3c3c3c") +
  geom_smooth(method = "gam", alpha = 0.2, color = "#d64933") +
  geom_vline(xintercept = 0, lty = 2) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 6), limits = c(-30, 35)) +
  scale_y_continuous() +
  labs(
    x = "OWD",
    y = quote("Chlorophyll-a"~(mg~m^{-2}))
  )

p3 <- df_viz %>%
  ggplot(aes(x = owd_mean, y = cp_mean)) +
  geom_linerange(
    aes(
      ymin = cp_mean - cp_sd,
      ymax = cp_mean + cp_sd
    ),
    color = "black",
    size = 0.2
  ) +
  geom_point(size = 2, color = "#3c3c3c") +
  geom_smooth(method = "gam", alpha = 0.2, color = "#d64933") +
  geom_vline(xintercept = 0, lty = 2) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 6), limits = c(-30, 35)) +
  scale_y_continuous() +
  labs(
    x = "OWD",
    y = quote(C[p~(657)]~(m))
  )

# bbp from the hydroscat --------------------------------------------------

hydroscat <- read_csv(here("data","clean","hydroscat.csv")) %>%
  filter(wavelength == 470) %>%
  rename(depth_m = depth)

df_viz <- hydroscat %>%
  inner_join(integration_depths, by = c("station", "owd")) %>%
  filter(depth_m <= isolume_m_at_0_1_einm_2d_1) %>%
  group_by(station, owd) %>%
  summarize(across(c(bbp), ~pracma::trapz(depth_m, .))) %>%
  ungroup() %>%
  # Use 12 classes as Achim did?
  mutate(owd_class = santoku::chop_evenly(owd, intervals = 12)) %>%
  group_by(owd_class) %>%
  summarise(across(
    c(owd, bbp),
    .fns = list(mean = mean, sd = sd),
    na.rm = TRUE
  ))

p4 <- df_viz %>%
  ggplot(aes(x = owd_mean, y = bbp_mean)) +
  geom_linerange(
    aes(
      ymin = bbp_mean - bbp_sd,
      ymax = bbp_mean + bbp_sd
    ),
    color = "black",
    size = 0.2
  ) +
  geom_point(size = 2, color = "#3c3c3c") +
  geom_smooth(method = "gam", alpha = 0.2, color = "#d64933") +
  geom_vline(xintercept = 0, lty = 2) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 6), limits = c(-30, 35)) +
  scale_y_continuous() +
  labs(
    x = "OWD",
    y = quote(b[bp]~(470)~(m))
  )


# Combine plots -----------------------------------------------------------

p <- p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here("graphs","fig02c.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 5
)


