# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Check if there is lag correlation along the water column between
# CP and Chla.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

ctd <- fread(here("data","clean","ctd.csv")) %>%
  select(station, transect, depth_m, owd, flor_mg_m3, cp) %>%
  as_tibble() %>%
  drop_na()

ctd

# Check we only have 1 measure by station/depth_m combination
setDT(ctd)[, .N, by=.(station, depth_m)] %>%
  as_tibble() %>%
  assertr::verify(N == 1)

ctd <- ctd %>%
  as_tibble()

# Find the stations with the lowest and highest OWD -----------------------

stations <- ctd %>%
  distinct(station, owd) %>%
  filter(owd == max(owd) | owd == min(owd)) %>%
  pull(station)

# stations <- ctd %>%
#   distinct(station) %>%
#   pull(station) %>%
#   sample(., size = 2)

stations

df <- ctd %>%
  filter(station %in% stations)

df %>%
  arrange(depth_m) %>%
  mutate(d = depth_m - lag(depth_m)) %>%
  ggplot(aes(x = d)) +
  geom_histogram() +
  facet_wrap(~station)

df %>%
  pivot_longer(c(flor_mg_m3, cp)) %>%
  arrange(desc(depth_m)) %>%
  ggplot(aes(x = value, y = depth_m, group = name)) +
  geom_path() +
  labs(
    title = "Vertical profiles of fchla and cp",
    y = "Depth (m)"
  ) +
  scale_y_reverse() +
  facet_grid(glue("Station {station} (owd: {owd})")~name, scales = "free")

# Create a depth grid that is common to each station ----------------------

df

# Let's round depth_m to the 1/10 decimal

df <- df %>%
  mutate(depth_m_rounded = round(depth_m, digits = 0)) %>%
  group_by(station, transect, owd, depth_m_rounded) %>%
  summarise(across(c(flor_mg_m3, cp), mean, na.rm = TRUE), n = n()) %>%
  ungroup()

df

# df %>%
#   arrange(station, depth_m_rounded) %>%
#   group_by(station) %>%
#   mutate(d = depth_m_rounded - lag(depth_m_rounded)) %>%
#   ggplot(aes(x = d)) +
#   geom_histogram() +
#   facet_wrap(~station)

p1 <- df %>%
  filter(depth_m_rounded <= 100) %>%
  pivot_longer(c(flor_mg_m3, cp)) %>%
  arrange(desc(depth_m_rounded)) %>%
  ggplot(aes(x = value, y = depth_m_rounded, group = name)) +
  geom_path() +
  labs(
    title = "Vertical profiles of fchla and cp",
    y = "Depth (m)"
  ) +
  scale_y_reverse() +
  facet_grid(glue("Station {station} (owd: {owd})")~name, scales = "free") +
  theme(
    strip.text.y = element_text(size = 8)
  )

# Depth correlation analysis ----------------------------------------------

df

df_viz <- df %>%
  group_nest(station, owd) %>%
  mutate(r = map(
    data,
    ~ roll::roll_cor(
      .$flor_mg_m3,
      .$cp,
      width = 10,
      complete_obs = TRUE,
      center = TRUE,
      scale = TRUE
    )
  )) %>%
  unnest(c(data, r))

df_viz

p2 <- df_viz %>%
  drop_na(r) %>%
  ggplot(aes(x = r, y = depth_m_rounded)) +
  geom_path() +
  scale_y_reverse(limits = c(100, 0)) +
  facet_wrap(~glue("Station {station} (owd: {owd})")) +
  labs(
    y = "Depth (m)"
  )

# Combine -----------------------------------------------------------------

p <- p1 / p2 +
  plot_layout(heights = c(1, 0.75))

ggsave(
  here("graphs","28_lag_correlation.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 7
)
