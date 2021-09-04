# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore absorption data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- readxl::read_excel(
  here("data","raw","GE-Amundsen-phyto_absorption_120517.xlsx"),
  sheet = "Discrete"
) %>%
  janitor::clean_names() %>%
  pivot_longer(matches("^x\\d{3}"), names_to = "wavelength", values_to = "aphy") %>%
  mutate(wavelength = parse_number(wavelength))

names(df)

df <- df %>%
  select(station, ctd, bottle, depth_m, total_chlorophyll_a, wavelength, aphy) %>%
  mutate(station = parse_number(station)) %>%
  drop_na(station)

# df %>%
#   count(station, ctd, depth_m) %>%
#   assertr::verify(n == 551)

df %>%
  filter(wavelength > 254) %>%
  ggplot(aes(x = wavelength, y = aphy, group = depth_m, color = depth_m)) +
  geom_line() +
  facet_wrap(~station, scales = "free_y")

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
  drop_na() %>%
  filter(isolume == "isolume_m_at_0_1_einm_2d_1")

# Three stations have no owd
df %>%
  anti_join(owd, by = "station") %>%
  distinct(station)

absorption <- df %>%
  inner_join(owd, by = "station")

absorption

# Classify stations -------------------------------------------------------

absorption <- absorption %>%
  mutate(
    above_isolume = case_when(
      depth_m < isolume_depth_m ~ "Above the 0.1 isolume",
      TRUE ~ "Below the 0.1 isolume"
    )
  ) %>%
  mutate(ice_covered = case_when(
    owd < 0 ~ "Ice covered",
    TRUE ~ "Open water"
  ))

absorption %>%
  filter(wavelength == 254) %>%
  count(above_isolume, ice_covered)

# Calculate phytoplankton specific absorption -----------------------------

absorption <- absorption %>%
  mutate(aphy_specific = aphy / total_chlorophyll_a, .after = aphy)

absorption

# Export the data ---------------------------------------------------------

# Add an ID to each spectra
absorption %>%
  count(station, ctd, bottle) %>%
  assertr::verify(n == 551)

absorption <- absorption %>%
  group_by(station, ctd, bottle) %>%
  mutate(spectra_id = cur_group_id(), .before = 1)

write_csv(absorption, here("data","clean","phytoplankton_absorption.csv"))

# Visualize the spectral profiles -----------------------------------------

p <- absorption %>%
  filter(between(wavelength, 400, 700)) %>%
  ggplot(
    aes(
      x = wavelength,
      y = aphy,
      group = depth_m,
      color = above_isolume,
      linetype = ice_covered
    )
  ) +
  geom_line() +
  facet_wrap(~glue("{station} ({owd} OWD)"), scales = "free", ncol = 4) +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[phi](lambda)),
    title = "Spectral profiles of phytoplankton absorption",
    subtitle = "Each line represents a different depth."
  ) +
  paletteer::scale_color_paletteer_d(
    "ggthemes::wsj_colors6"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.box = "horizontal",
    legend.direction = "vertical"
  )

ggsave(
  here::here(
    "graphs",
    "22_spectral_profiles_phyto_absorption_isolume_owd.pdf"
  ),
  device = cairo_pdf,
  height = 9,
  width = 7
)

# Visualize the averaged spectral profiles --------------------------------

df_viz <- absorption %>%
  filter(between(wavelength, 400, 700)) %>%
  group_by(wavelength, above_isolume, ice_covered) %>%
  summarise(mean_aphy = mean(aphy, na.rm = TRUE)) %>%
  ungroup()

p <- df_viz %>%
  ggplot(aes(x = wavelength, y = mean_aphy, color = above_isolume)) +
  geom_line() +
  facet_grid( ~ ice_covered, scales = "free") +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[phi](lambda)),
    title = "Averaged spectral profiles of phytoplankton absorption"
  ) +
  paletteer::scale_color_paletteer_d(
    "ggthemes::wsj_colors6",
    guide = guide_legend(
      label.position = "top",
      override.aes = list(size = 2),
      keywidth = unit(5, "cm")
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

ggsave(
  here::here(
    "graphs",
    "22_spectral_profiles_phyto_absorption_averaged_isolume_owd.pdf"
  ),
  device = cairo_pdf,
  height = 5,
  width = 8
)

# Visualize aphy 440 ------------------------------------------------------

p1 <- absorption %>%
  filter(wavelength == 440) %>%
  ggplot(aes(x = above_isolume, y = aphy, fill = ice_covered)) +
  geom_boxplot(size = 0.25, outlier.size = 0.5) +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  labs(
    x = NULL,
    y = quote(a[phi](440)),
    fill = NULL
  ) +
  paletteer::scale_fill_paletteer_d("nord::aurora")

p2 <- absorption %>%
  filter(wavelength == 440) %>%
  ggplot(aes(x = above_isolume, y = aphy_specific, fill = ice_covered)) +
  geom_boxplot(size = 0.25, outlier.size = 0.5) +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  labs(
    x = NULL,
    y = quote(a[phi]^"*"~(440)),
    fill = NULL
  ) +
  paletteer::scale_fill_paletteer_d("nord::aurora")

p <- p1 / p2 +
  plot_annotation(tag_levels = "A")

ggsave(
  here::here("graphs", "22_boxplot_phyto_absorption_isolume_owd.pdf"),
  device = cairo_pdf,
  height = 7,
  width = 6
)

# Vertical profiles -------------------------------------------------------

# Only keep vertical profiles that go at least to 50 m

df_viz <- absorption %>%
  filter(wavelength == 440) %>%
  drop_na(aphy) %>%
  group_by(station) %>%
  filter(any((depth_m >= 50))) %>%
  ungroup()

p <- df_viz %>%
    ggplot(aes(
    x = aphy,
    y = depth_m,
    color = ice_covered
  )) +
  geom_path() +
  geom_hline(
    aes(yintercept = isolume_depth_m, color = "Isolume 0.1"),
    size = 0.5,
    lty = 2
  ) +
  scale_y_reverse() +
  facet_wrap(~glue("{station} ({owd} OWD)"), scales = "fixed", ncol = 5) +
  paletteer::scale_color_paletteer_d(
    "ggthemes::wsj_colors6",
    guide = guide_legend(
      label.position = "top",
      keywidth = unit(2, "cm"),
      override.aes = list(size = 2)
    ),
    breaks = c("Ice covered", "Open water", "Isolume 0.1")
  ) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  labs(
    x = quote(a[phi](440)),
    y = "Depth (m)",
    color = NULL,
    title = "Vertical profiles of phytoplankton absorption",
    subtitle = "Each panel represents a station. The number in parenthesis is the number of open water days."
  ) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  here::here("graphs", "22_vertical_profiles_aphy.pdf"),
  device = cairo_pdf,
  height = 10,
  width = 9
)

# Averaged vertical profiles ----------------------------------------------

# Calculate the average vertical profiles for open and ice-covered stations.

df_viz

df_viz %>%
  group_by(depth_m, ice_covered) %>%
  summarise(
    mean_se(aphy),
    n = n()
  ) %>%
  ggplot(aes(x = y, y = depth_m, color = ice_covered)) +
  geom_path() +
  scale_y_reverse()

