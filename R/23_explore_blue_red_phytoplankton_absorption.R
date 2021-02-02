# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore the relationships between blue and red regions of
# phytoplankton absorption.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/interpolate_fun.R")

absorption <- read_csv(here("data/clean/phtyplankton_absorption.csv"))

absorption %>%
  count(spectra_id)

df_viz <- absorption %>%
  select(
    spectra_id,
    wavelength,
    aphy,
    aphy_specific,
    above_isolume,
    ice_covered
  ) %>%
  mutate(spectral_region = case_when(
    between(wavelength, 430, 450) ~ "blue",
    between(wavelength, 672, 678) ~ "red",
    TRUE ~ NA_character_
  )) %>%
  drop_na(spectral_region)

df_viz

df_viz <- df_viz %>%
  group_by(spectra_id, spectral_region, above_isolume, ice_covered) %>%
  summarise(across(contains("aphy"), mean, na.rm = TRUE)) %>%
  ungroup()

df_viz

# Scatterplot blue and red specific absorption ----------------------------

p <- df_viz %>%
  select(-aphy) %>%
  pivot_wider(names_from = spectral_region, values_from = aphy_specific) %>%
  ggplot(aes(x = blue, y = red)) +
  geom_point(color = "#393E41") +
  labs(
    title = "Comparing phytoplankton specific absorption",
    subtitle = "Each point represents the averaged value calculated for each spectral region.",
    x = quote(a[phi]^"*"~"(430-450 nm)"),
    y = quote(a[phi]^"*"~"(672-678 nm)")
  ) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
  geom_smooth(method = "lm", color = "#bf1d28", size = 0.5) +
  theme(
    # panel.border = element_blank()
  )

ggsave(
  here("graphs/23_scatterplot_phytoplankton_specific_absorption_blue_red.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 5
)

# By group
p <- p +
  facet_grid(above_isolume ~ ice_covered, scales = "fixed")

ggsave(
  here("graphs/23_scatterplot_phytoplankton_specific_absorption_blue_red_by_group.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 5
)

# Prepare data for interpolation ------------------------------------------

df_viz <- absorption %>%
  select(
    spectra_id,
    station,
    owd,
    depth_m,
    wavelength,
    aphy_specific
  ) %>%
  mutate(spectral_region = case_when(
    between(wavelength, 430, 450) ~ "blue",
    between(wavelength, 672, 678) ~ "red",
    TRUE ~ NA_character_
  )) %>%
  drop_na(spectral_region) %>%
  select(-wavelength)

df_viz

# Calculate the average for each spectral region

df_viz <- df_viz %>%
  group_by(spectra_id, owd, depth_m, spectral_region) %>%
  summarise(across(contains("aphy"), mean, na.rm = TRUE)) %>%
  ungroup()

df_viz

# Pivot wider to calculate the blue/red ratio

df_viz <- df_viz %>%
  pivot_wider(names_from = spectral_region, values_from = aphy_specific) %>%
  mutate(aphy_blue_red_ratio = blue / red)

df_viz

# Calculate the blue/red ratio

df_viz <- df_viz %>%
  group_by(spectra_id, depth_m, owd) %>%
  summarise(across(contains("aphy"), mean, na.rm = TRUE)) %>%
  ungroup()

df_viz

df_viz <- df_viz %>%
  group_by(owd, depth_m) %>%
  summarise(mean_aphy_blue_red_ratio = mean(aphy_blue_red_ratio, na.rm = TRUE)) %>%
  ungroup()

# Interpolation -----------------------------------------------------------

df_viz <- df_viz %>%
  nest(data = everything()) %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, mean_aphy_blue_red_ratio)) %>%
  unnest(res) %>%
  rename(
    owd = x,
    depth_m = y,
    mean_aphy_blue_red_ratio = z
  ) %>%
  select(-data) %>%
  mutate(
    mean_aphy_blue_red_ratio = ifelse(mean_aphy_blue_red_ratio < 0, 0, mean_aphy_blue_red_ratio)
  )

range(df_viz$mean_aphy_blue_red_ratio, na.rm = TRUE)

# Isolume -----------------------------------------------------------------

isolume <-
  read_csv(
    "https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/FIGURE_9-c-d-e.csv"
  ) %>%
  janitor::clean_names() %>%
  select(owd, starts_with("isolume")) %>%
  pivot_longer(starts_with("isolume"), names_to = "isolume", values_to = "depth_m")

# Plot --------------------------------------------------------------------

p <- df_viz %>%
  ggplot(aes(x = owd, y = depth_m, z = mean_aphy_blue_red_ratio, fill = mean_aphy_blue_red_ratio)) +
  geom_isobands(color = NA, breaks = seq(0, 10, by = 0.05)) +
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
    x = "OWD",
    fill = quote(a[phi]^"*"~"(430-450 nm)"~"/"~a[phi]^"*"~"(672-678 nm)"),
    title = "Blue/red ratio of specific phytoplankon absorption"
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
  here("graphs/23_visualize_phytoplankton_specific_absorption_blue_red_ratio_owd.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 5
)
