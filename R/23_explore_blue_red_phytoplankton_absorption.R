# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore the relationships between blue and red regions of
# phytoplankton absorption.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

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
