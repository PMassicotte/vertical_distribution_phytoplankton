# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore the relationship between bbp and fluorescence.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

ctd <- vroom::vroom(here::here("data/clean/ctd.csv"), altrep = TRUE)

hydroscat <- vroom::vroom(here::here("data/clean/hydroscat.csv"), altrep = TRUE) %>%
  rename(depth_m = depth)

hydroscat

# Isolume data ------------------------------------------------------------

isolume <-
  read_csv(
    "https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/Randelhoff-et-al-2019_GreenEdge_per-station_v1.0.csv"
  ) %>%
  janitor::clean_names() %>%
  select(station, owd, isolume_m_at_0_1_einm_2d_1) %>%
  pivot_longer(starts_with("isolume"),
    names_to = "isolume",
    values_to = "isolume_depth_m"
  )

isolume

hydroscat %>%
  anti_join(isolume, by = c("station", "owd"))

hydroscat <- hydroscat %>%
  inner_join(isolume, by = c("station", "owd"))

# Classify stations -------------------------------------------------------

hydroscat <- hydroscat %>%
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

# Visualize bbp vs fluo ---------------------------------------------------

p <- hydroscat %>%
  ggplot(aes(x = bbp, y = fchla)) +
  geom_hex(bins = 75) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(size = 0.1) +
  scale_fill_viridis_c() +
  facet_wrap(~glue("{wavelength} nm")) +
  labs(
    title = "Backscattering vs chla fluorescence",
    subtitle = "Data from the Hydroscat.",
    x = quote("bbp" ~ (m^{-1})),
    y = "Chla fluorescence (raw counts)"
  ) +
  theme(
    aspect.ratio = 1,
    legend.position = "none"
  )

ggsave(
  here("graphs/24_scatterplot_hydroscat_bbp_fluo_chla.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 5
)

knitr::plot_crop(here("graphs/24_scatterplot_hydroscat_bbp_fluo_chla.pdf"))

p <- p +
  facet_grid(ice_covered + above_isolume~glue("{wavelength} nm")) +
  theme(
    strip.text.y = element_text(size = 5),
    axis.text = element_text(size = 6)
  )

ggsave(
  here("graphs/24_scatterplot_hydroscat_bbp_fluo_chla_by_group.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 7
)

knitr::plot_crop(here("graphs/24_scatterplot_hydroscat_bbp_fluo_chla_by_group.pdf"))
