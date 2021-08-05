# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore the relationship between bbp and fluorescence.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/interpolate_fun.R")

hydroscat <- read_csv(here::here("data","clean","hydroscat.csv")) %>%
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

# Spectral dependency ------------------------------------------------------

# Marcel wanted to have a look at the relationship between 532 and 700 nm

hydroscat

df_viz <- hydroscat %>%
  filter(wavelength %in% c(532, 700)) %>%
  pivot_wider(names_from = wavelength, values_from = bbp) %>%
  janitor::clean_names()

df_viz

p <- df_viz %>%
  ggplot(aes(x = x532, y = x700)) +
  geom_point(color = "#393E41") +
  labs(
    title = "Comparing bbp at two wavelengths",
    subtitle = "Data from the Hydroscat.",
    x = quote(bbp[532~nm] ~ (m^{-1})),
    y = quote(bbp[700~nm] ~ (m^{-1}))
  ) +
  geom_smooth(method = "lm", color = "#bf1d28", size = 0.5)

ggsave(
  here("graphs/24_scatterplot_hydroscat_bbp_532nm_vs_700nm.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 5
)

p <- p +
  facet_grid(ice_covered ~ above_isolume, scales = "free")

ggsave(
  here("graphs/24_scatterplot_hydroscat_bbp_532nm_vs_700nm_by_group.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 5
)

# 3D plot of bbp/chla -----------------------------------------------------

isolume <-
  read_csv(
    "https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/FIGURE_9-c-d-e.csv"
  ) %>%
  janitor::clean_names() %>%
  select(owd, starts_with("isolume")) %>%
  pivot_longer(starts_with("isolume"), names_to = "isolume", values_to = "depth_m")

hydroscat

df_viz <- hydroscat %>%
  filter(bbp >= 0) %>%
  filter(fchla >= 0) %>%
  mutate(bbp_chla_ratio = bbp / fchla) %>%
  dtplyr::lazy_dt() %>%
  group_by(wavelength, owd, depth_m) %>%
  summarise(mean_bbp_chla_ratio = mean(bbp_chla_ratio, na.rm = TRUE), n = n()) %>%
  as_tibble() %>%
  drop_na() %>%
  filter(depth_m <= 100)

df_viz <- df_viz %>%
  group_nest(wavelength)  %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, mean_bbp_chla_ratio))

res <- df_viz %>%
  unnest(res) %>%
  rename(owd = x, depth_m = y, mean_bbp_chla_ratio = z) %>%
  select(-data) %>%
  drop_na(mean_bbp_chla_ratio) %>%
  mutate(mean_bbp_chla_ratio = ifelse(mean_bbp_chla_ratio < 0, 0, mean_bbp_chla_ratio)) %>%
  mutate(wavelength = glue("{wavelength} (nm)"))

p <- res %>%
  ggplot(aes(x = owd, y = depth_m, z = mean_bbp_chla_ratio, fill = mean_bbp_chla_ratio)) +
  geom_isobands(color = NA, breaks = seq(0, 20, by = 0.1)) +
  paletteer::scale_fill_paletteer_c(
    "oompaBase::jetColors",
    trans = "sqrt",
    breaks = scales::breaks_pretty(n = 8),
    guide =
      guide_colorbar(
        barwidth = unit(8, "cm"),
        barheight = unit(0.2, "cm"),
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
  ) +
  geom_line(
    data = isolume,
    size = 1,
    aes(x = owd, y = depth_m, color = isolume),
    inherit.aes = FALSE) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.05)),
    breaks = scales::breaks_pretty(n = 8)
  ) +
  paletteer::scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  labs(
    y = "Depth (m)",
    title = "bbp/chla ratio at different wavelengths",
    subtitle = "Data from the Hydroscat."
  ) +
  facet_wrap(~wavelength, ncol = 2) +
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
  here("graphs/24_bbp_fchla_by_wavelength_hydroscat.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 8
)
