# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore the vertical distribution of the phytoplankton biomass
# using different kind of proxy.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/interpolate_fun.R")

# Isolume data ------------------------------------------------------------

isolume <-
  read_csv(
    "https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/FIGURE_9-c-d-e.csv"
  ) %>%
  janitor::clean_names() %>%
  select(owd, starts_with("isolume")) %>%
  pivot_longer(starts_with("isolume"), names_to = "isolume", values_to = "depth_m")

# CTD data ----------------------------------------------------------------

ctd <- read_csv(here::here("data/clean/ctd.csv"))

transect <- ctd %>%
  distinct(transect, latitude) %>%
  group_by(transect) %>%
  summarise(latitude = median(latitude)) %>%
  mutate(longitude = -65)

p1 <- ctd %>%
  distinct(station, transect, longitude, latitude, owd) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  ggplot(aes(color = owd)) +
  geom_sf(size = 3) +
  geom_label(
    data = transect,
    aes(
      x = longitude,
      y = latitude,
      label = glue("T {transect}")
    ),
    inherit.aes = FALSE,
    size = 2,
    color = "gray50",
    label.size = NA
  ) +
  scale_colour_gradient2(
    low = "#326391",
    mid = "#DBD3C5",
    high = "#A84222",
    breaks = scales::breaks_pretty(n = 6),
    midpoint = 0,
    guide =
      guide_colorbar(
        barwidth = unit(8, "cm"),
        barheight = unit(0.2, "cm"),
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  theme(
    legend.position = "bottom",
    axis.title = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

p1

ggsave(
  here::here("graphs/02_map_owd.pdf"),
  device = cairo_pdf
)

knitr::plot_crop(here::here("graphs/02_map_owd.pdf"))

# CTD fluorescence --------------------------------------------------------

# Summarize by depth and open water day
df_viz <- ctd %>%
  dtplyr::lazy_dt() %>%
  group_by(owd, depth_m) %>%
  summarise(mean_fluo = mean(flor_mg_m3, na.rm = TRUE), n = n()) %>%
  as_tibble() %>%
  drop_na() %>%
  filter(depth_m <= 100)

# Just an idea, we could use boxplot/manova to determine if there are
# differences in the data.

isolume2 <- isolume %>%
  rename(isolume_depth_m = depth_m)

res <- df_viz %>%
  inner_join(isolume2, by = "owd") %>%
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

res

res %>%
  filter(isolume == "isolume_01") %>%
  ggplot(aes(x = above_isolume, y = mean_fluo, fill = ice_covered)) +
  geom_boxplot(size = 0.25, outlier.size = 0.5) +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  labs(
    x = NULL,
    y = "Mean fluorescence (CTD)",
    fill = NULL
  ) +
  paletteer::scale_fill_paletteer_d("nord::aurora")

ggsave(
  here::here("graphs/02_boxplot_ctd_fluorescence_isolume_owd.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 4
)

# Maybe bin the depths before interpolation? Use the mean between certain ranges
# of depths?

df_viz <- df_viz %>%
  nest(data = everything()) %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, mean_fluo))

# Do the plot
p2 <- df_viz %>%
  unnest(res) %>%
  rename(owd = x, depth_m = y, mean_fluo = z) %>%
  select(-data) %>%
  mutate(mean_fluo = ifelse(mean_fluo < 0, 0, mean_fluo)) %>%
  ggplot(aes(x = owd, y = depth_m, z = mean_fluo, fill = mean_fluo)) +
  geom_isobands(color = NA, breaks = seq(0, 20, by = 0.25)) +
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
  geom_line(
    data = isolume,
    size = 1,
    aes(x = owd, y = depth_m, color = isolume),
    inherit.aes = FALSE) +
  paletteer::scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  labs(
    y = "Depth (m)"
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

# Particle beam attenuation (CP) ------------------------------------------

# Summarize by depth and open water day
df_viz <- ctd %>%
  dtplyr::lazy_dt() %>%
  group_by(owd, depth_m) %>%
  summarise(mean_cp = mean(cp, na.rm = TRUE), n = n()) %>%
  as_tibble() %>%
  drop_na() %>%
  filter(depth_m <= 100)

df_viz %>%
  ggplot(aes(x = mean_cp)) +
  geom_histogram()

 # Maybe bin the depths before interpolation? Use the mean between certain ranges
# of depths?

df_viz <- df_viz %>%
  nest(data = everything()) %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, mean_cp))

p3 <- df_viz %>%
  unnest(res) %>%
  rename(owd = x, depth_m = y, mean_cp = z) %>%
  select(-data) %>%
  mutate(mean_cp = ifelse(mean_cp < 0, 0, mean_cp)) %>%
  # filter(between(mean_cp, 0, 1)) %>%
  ggplot(aes(x = owd, y = depth_m, z = mean_cp, fill = mean_cp)) +
  geom_isobands(color = NA, breaks = seq(0, 2, by = 0.02)) +
  paletteer::scale_fill_paletteer_c(
    "oompaBase::jetColors",
    trans = "sqrt",
    # oob = scales::squish,
    breaks = scales::breaks_pretty(n = 6),
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
    fill = "Cp @ 657 nm"
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

# Ratio chla/cp -----------------------------------------------------------

df_viz <- ctd %>%
  filter(depth_m <= 100)

df_viz

df_viz <- df_viz %>%
  dtplyr::lazy_dt() %>%
  group_by(owd, depth_m) %>%
  summarise(
    mean_fluo = mean(flor_mg_m3, na.rm = TRUE),
    mean_cp = mean(cp, na.rm = TRUE),
    n = n()) %>%
  as_tibble() %>%
  drop_na(mean_fluo, mean_cp) %>%
  mutate(mean_chla_cp_ratio = mean_fluo / mean_cp)

df_viz <- df_viz %>%
  nest(data = everything()) %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, mean_chla_cp_ratio)) %>%
  select(-data) %>%
  unnest(res) %>%
  rename(
    owd = x,
    depth_m = y,
    mean_chla_cp_ratio = z
  ) %>%
  mutate(mean_chla_cp_ratio = ifelse(mean_chla_cp_ratio < 0, 0, mean_chla_cp_ratio))

df_viz

range(df_viz$mean_chla_cp_ratio, na.rm = TRUE)

p4 <- df_viz %>%
  ggplot(aes(x = owd, y = depth_m, z = mean_chla_cp_ratio, fill = mean_chla_cp_ratio)) +
  geom_isobands(color = NA, breaks = seq(0, 50, by = 1)) +
  paletteer::scale_fill_paletteer_c(
    "oompaBase::jetColors",
    # trans = "sqrt",
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
  geom_line(data = isolume, size = 1, aes(x = owd, y = depth_m, color = isolume), inherit.aes = FALSE) +
  paletteer::scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  labs(
    y = "Depth (m)",
    fill = "chla/cp ratio"
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

# Save plots --------------------------------------------------------------

p <- p2 + p3 + p4 +
  plot_layout(ncol = 1) +
  plot_annotation(
    tag_levels = "A",
    title = str_wrap("Phytoplankton vertical distribution using data from the CTD", 50),
    theme = theme(plot.title = element_text(hjust = 0))
  )

ggsave(
  here::here("graphs/02_phytoplankton_biomass_ctd.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 12
)

# Correlation between CP and fluorescence ---------------------------------

df_viz <- ctd %>%
  filter(depth_m <= 100)

p <- df_viz %>%
  ggplot(aes(x = flor_mg_m3, y = cp)) +
  geom_hex(bins = 50) +
  scale_y_log10() +
  scale_x_log10() +
  scale_fill_viridis_c() +
  annotation_logticks() +
  labs(
    x = bquote("Fluorescence"~(mg~m^{-3})),
    y = bquote("Particle beam attenuation coefficient"~(m^{-1})),
    title = "CP vs fluorescence",
    subtitle = "For the first 100 meters of the water column. Data from the CTD."
  ) +
  geom_smooth(method = "lm", color = "red", size = 0.25) +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  )

ggsave(
  here::here("graphs/02_scatterplot_fluorescence_vs_cp.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 7
)

# Scatterplot by transect -------------------------------------------------

p +
  facet_wrap(~transect) +
  ggsave(
    here("graphs/02_scatterplot_fluorescence_vs_cp_by_transect.pdf"),
    device = cairo_pdf,
    width = 7,
    height = 7
  )

# Scatterplot by groups ---------------------------------------------------

# Attention, there are quite a lot of CTD data with an OWD that are not found in
# the isolume provided by Achim.

df_viz2 <- df_viz %>%
  inner_join(isolume %>% rename(isolume_depth_m = depth_m), by = "owd") %>%
  mutate(is_water_open = ifelse(owd < 0, "Ice covered", "Open water")) %>%
  mutate(
    above_isolume = case_when(
      depth_m < isolume_depth_m ~ "Above the 0.1 isolume",
      TRUE ~ "Below the 0.1 isolume"
    )
  )

df_viz2

p <- df_viz2 %>%
  filter(isolume == "isolume_01") %>%
  ggplot(aes(x = flor_mg_m3, y = cp, color = isolume_depth_m)) +
  geom_hex(bins = 50) +
  scale_y_log10() +
  scale_x_log10() +
  scale_fill_viridis_c() +
  annotation_logticks() +
  labs(
    x = bquote("Fluorescence"~(mg~m^{-3})),
    y = bquote("Particle beam attenuation coefficient"~(m^{-1})),
    title = "CP vs fluorescence",
    subtitle = str_wrap("For the first 100 meters of the water column. Data from the CTD.", 120)
  ) +
  facet_grid(above_isolume~is_water_open) +
  geom_smooth(method = "lm", color = "red", size = 0.5) +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 8),
    aspect.ratio = 1
  )

ggsave(
  here("graphs/02_scatterplot_fluorescence_vs_cp_isolume_depth.pdf"),
  device = cairo_pdf,
  height = 8,
  width = 8
)

# Vertical profiles of CP at the deepest stations -------------------------

ctd

df_viz <- ctd %>%
  filter(depth_m >= 500) %>%
  group_nest(station) %>%
  mutate(n = map_int(data, nrow)) %>%
  slice_max(order_by = n, n = 25) %>%
  unnest(data) %>%
  mutate(station = glue("{station} ({as.Date(date_time)})"))

p <- df_viz %>%
  ggplot(aes(x = cp, y = depth_m)) +
  geom_path(size = 0.25) +
  geom_vline(xintercept = 0, lty = 2, color = "red") +
  scale_y_reverse() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 4)) +
  facet_wrap(~station, scales = "fixed") +
  labs(
    title = "CP vertical profiles",
    subtitle = "Showing the 25 deepest stations (only starting at 500 meters). Data from the CTD.",
    x = bquote("Particle beam attenuation coefficient"~(m^{-1})),
    y = "Depth (m)"
  ) +
  theme(
    panel.spacing = unit(1.25, "lines")
  )

ggsave(
  here("graphs/02_vertical_profiles_cp.pdf"),
  device = cairo_pdf,
  height = 8,
  width = 9
)
