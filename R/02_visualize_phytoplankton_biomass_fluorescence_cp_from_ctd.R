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

ctd <- vroom::vroom(here::here("data/clean/ctd.csv"), altrep = TRUE)

p1 <- ctd %>%
  distinct(station, transect, longitude, latitude, owd) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  ggplot(aes(color = owd)) +
  geom_sf() +
  scale_color_binned(
    breaks = scales::breaks_pretty(n = 6),
    type = "viridis",
    guide =
      guide_colorbar(
        barwidth = unit(8, "cm"),
        barheight = unit(0.2, "cm"),
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
  ) +
  theme(
    legend.position = "bottom"
  )

p1

# CTD fluorescence --------------------------------------------------------

# Summarize by depth and open water day
df_viz <- ctd %>%
  dtplyr::lazy_dt() %>%
  group_by(owd, depth_m) %>%
  summarise(mean_fluo = mean(flor_mg_m3, na.rm = TRUE), n = n()) %>%
  as_tibble() %>%
  drop_na() %>%
  filter(depth_m <= 100)

# Maybe bin the depths before interpolation? Use the mean between certain ranges
# of depths?

df_viz <- df_viz %>%
  nest(data = everything()) %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, mean_fluo))

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
  geom_line(data = isolume, size = 1, aes(x = owd, y = depth_m, color = isolume), inherit.aes = FALSE) +
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

# Particle beam attenuation coefficient (CP) ------------------------------

# ftp://ftp.nodc.noaa.gov/nodc/archive/arc0022/0001155/1.1/data/1-data/docs/PI-NOTES/arabian/Gardner-beamcp.htm

# Beam transmission was converted to beam attenuation coefficients using
# c=-(1/r)*ln(%Tr/100) where c=beam attenuation coefficient (m^-1), r=beam path
# length (m), and Tr=% beam transmission.

# Check Table 1 for an idea of the range of Cp.
# https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2015JC010878

# 25 cm beam, confirmed by Pascal Guillot
r <- 0.25

# From Pascal:

# Voici les infos (Thomas, un des techs de AS a contacté SeaBird):
#
# All the instruments listed have a wavelength of 657nm (using a red LED):
#
# CST-558DR – 657nm
# CST-671DR – 657nm
# CST-2021 – 657nm
# CST-2022 - 657nm

ctd %>%
  drop_na(tran_percent) %>%
  ggplot(aes(x = tran_percent)) +
  geom_histogram(binwidth = 1)

# A lot of transmittance >= 100, problem! Email sent to Pascal Guillot to
# understand what is going on.
ctd %>%
  drop_na(tran_percent) %>%
  filter(tran_percent > 100) %>%
  ggplot(aes(x = tran_percent)) +
  geom_histogram()

ctd %>%
  drop_na(tran_percent) %>%
  count(tran_percent >= 100) %>%
  mutate(prop = n / sum(n))

ctd %>%
  drop_na(tran_percent) %>%
  filter(depth_m <= 100, transect == 100) %>%
  ggplot(aes(x = tran_percent, y = depth_m, group = interaction(station, transect))) +
  geom_path(size = 0.25) +
  scale_y_reverse() +
  facet_wrap(~station, scales = "free")

# After a discussion with Pascal, he told me to rescale the transmittance data
# between 0-100%.

ctd <- ctd %>%
  mutate(tran_percent = scales::rescale(tran_percent, to = c(0.001, 99.999)))

# Summarize by depth and open water day
df_viz <- ctd %>%
  dtplyr::lazy_dt() %>%
  group_by(owd, depth_m) %>%
  summarise(mean_tran_percent = mean(tran_percent, na.rm = TRUE), n = n()) %>%
  as_tibble() %>%
  drop_na() %>%
  filter(depth_m <= 100)

# Calculate the beam attenuation coefficient
df_viz <- df_viz %>%
  mutate(cp = -(1 / r) * log10(mean_tran_percent / 100))

df_viz %>%
  ggplot(aes(x = cp)) +
  geom_histogram()

df_viz <- df_viz %>%
  filter(cp <= 5)

 # Maybe bin the depths before interpolation? Use the mean between certain ranges
# of depths?

df_viz <- df_viz %>%
  nest(data = everything()) %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, cp))

p3 <- df_viz %>%
  unnest(res) %>%
  rename(owd = x, depth_m = y, mean_cp = z) %>%
  select(-data) %>%
  mutate(mean_cp = ifelse(mean_cp < 0, 0, mean_cp)) %>%
  # filter(between(mean_cp, 0, 1)) %>%
  ggplot(aes(x = owd, y = depth_m, z = mean_cp, fill = mean_cp)) +
  geom_isobands(color = NA, breaks = seq(0, 10, by = 0.05)) +
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
  filter(depth_m <= 100) %>%
  mutate(cp = -(1 / r) * log10(tran_percent / 100))

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
  mutate(mean_chla_cp_ratio = mean_fluo / mean_cp) %>%
  filter(depth_m <= 100)

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

p <- ctd %>%
  filter(depth_m <= 100) %>%
  mutate(cp = -(1 / r) * log10(tran_percent / 100)) %>%
  ggplot(aes(x = flor_mg_m3, y = cp)) +
  geom_hex(bins = 100) +
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
    legend.position = "none"
  )

ggsave(
  here::here("graphs/02_scatterplot_fluorescence_vs_cp.pdf"),
  device = cairo_pdf
)

# More exploration. Attention, there are quite a lot of CTD data with an OWD
# that are not found in the isolume provided by Achim.

df_viz <- ctd %>%
  filter(depth_m <= 100) %>%
  mutate(cp = -(1 / r) * log10(tran_percent / 100)) %>%
  inner_join(isolume %>% rename(isolume_depth_m = depth_m), by = "owd") %>%
  mutate(is_water_open = ifelse(owd < 0, "Ice covered", "Open water"))

df_viz

p <- df_viz %>%
  ggplot(aes(x = flor_mg_m3, y = cp, color = isolume_depth_m)) +
  geom_point(size = 0.25) +
  # geom_hex(bins = 100) +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_viridis_c() +
  annotation_logticks() +
  labs(
    x = bquote("Fluorescence"~(mg~m^{-3})),
    y = bquote("Particle beam attenuation coefficient"~(m^{-1})),
    title = "CP vs fluorescence",
    subtitle = str_wrap("For the first 100 meters of the water column. Data from the CTD. Observations were divided into two groups: ice covered with OWD < 0 or open water when OWD >= 0.", 120)
  ) +
  geom_smooth(method = "lm", color = "red", size = 0.5) +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(size = 8)
  ) +
  facet_grid(isolume~is_water_open)

ggsave(
  here::here("graphs/02_scatterplot_fluorescence_vs_cp_isolume_depth.pdf"),
  device = cairo_pdf,
  height = 6,
  width = 8
)
