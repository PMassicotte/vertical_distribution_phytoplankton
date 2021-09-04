# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Visualize phytoplankton biomass using MVP data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","interpolate_fun.R"))

# color <- c("#ECC5ED", "#E8AFE7", "#E491E0", "#E078DA", "#DC65D4", "#D84FCF", "#D548CA", "#D151C5", "#CD69C0", "#C87BBD", "#C38FBE", "#BA99C1", "#AF9BC8", "#9D95D2", "#8985DD", "#7270E7", "#5956F2", "#4643FA", "#333DFD", "#263FFA", "#1C45F3", "#1A50EE", "#1B60EA", "#2272E8", "#2C88EA", "#3898EF", "#45AEF5", "#53BFF9", "#60CCFA", "#6ADCF4", "#72E6ED", "#77ECE2", "#78EFD8", "#75EECE", "#6FE9C1", "#66E1B2", "#5BD8A1", "#4ECE91", "#40C682", "#33BF75", "#27BD6A", "#1CBD60", "#13C257", "#0CCA4E", "#07D544", "#05DD3C", "#06E236", "#09E531", "#0FE42D", "#17E02B", "#21DA2B", "#2CD42D", "#38CE31", "#44C937", "#4FC63F", "#5AC547", "#65C550", "#6FC758", "#79CA5F", "#83D064", "#8CD767", "#97DE68", "#A2E567", "#ADEB64", "#BAEF5F", "#C6EF58", "#D3EC4F", "#E0E444", "#EADA3B", "#F2CF33", "#F7C82C", "#F9BF26", "#FAB921", "#FBB31C", "#FCAC18", "#FCA514", "#FCA011", "#FC990E", "#FA930B", "#F88D09", "#F48607", "#EE8105", "#E87A04", "#E27202", "#DD6A02", "#DA6201", "#DA5901", "#DE4D02", "#E54402", "#EE3A03", "#F52C05", "#FA1F07", "#FB1309", "#F9090B", "#F3020E", "#EB0012", "#E10016", "#DA021A", "#D5051F", "#D30924", "#D30F2B", "#D51632", "#D72039", "#DB2D41", "#DF3C4A", "#E44D51", "#EA605A", "#F07462", "#F68A6D", "#FBA078", "#FEB483", "#FFC48E", "#FFC690")

df <- read_csv(here("data","raw","greenedge_mvp.csv")) %>%
  mutate(transect = case_when(
    str_detect(file_name, "2016001_01|2016001_02") ~ 100,
    str_detect(file_name, "2016001_05") ~ 400,
    str_detect(file_name, "2016001_04") ~ 300,
    str_detect(file_name, "2016001_08|2016001_09") ~ 600,
    str_detect(file_name, "2016001_06|2016001_07") ~ 500,
    str_detect(file_name, "2016001_03") ~ 200,
    TRUE ~ NA_real_
  )) %>%
  rename(
    longitude = initial_longitude_deg,
    latitude = initial_latitude_deg
  ) %>%
  mutate(longitude = -longitude) %>%
  drop_na(transect)

df

# Check there is only 1 observation at each lon,lat,depth combination
df %>%
  count(longitude, latitude, pres) %>%
  assertr::verify(n == 1)

df %>%
  distinct(longitude, latitude, .keep_all = TRUE) %>%
  ggplot(aes(x = longitude, y = latitude, color = factor(transect))) +
  geom_point()

df %>%
  ggplot(aes(x = longitude, y = pres)) +
  geom_point(size = 0.1) +
  scale_y_reverse() +
  facet_wrap(~transect, scales = "free_x")

# Fluorescence ------------------------------------------------------------

df_viz <- df %>%
  # filter(transect == 200) %>%
  filter(pres <= 100) %>%
  group_nest(transect) %>%
  mutate(res = map(
    data,
    interpolate_2d,
    longitude,
    pres,
    fluo,
    nx = 200,
    ny = 200
  )) %>%
  unnest(res) %>%
  rename(
    longitude = x,
    pres = y,
    fluo = z
  ) %>%
  select(-data) %>%
  mutate(fluo = ifelse(fluo < 0, 0, fluo))

p1 <- df_viz %>%
  ggplot(aes(x = longitude, y = pres, z = fluo, fill = fluo)) +
  geom_isobands(color = NA, bins = 200) +
  facet_wrap(~transect, scales = "free_x") +
  # scale_fill_gradientn(
  #   trans = "sqrt",
  #   colours = color, guide = guide_colorbar(title.position = "top", nrow = 1)
  # ) +
  paletteer::scale_fill_paletteer_c(
    "oompaBase::jetColors",
    # trans = "sqrt",
    limits = c(0, 8),
    oob = scales::squish,
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
    breaks = scales::breaks_pretty(n = 4)
  ) +
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

ggsave(
  here::here("graphs","05_phytoplankton_biomass_mvp_fluo.pdf"),
  device = cairo_pdf,
  height = 4,
  width = 8
)

# Beam attenuation (cp) ---------------------------------------------------

# https://www.seabird.com/c-star-25-cm-red-6000-m/product-details?id=54627910468&callback=pf

# Path Length: 25 cm
# Spectral Bandwidth: ~ 20 nm
# Wavelength: 650 nm

r <- 0.25

df_viz <- df %>%
  # filter(transect == 200) %>%
  filter(pres <= 100) %>%
  mutate(cp = -(1 / r) * log10(trans / 100)) %>%
  group_nest(transect) %>%
  mutate(res = map(
    data,
    interpolate_2d,
    longitude,
    pres,
    cp,
    nx = 200,
    ny = 200
  )) %>%
  unnest(res) %>%
  rename(
    longitude = x,
    pres = y,
    cp = z
  ) %>%
  select(-data) %>%
  mutate(cp = ifelse(cp < 0, 0, cp))

p <- df_viz %>%
  ggplot(aes(x = longitude, y = pres, z = cp, fill = cp)) +
  geom_isobands(color = NA, bins = 200) +
  facet_wrap(~transect, scales = "free_x") +
  paletteer::scale_fill_paletteer_c(
    "oompaBase::jetColors",
    # trans = "sqrt",
    limits = c(0, 0.5),
    oob = scales::squish,
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
    breaks = scales::breaks_pretty(n = 4)
  ) +
  labs(
    y = "Depth (m)",
    fill = "Cp @ 650 nm"
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
  here::here("graphs","05_phytoplankton_biomass_mvp_cp.pdf"),
  device = cairo_pdf,
  height = 4,
  width = 8
)

# Explore CP deeply in the water column -----------------------------------

r <- 0.25

df_viz <- df %>%
  mutate(cp = -(1 / r) * log10(trans / 100))

df_viz

# df_viz %>%
#   ggplot(aes(x = longitude, y = pres)) +
#   geom_point(size = 0.1) +
#   scale_y_reverse() +
#   facet_wrap(~transect, scales = "free_x")

p1 <- df_viz %>%
  group_by(transect, longitude) %>%
  filter(pres == max(pres)) %>%
  ggplot(aes(x = longitude, y = pres)) +
  geom_line() +
  scale_y_reverse(limits = c(NA, 0)) +
  facet_wrap(~transect, scales = "free_x", ncol = 1) +
  labs(
    x = "Longitude",
    y = "Depth (m)",
    title = "Bottom profiles of the MVP"
  ) +
  theme(plot.title = element_text(size = 10))

p2 <- df_viz %>%
  group_by(transect, longitude) %>%
  filter(pres == max(pres)) %>%
  ggplot(aes(x = longitude, y = cp)) +
  geom_line() +
  facet_wrap(~transect, scales = "free_x", ncol = 1) +
  labs(
    x = "Longitude",
    y = quote("Particle beam attenuation coefficient" ~ (m^{-1})),
    title = "Bottom CP profiles"
  ) +
  theme(plot.title = element_text(size = 10))

p <- p1 + p2

ggsave(
  here("graphs","05_deepest_cp_longitudinal_profil.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 8
)
