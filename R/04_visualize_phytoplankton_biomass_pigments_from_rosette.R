# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Visualize the vertical distribution of phytoplankton using
# pigments.
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

# Prepare the data --------------------------------------------------------

df <- vroom::vroom(here::here("data/raw/nutrients.csv"))

df <- df %>%
  filter(mission == "amundsen_2016") %>%
  filter(sample_type == "water") %>%
  filter(sample_source == "rosette") %>%
  filter(str_starts(station, "G")) %>%
  select(
    mission,
    station,
    station_type,
    date,
    longitude,
    latitude,
    cast,
    bottle,
    depth_m,
    pigment,
    starts_with("conc"),
    method
  ) %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100) %>%
  drop_na(depth_m) %>%
  select(-conc_mg_m2)

df

# Associate the number of open water day

owd <- read_csv("https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/Randelhoff-et-al-2019_GreenEdge_per-station_v1.0.csv", na = "NaN") %>%
  janitor::clean_names() %>%
  select(station, owd)

# df %>%
#   anti_join(owd, by = "station") %>%
#   distinct(station)

df <- df %>%
  inner_join(owd, by = "station") %>%
  relocate(owd, .after = date)

# Summary of the data -----------------------------------------------------

df %>%
  distinct(longitude, latitude, transect) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  ggplot() +
  geom_sf(aes(color = factor(transect)))

df %>%
  count(station, depth_m) %>%
  arrange(station, depth_m)

# Which pigments are available? -------------------------------------------

p <- df %>%
  count(pigment, sort = TRUE) %>%
  mutate(pigment = fct_reorder(pigment, n)) %>%
  ggplot(aes(x = n, y = pigment)) +
  geom_col() +
  geom_text(aes(label = n), size = 2, color = "white", hjust = 1.2) +
  labs(
    x = "Number of observations",
    y = NULL,
    title = "Overview of available pigments from the rosette"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 14),
    axis.ticks = element_blank(),
    panel.border = element_blank()
  )

ggsave(
  here::here("graphs/04_available_pigments_rosette.pdf"),
  device = cairo_pdf
)

# Average by cast and depth -----------------------------------------------

df %>%
  count(station, cast)

df_viz <- df %>%
  group_by(station, transect, longitude, latitude, pigment, depth_m) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), n = n()) %>%
  ungroup() %>%
  select(-cast, -bottle)

# Categorize pigments into two categories ---------------------------------

df_viz <- df_viz %>%
  filter(
    pigment %in% c(
      "Zeaxanthin",
      "Diatoxanthin",
      "Diadinoxanthin",
      "Sum 19HF-like",
      "Sum 19BF-like",
      "Peridinin",
      "Fucoxanthin",
      "Chlorophyll c3",
      "Chlorophyll c1+c2+MgDVP",
      "Chlorophyll b",
      "Antheraxanthin",
      "Violaxanthin",
      "Lutein",
      "Total Chlorophyll a"
    )
  )

df_viz %>%
  distinct(pigment)

df_viz <- df_viz %>%
  mutate(
    pigment_group = case_when(
      pigment %in% c(
        "Zeaxanthin",
        "Diatoxanthin",
        "Diadinoxanthin",
        "Antheraxanthin",
        "Violaxanthin"
      ) ~ "photoprotection",
      pigment %in%
        c(
          "Sum 19HF-like",
          "Sum 19BF-like",
          "Peridinin",
          "Fucoxanthin",
          "Chlorophyll c3",
          "Chlorophyll c1+c2+MgDVP",
          "Chlorophyll b",
          "Lutein"
        ) ~ "accessory",
      pigment == "Total Chlorophyll a" ~ "tchla",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(station, transect, longitude, latitude, depth_m, owd, pigment_group) %>%
  summarise(sum_conc_mg_m3 = sum(conc_mg_m3, na.rm = TRUE)) %>%
  ungroup()

df_viz

df_viz %>%
  count(station, transect, depth_m, pigment_group) %>%
  assertr::verify(n == 1)

# Normalize by total chla -------------------------------------------------

df_viz <- df_viz %>%
  pivot_wider(names_from = pigment_group, values_from = sum_conc_mg_m3) %>%
  pivot_longer(c(accessory, photoprotection),
    names_to = "pigment_group",
    values_to = "sum_conc_mg_m3"
  ) %>%
  mutate(chla_normalized_sum_conc_mg_m3 = sum_conc_mg_m3 / tchla)

df_viz

# Interpolation -----------------------------------------------------------

df_viz <- df_viz %>%
  pivot_longer(c(chla_normalized_sum_conc_mg_m3, sum_conc_mg_m3)) %>%
  group_nest(pigment_group, name) %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, value)) %>%
  unnest(res) %>%
  rename(owd = x, depth_m = y, sum_conc_mg_m3 = z) %>%
  select(-data) %>%
  mutate(sum_conc_mg_m3 = ifelse(sum_conc_mg_m3 < 0, 0, sum_conc_mg_m3)) %>%
  drop_na(sum_conc_mg_m3)
# %>%
#   pivot_wider(names_from = name, values_from = sum_conc_mg_m3)

df_viz

df_viz %>%
  group_by(pigment_group, name) %>%
  skimr::skim()

df_viz %>%
  group_by(pigment_group) %>%
  summarise(mean_se(sum_conc_mg_m3))

# Plot --------------------------------------------------------------------

p1 <- df_viz %>%
  filter(pigment_group == "photoprotection") %>%
  ggplot(aes(x = owd, y = depth_m, z = sum_conc_mg_m3, fill = sum_conc_mg_m3)) +
  geom_isobands(color = NA, breaks = seq(0, 5, by = 0.015)) +
  facet_wrap(~name) +
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
    fill = bquote("Photoprotection pigments"~(mg~m^{-3})),
    title = "Photoprotection pigments"
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

p2 <- df_viz %>%
  filter(pigment_group == "accessory") %>%
  ggplot(aes(x = owd, y = depth_m, z = sum_conc_mg_m3, fill = sum_conc_mg_m3)) +
  geom_isobands(color = NA, breaks = seq(0, 50, by = 0.025)) +
  facet_wrap(~name) +
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
    fill = bquote("Accessory pigments"~(mg~m^{-3})),
    title = "Accessory pigments"
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

p <- p1 + p2 +
  plot_layout(ncol = 1)

ggsave(
  here::here("graphs/04_phytoplankton_biomass_pigments.pdf"),
  device = cairo_pdf,
  width = 10,
  height = 8
)
