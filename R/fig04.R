# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Visualize the vertical distribution of phytoplankton using
# pigments.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","interpolate_fun.R"))
source(here("R","plot_funs.R"))
source(here("R","utils.R"))

breaks <- c(-30, -10, 10, 40)

# Isolume data ------------------------------------------------------------

isolume <-
  read_csv(
    "https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/FIGURE_9-c-d-e.csv"
  ) %>%
  janitor::clean_names() %>%
  select(owd, isolume_01) %>%
  pivot_longer(starts_with("isolume"), names_to = "isolume", values_to = "depth_m")

# Prepare the data --------------------------------------------------------

df <- read_csv(here::here("data","raw","nutrients.csv"))

df <- df %>%
  filter(mission == "amundsen_2016") %>%
  filter(sample_type == "water") %>%
  filter(sample_source == "rosette") %>%
  filter(str_starts(station, "G")) %>%
  filter(method == "HPLC") %>%
  filter(!str_detect(sample_code, regex("flash", ignore_case = TRUE))) %>%
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
  select(-conc_mg_m2, -conc_ug_l) %>%
  drop_na(conc_mg_m3)

df

# Should have only 1 measure per cast, station, depth and pigment

df %>%
  count(station, cast, depth_m, pigment, sort = TRUE) %>%
  assertr::verify(n == 1)

# Associate the number of open water day

owd <- read_csv("https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/Randelhoff-et-al-2019_GreenEdge_per-station_v1.0.csv", na = "NaN") %>%
  janitor::clean_names() %>%
  select(station, owd)

# Station 3000 has no associated OWD

df %>%
  anti_join(owd, by = "station") %>%
  distinct(station)

df <- df %>%
  inner_join(owd, by = "station") %>%
  relocate(owd, .after = date)

df

# Summary of the data -----------------------------------------------------

df %>%
  distinct(longitude, latitude, transect) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  ggplot() +
  geom_sf(aes(color = factor(transect)))

df %>%
  count(station, depth_m) %>%
  arrange(station, depth_m)

# Average by cast and depth -----------------------------------------------

df %>%
  count(station, cast, depth_m, pigment, sort = TRUE)

pigments <- df %>%
  group_by(station, transect, longitude, latitude, pigment, depth_m) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), n = n()) %>%
  ungroup() %>%
  select(-cast, -bottle)

pigments

# Categorize pigments into few categories ---------------------------------

# This was validated by Josephine Ras

pigments %>%
  pull(pigment) %>%
  unique()

pigments <- pigments %>%
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
      "Total Chlorophyll a",
      "Sum Phaeophytin a",
      "sum Phbd a",
      "sum Chld a"
    )
  )

pigments %>%
  distinct(pigment)

pigments <- pigments %>%
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
        ) ~ "photosynthetic_pigments",
      pigment == "Total Chlorophyll a" ~ "tchla",
      pigment == "Sum Phaeophytin a" ~ "phaeophytin_a",
      pigment == "sum Phbd a" ~ "phaeophorbide_a",
      pigment == "sum Chld a" ~ "chlorophyllide_a",
      TRUE ~ NA_character_
    )
  )

pigments

pigments %>%
  distinct(pigment_group)

# Verify that we still have 1 pigment measurement per station and depth
pigments %>%
  count(station, depth_m, owd, pigment) %>%
  assertr::verify(n == 1)

## Sum pigments per group ----

pigments <- pigments %>%
  group_by(station, transect, longitude, latitude, depth_m, owd, pigment_group) %>%
  summarise(sum_conc_mg_m3 = sum(conc_mg_m3, na.rm = TRUE)) %>%
  ungroup()

pigments

pigments %>%
  count(station, transect, depth_m, pigment_group) %>%
  assertr::verify(n == 1)

# We should have 6 groups of pigments
pigments %>%
  summarise(n = n_distinct(pigment_group)) %>%
  assertr::verify(n == 6)

pigments %>%
  write_csv(here("data/clean/pigments_grouped.csv"))

# TODO: Not all station/depth_m pairs have the 6 groups of pigments. For
# example, station 100 at depth 1.88, there is no sum Phbd a (Phaeophorbide a)

pigments %>%
  count(station, depth_m)

# Normalize by total chla -------------------------------------------------

# TODO
# df_viz <- df_viz %>%
#   pivot_wider(names_from = pigment_group, values_from = sum_conc_mg_m3) %>%
#   pivot_longer(
#     c(
#       chlorophyllide_a,
#       phaeophytin_a,
#       photoprotection,
#       photosynthetic_pigments,
#       phaeophorbide_a
#     ),
#     names_to = "pigment_group",
#     values_to = "sum_conc_mg_m3"
#   ) %>%
#   mutate(chla_normalized_sum_conc_mg_m3 = sum_conc_mg_m3 / tchla)

pigments

# Summarize by depth and open water day -----------------------------------

pigments %>%
  count(owd, depth_m)

df_mean <- pigments %>%
  dtplyr::lazy_dt() %>%
  group_by(owd, pigment_group, depth_m) %>%
  summarise(across(c(sum_conc_mg_m3), ~mean(., na.rm = TRUE), .names = "mean_{.col}")) %>%
  as_tibble() %>%
  drop_na()

df_mean

# Interpolation -----------------------------------------------------------

df_viz <- df_mean %>%
  group_nest(pigment_group) %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, mean_sum_conc_mg_m3, h = 6)) %>%
  unnest(res) %>%
  rename(owd = x, depth_m = y, mean_sum_conc_mg_m3 = z) %>%
  select(-data) %>%
  mutate(mean_sum_conc_mg_m3 = ifelse(mean_sum_conc_mg_m3 < 0, 0, mean_sum_conc_mg_m3)) %>%
  drop_na(mean_sum_conc_mg_m3)

df_viz

df_viz <- df_viz %>%
  pivot_wider(
    names_from = pigment_group,
    values_from = mean_sum_conc_mg_m3,
    names_prefix = "mean_"
  )

pigments %>%
  distinct(pigment_group)

# Photoprotection pigments ------------------------------------------------

# 3D plot

range(df_viz$mean_photoprotection, na.rm = TRUE)

p1 <- gg3d(
  df = df_viz,
  x = owd,
  y = depth_m,
  z = mean_photoprotection,
  iso_breaks = seq(0, 2, by = 0.01),
  fill_text = expression("Photoprotection pigments"~(mg~m^{-3})),
  isolume = isolume,
  nbreaks = 6,
  trans_fun = scales::trans_new("shift", function(x) x, identity)
)

# Average vertical profiles

df_average_profiles <- average_vertical_profiles(df_viz, mean_photoprotection, breaks = breaks)

p2 <- gg2dprofiles(df_average_profiles, mean_photoprotection, depth_m, owd_bin)

# Photosynthetic pigments -------------------------------------------------

# 3D plot

range(df_viz$mean_photosynthetic_pigments, na.rm = TRUE)

p3 <- gg3d(
  df = df_viz,
  x = owd,
  y = depth_m,
  z = mean_photosynthetic_pigments,
  iso_breaks = seq(0, 10, by = 0.1),
  fill_text = expression("Photosynthetic pigments"~(mg~m^{-3})),
  isolume = isolume,
  nbreaks = 6
)

# Average vertical profiles

df_average_profiles <- average_vertical_profiles(df_viz, mean_photosynthetic_pigments, breaks = breaks)

p4 <- gg2dprofiles(df_average_profiles, mean_photosynthetic_pigments, depth_m, owd_bin)


# Chlorophyllide-a --------------------------------------------------------

# 3D plot

range(df_viz$mean_chlorophyllide_a, na.rm = TRUE)

p5 <- gg3d(
  df = df_viz,
  x = owd,
  y = depth_m,
  z = mean_chlorophyllide_a,
  iso_breaks = seq(0, 2, by = 0.001),
  fill_text = expression("Chlorophyllide-a"~(mg~m^{-3})),
  isolume = isolume,
  nbreaks = 6
)

# Average vertical profiles

df_average_profiles <- average_vertical_profiles(df_viz, mean_chlorophyllide_a, breaks = breaks)

p6 <- gg2dprofiles(df_average_profiles, mean_chlorophyllide_a, depth_m, owd_bin)


# Phaeophytin-a -----------------------------------------------------------

# 3D plot

range(df_viz$mean_phaeophytin_a, na.rm = TRUE)

p7 <- gg3d(
  df = df_viz,
  x = owd,
  y = depth_m,
  z = mean_chlorophyllide_a,
  iso_breaks = seq(0, 2, by = 0.001),
  fill_text = expression("Phaeophytin-a"~(mg~m^{-3})),
  isolume = isolume,
  nbreaks = 6
)

# Average vertical profiles

df_average_profiles <- average_vertical_profiles(df_viz, mean_phaeophytin_a, breaks = breaks)

p8 <- gg2dprofiles(df_average_profiles, mean_phaeophytin_a, depth_m, owd_bin)


# Phaeophorbide-a ---------------------------------------------------------

# 3D plot

range(df_viz$mean_phaeophorbide_a, na.rm = TRUE)

p9 <- gg3d(
  df = df_viz,
  x = owd,
  y = depth_m,
  z = mean_phaeophorbide_a,
  iso_breaks = seq(0, 2, by = 0.01),
  fill_text = expression("Phaeophorbide-a"~(mg~m^{-3})),
  isolume = isolume,
  nbreaks = 6
)

# Average vertical profiles

df_average_profiles <- average_vertical_profiles(df_viz, mean_phaeophorbide_a, breaks = breaks)

p10 <- gg2dprofiles(df_average_profiles, mean_phaeophorbide_a, depth_m, owd_bin)



# Save plots --------------------------------------------------------------

p <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 +
  plot_layout(ncol = 2, widths = c(0.7, 0.3)) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here::here("graphs","fig04.pdf"),
  device = cairo_pdf,
  width = 9,
  height = 16
)

