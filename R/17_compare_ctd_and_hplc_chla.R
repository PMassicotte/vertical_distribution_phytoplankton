# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Compare the Chla measured from the HPLC and the CTD.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

pigments <- vroom::vroom(here::here("data/raw/nutrients.csv"))

pigments <- pigments %>%
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
  select(-conc_mg_m2) %>%
  filter(pigment == "Chlorophyll a") %>%
  drop_na(conc_ug_l)

pigments

ctd <- fread(here::here("data/raw/ctd.csv")) %>%
  as_tibble()

ctd <- ctd %>%
  filter(str_detect(mission, "amundsen")) %>%
  filter(str_detect(station, "^G\\d*")) %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100, .after = station) %>%
  select(station, transect, cast, depth_m, flor_mg_m3) %>%
  mutate(depth2_m = depth_m)


# Match CTD and HPLC fluorescence -----------------------------------------

# Use a rolling join because the depths of measurements are not equal

setDT(ctd)
setDT(pigments)

# https://stackoverflow.com/questions/56502819/r-merge-rows-in-lookup-table-based-on-closest-match-and-exact-match-for-two-val

# a rolling join is performed on the last keys of the joined data.tables
df_viz <- ctd[pigments, on = c("station", "cast", "depth_m"), roll = "nearest"]

# Plot --------------------------------------------------------------------

df_viz %>%
  mutate(depth_difference = abs(depth2_m - depth_m)) %>%
  ggplot(aes(x = depth_difference)) +
  geom_histogram() +
  scale_x_log10()

p <- df_viz %>%
  ggplot(aes(x = flor_mg_m3, y = conc_ug_l, color = depth_m)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(color = "red", lty = 2) +
  geom_smooth(method = "lm") +
  scale_color_viridis_c() +
  labs(
    title = "Chla derived from HPLC and CTD",
    subtitle = "The dashed red line is the 1:1 line and the blue line is a linear model.",
    x = bquote("CTD Chla fluorescence" ~ (mgC~m^{-3})),
    y = bquote("HPLC Chla" ~ (mgC~m^{-3})),
    color = "Depth (m)"
  ) +
  annotation_logticks(sides = "bl", size = 0.25) +
  theme(
    axis.ticks = element_blank(),
    panel.border = element_blank()
  )

ggsave(
  here::here("graphs/17_scatterplot_ctd_vs_hplc_chla.pdf"),
  device = cairo_pdf
)
