# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Maps showing the spatial distribution of the data to be used to
# calculate primary production.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

station <- vroom::vroom("data/clean/ctd.csv") %>%
  distinct(station, transect, longitude, latitude)

kdpar_transmittance <- read_csv(here::here("data/clean/cops_kd_par_transmittance.csv")) %>%
  distinct(station, transect, longitude, latitude) %>%
  mutate(data = "kdpar (cops)")

hourly_par <- read_csv(here::here("data/clean/sbdart_hourly_par_0p.csv")) %>%
  distinct(station, transect) %>%
  inner_join(station, by = c("station", "transect")) %>%
  mutate(data = "sbdart (hourly par)")

pvse <- read_csv("/media/4TB/work-ulaval/projects/green_edge/green_edge/data/pe-curves/photosynthetic_parameters_amundsen_2016.csv") %>%
  distinct(station) %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100) %>%
  inner_join(station, by = c("station", "transect")) %>%
  mutate(data = "pvse")

ctd_fluorescence <- read_csv(here::here("data/clean/ctd_fluorescence_npq_corrected.csv")) %>%
  distinct(station, transect, cast, longitude, latitude) %>%
  mutate(data = "ctd fluorescence (chla)")

df <- bind_rows(ctd_fluorescence, hourly_par, kdpar_transmittance, pvse)

# Plot --------------------------------------------------------------------

p <- df %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  ggplot() +
  geom_sf() +
  facet_wrap(~data) +
  labs(
    title = str_wrap("Geographic positions of the variables needed to calculate primary production", 55)
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(face = "bold", size = 10)
  )

plot_file <- here::here("graphs/11_map_spatial_distribution_primary_production_parameters.pdf")

ggsave(
  plot_file,
  device = cairo_pdf,
  width = 7,
  height = 7
)

knitr::plot_crop(plot_file)
