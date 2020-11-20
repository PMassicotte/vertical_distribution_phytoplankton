# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Have a look to the distribution of PvsE parameters.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Use transmittance to get the type of deployment (Amundsen, Barge or Ice)
transmittance <- read_csv(here::here("data/clean/cops_kd_par_transmittance.csv")) %>%
  select(station, deployement)

pvse <-
  read_csv(
    "/media/4TB/work-ulaval/projects/green_edge/green_edge/data/pe-curves/photosynthetic_parameters_amundsen_2016.csv"
  ) %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100) %>%
  inner_join(transmittance, by = c("station")) %>%
  filter(str_starts(curve_id, "\\d+")) %>%
  select(station, transect, deployement, depth_m = depth, model_type, ps, alpha_b, pb_max, ek) %>%
  filter(model_type == "model1")

pvse

p <- pvse %>%
  ggplot(aes(x = pb_max)) +
  geom_histogram() +
  facet_wrap(~deployement) +
  scale_x_log10() +
  annotation_logticks(sides = "b", size = 0.25) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  here::here("graphs/19_histograms_pb_max.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 2
)
