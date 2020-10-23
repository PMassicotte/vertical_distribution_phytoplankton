# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Calculate primary production from PvsE parameters derived from
# the incubation.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# PvsE --------------------------------------------------------------------

pvse <- read_csv("/media/4TB/work-ulaval/projects/green_edge/green_edge/data/pe-curves/photosynthetic_parameters_amundsen_2016.csv")

glimpse(pvse)

pvse <- pvse %>%
  filter(str_starts(station, "G")) %>%
  mutate(station = parse_number(station)) %>%
  rename(depth_m = depth)

pvse %>%
  ggplot(aes(x = r2)) +
  geom_histogram(binwidth = 0.1)

pvse <- pvse %>%
  filter(r2 >= 0.8)

station <- vroom::vroom(
  here::here("data/clean/ctd.csv"),
  altrep = TRUE,
  col_select = c(station, transect, longitude, latitude)
) %>%
  distinct()

pvse %>%
  anti_join(station, by = "station")

pvse <- pvse %>%
  inner_join(station, by = "station")


# COPS --------------------------------------------------------------------

cops <- vroom::vroom(here::here("data/raw/greenedge_cops.csv")) %>%
  filter(mission == "amundsen_2016")

# Convert planar PAR to scalar PAR ----------------------------------------

# See ref in the paper. The value of 1.2 was discussed with the co-authors.
