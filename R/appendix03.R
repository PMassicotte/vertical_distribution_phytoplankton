# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Check if there are any relationships between chla/cp against Ek
# and Fv/Fm. These two last indices have been reported to be related to chla/cp.
#
# Behrenfeld, Michael J, and Emmanuel Boss. “The Beam Attenuation to Chlorophyll
# Ratio: An Optical Index of Phytoplankton Physiology in the Surface Ocean?"
# Deep Sea Research Part I: Oceanographic Research Papers 50, no. 12 (December
# 2003): 1537–49. https://doi.org/10.1016/j.dsr.2003.09.002.
#
# Xing, Xiaogang, Hervé Claustre, Julia Uitz, Alexandre Mignot, Antoine Poteau,
# and Haili Wang. “Seasonal Variations of Bio-Optical Properties and Their
# Interrelationships Observed by Bio-Argo Floats in the Subpolar North
# Atlantic." Journal of Geophysical Research: Oceans 119, no. 10 (October 2014):
# 7372–88. https://doi.org/10.1002/2014JC010189.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz_colors.R"))

# CTD ---------------------------------------------------------------------

ctd <- read_csv(here("data","clean","ctd.csv"))

# Chla/cp vs ek -----------------------------------------------------------

pvse <- read_csv(here("data", "raw", "photosynthetic_parameters_amundsen_2016.csv")) %>%
  select(station, depth_m = depth, ps:pb_max) %>%
  mutate(station = parse_number(station))

ctd
pvse

setDT(ctd)
setDT(pvse)

ctd[, depth_ctd := depth_m]

ek <- ctd[pvse, on = c("station", "depth_m"), roll = "nearest"]

ek <- ek %>%
  as_tibble() %>%
  filter(abs(depth_m - depth_ctd) <= 1) %>%
  drop_na(flor_mg_m3, cp, ek) %>%
  filter(depth_m <= 100) %>%
  mutate(chla_cp = flor_mg_m3 / cp) %>%
  mutate(is_open_water = is_open_water(owd))

p1 <- ek %>%
  ggplot(aes(x = chla_cp, y = ek)) +
  geom_point(aes(color = is_open_water)) +
  scale_color_owd() +
  geom_smooth(
    method = "lm",
    color = lm_color,
    size = 0.5
  ) +
  ggpubr::stat_regline_equation(label.y.npc = 1) +
  ggpubr::stat_regline_equation(label.y.npc = 0.93, aes(label = ..rr.label..)) +
  labs(
    x = quote(Chla/C[p] ~ (657) ~ (mg~m^{-2})),
    y = quote(E[k] ~ (mu*mol~m^{-2}~s^{-1}))
  ) +
  theme(
    panel.border = element_blank(),
    legend.background = element_blank(),
    legend.key = element_rect(fill = NA),
    axis.ticks = element_blank(),
    legend.justification = c(1, 1),
    legend.position = c(0.9, 0.9)
  )

# Fv/Fm vs ek -------------------------------------------------------------

pam <- read_csv(here("data", "raw", "GreenEdge_FvFm_all_station_data.csv")) %>%
  janitor::clean_names() %>%
  setDT()

ctd

fvfm <- ctd[pam, on = c("station", "depth_m"), roll = "nearest"]

fvfm <- fvfm %>%
  as_tibble() %>%
  filter(abs(depth_m - depth_ctd) <= 1) %>%
  drop_na(flor_mg_m3, cp, fv_fm_mean) %>%
  filter(depth_m <= 100) %>%
  mutate(chla_cp = flor_mg_m3 / cp) %>%
  mutate(is_open_water = is_open_water(owd))

p2 <- fvfm %>%
  ggplot(aes(x = chla_cp, y = fv_fm_mean)) +
  geom_point(aes(color = is_open_water)) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_color_owd() +
  geom_smooth(
    method = "lm",
    color = "#bf1d28",
    size = 0.5
  ) +
  ggpubr::stat_regline_equation(label.y.npc = 1) +
  ggpubr::stat_regline_equation(label.y.npc = 0.93, aes(label = ..rr.label..)) +
  labs(
    x = quote(Chla/C[p] ~ (657) ~ (mg~m^{-2})),
    y = quote(Fv/Fm)
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

# Combine plots -----------------------------------------------------------

p <- p1 / p2 +
  plot_annotation(
  tag_levels = "A"
) &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here("graphs","appendix03.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 8
)

# Correlation stats -------------------------------------------------------

cor(ek$chla_cp, ek$ek)
cor(fvfm$chla_cp, fvfm$fv_fm_mean)

# These are different indices, but they do not correlate to each other.
# Ek: photoacclimation
# Fv_Fm: physiology

