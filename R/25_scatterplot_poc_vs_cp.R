# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Compare POC and CP
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

poc <- fread(here("data/clean/poc.csv")) %>%
  select(station, transect, depth_m, poc_umol_l)

ctd <- fread(here("data/clean/ctd.csv")) %>%
  select(station, transect, depth_m, owd, cp) %>%
  mutate(ctd_depth_m = depth_m)

# Thank to data.table rolling join!
df <- ctd[poc, roll = "nearest", on = .(station, transect, depth_m)]

df <- df %>%
  as_tibble() %>%
  mutate(depth_difference_m = abs(depth_m - ctd_depth_m))

# The matched depths are pretty close, I will keep all the data
df %>%
  ggplot(aes(x = depth_difference_m)) +
  geom_histogram(bins = 60)

# Isolume data ------------------------------------------------------------

isolume <-
  read_csv(
    "https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/Randelhoff-et-al-2019_GreenEdge_per-station_v1.0.csv"
  ) %>%
  janitor::clean_names() %>%
  select(station, owd, isolume_m_at_0_1_einm_2d_1) %>%
  pivot_longer(starts_with("isolume"),
    names_to = "isolume",
    values_to = "isolume_depth_m"
  ) %>%
  filter(isolume == "isolume_m_at_0_1_einm_2d_1")

df %>%
  anti_join(isolume)

df <- df %>%
  inner_join(isolume)

df <- df %>%
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

# Relationship between POC and CP -----------------------------------------

p <- df %>%
  ggplot(aes(x = cp, y = poc_umol_l)) +
  geom_point(color = "#393E41") +
  scale_y_log10() +
  scale_x_log10() +
  annotation_logticks() +
  labs(
    title = "Relationship between CP and POC",
    subtitle = "CP calculated from the CTD and POC data from the rosette.",
    x = bquote("Particle beam attenuation coefficient"~(m^{-1})),
    y = quote("POC"~(mg~m^{-3}))
  ) +
  geom_smooth(method = "lm", color = "#bf1d28", size = 0.5)

ggsave(
  here("graphs/25_scatterplot_poc_vs_cp.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 5
)

p <- p +
  facet_grid(ice_covered ~ above_isolume)

ggsave(
  here("graphs/25_scatterplot_poc_vs_cp_by_group.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 5
)
