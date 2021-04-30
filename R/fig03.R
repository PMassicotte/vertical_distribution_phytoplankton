# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Scatterplot of CP vs Chla.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Correlation between CP and fluorescence ---------------------------------

ctd <- vroom::vroom(here::here("data/clean/ctd.csv"), altrep = TRUE) %>%
  filter(depth_m <= 100)

# Summarize by depth and open water day -----------------------------------

df_mean <- ctd %>%
  dtplyr::lazy_dt() %>%
  group_by(owd, depth_m) %>%
  summarise(across(c(flor_mg_m3, cp), ~mean(., na.rm = TRUE), .names = "mean_{.col}")) %>%
  as_tibble() %>%
  drop_na()

p <- ctd %>%
  ggplot(aes(x = flor_mg_m3, y = cp)) +
  geom_hex(bins = 50) +
  scale_y_log10() +
  scale_x_log10() +
  scale_fill_viridis_c() +
  annotation_logticks() +
  labs(
    x = bquote("Fluorescence"~(mg~m^{-3})),
    y = bquote("Particle beam attenuation coefficient"~(m^{-1}))
  ) +
  geom_smooth(method = "lm", color = "red", size = 1) +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  )

ggsave(
  here::here("graphs","fig03.pdf"),
  device = cairo_pdf,
  width = 5,
  height = 5
)
