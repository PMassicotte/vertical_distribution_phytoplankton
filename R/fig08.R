# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Visualize the UVP data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/interpolate_fun.R")

uvp <- read_csv(here("data", "clean", "uvp_small_medium_large_class_size.csv"))

uvp

# Arrange particle size class (small -> large) ----------------------------

uvp <- uvp %>%
  mutate(
    particle_size_class_label = case_when(
      str_detect(particle_size_class, "small") ~ glue("Small particles ({particle_size_range})"),
      str_detect(particle_size_class, "medium") ~ glue("Medium particles ({particle_size_range})"),
      str_detect(particle_size_class, "large") ~ glue("Large particles ({particle_size_range})"),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(particle_size_class_label = factor(
    particle_size_class_label,
    levels = c(
      "Small particles (0.102-0.323 mm)",
      "Medium particles (0.323-1.02 mm)",
      "Large particles (1.02-26 mm)"
    )
  ))

uvp %>%
  distinct(particle_size_class, particle_size_class_label)

# Looks like there are measurements only every 5 meters along the water column.

uvp %>%
  distinct(depth_m)

uvp %>%
  distinct(station)

# Particle biovolume/concentration vs owd ---------------------------------

# I think I will keep observations that are within the isoume layer to calculate
# the average concentration.

uvp

mean(uvp$isolume_m_at_0_1_einm_2d_1, na.rm = TRUE)

df_viz <- uvp %>%
  # filter(between(depth_m, 0, 25)) %>%
  # filter(between(depth_m, 0, isolume_m_at_0_1_einm_2d_1)) %>%
  filter(between(depth_m, 0, mean(isolume_m_at_0_1_einm_2d_1, na.rm = TRUE))) %>%
  group_by(owd, particle_size_class_label) %>%
  summarise(across(c(count_per_liter, biovolume_ppm), ~ mean(., na.rm = TRUE)),
    n = n()
  ) %>%
  ungroup()

df_viz

df_viz %>%
  ggplot(aes(x = owd, y = count_per_liter)) +
  geom_point(size = 1, color = "#393E41") +
  facet_wrap(~particle_size_class_label, scales = "free_y", ncol = 1) +
  geom_smooth(color = "#bf1d28", size = 0.5) +
  labs(
    x = "Number of open water days (OWD)",
    y = "Particle count per liter"
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank()
  )

ggsave(here("graphs", "fig08.pdf"),
  device = cairo_pdf,
  width = 4,
  height = 5
)

# Autocorrelation lag -----------------------------------------------------

df_viz

df_acf <- df_viz %>%
  select(-n, -biovolume_ppm) %>%
  pivot_wider(
    names_from = particle_size_class_label,
    values_from = count_per_liter,
    names_repair = janitor::make_clean_names
  )

ccf(
  df_acf$small_particles_0_102_0_323_mm,
  df_acf$large_particles_1_02_26_mm,
  lag.max = 30,
  type = "correlation"
)

cor(df_acf$small_particles_0_102_0_323_mm, lag(df_acf$large_particles_1_02_26_mm, 20), use = "pair")

# # Autocorelation lag ------------------------------------------------------
#
# df_viz
#
# df_loess <- df_viz %>%
#   group_nest(particle_size_class_label) %>%
#   mutate(mod = map(data, ~ stats::loess(.$count_per_liter ~ .$owd))) %>%
#   mutate(predicted = map(mod, predict))
#
# # Looks like the calculated loess is the same as geom_smooth()
#
# df_loess %>%
#   unnest(c(data, predicted)) %>%
#   ggplot(aes(x = owd, y = count_per_liter)) +
#   geom_point() +
#   geom_line(aes(y = predicted), color = "red") +
#   facet_wrap(~particle_size_class_label, scales = "free_y", ncol = 1)
#
# ## Predict particle concentration between -30 to 30 OWD ----
#
# df_pred <- df_loess %>%
#   mutate(new_owd = list(seq(-30, 30, by = 1))) %>%
#   mutate(predicted_new_owd = map2(mod, new_owd, ~ predict(..1, newdata = ..2))) %>%
#   select(particle_size_class_label, new_owd, predicted_new_owd) %>%
#   unnest(c(new_owd, predicted_new_owd))
#
# df_pred
#
# # Normalize the curve so we can focus on the shape
#
# df_pred <- df_pred %>%
#   group_by(particle_size_class_label) %>%
#   mutate(normalized_prediction = predicted_new_owd / pracma::trapz(new_owd, predicted_new_owd))
#
# df_pred %>%
#   ggplot(aes(x = new_owd, y = normalized_prediction, color = particle_size_class_label)) +
#   geom_line() +
#   scale_x_continuous(breaks = scales::breaks_pretty(n = 10))
#
# ## Autocorrelation ----
#
# df_ccf <- df_pred %>%
#   ungroup() %>%
#   select(-predicted_new_owd) %>%
#   pivot_wider(
#     names_from = particle_size_class_label,
#     values_from = normalized_prediction,
#     names_repair = janitor::make_clean_names
#   ) %>%
#   nest(everything()) %>%
#   mutate(ccf_mod = map(
#     data,
#     ~ ccf(
#       .$small_particles_0_102_0_323_mm,
#       .$medium_particles_0_323_1_02_mm,
#       plot = FALSE,
#       lag.max = 30,
#       type = "correlation"
#     )
#   )) %>%
#   mutate(ccf_tidied = map(ccf_mod, broom::tidy))
#
# df_ccf
#
# df_ccf %>%
#   unnest(ccf_tidied) %>%
#   ggplot(aes(x = lag, xend = lag, y = 0, yend = acf)) +
#   geom_segment() +
#   scale_x_continuous(breaks = scales::breaks_pretty(n = 10))

