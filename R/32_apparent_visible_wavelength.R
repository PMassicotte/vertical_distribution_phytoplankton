# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Calculate the apparent visible wavelength.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- vroom::vroom("data/clean/phytoplankton_absorption.csv")

df

# Normalize absorption spectra --------------------------------------------

df <- df %>%
  filter(between(wavelength, 400, 700)) %>%
  group_by(spectra_id) %>%
  mutate(
    aphy_normalized = aphy / pracma::trapz(wavelength, aphy),
    .after = aphy
  )

# Remove spectra with negative values

df <- df %>%
  group_by(spectra_id) %>%
  filter(all(aphy_normalized >= 0))

# Calculate the apparent visible wavelength -------------------------------

df_avw <- df %>%
  drop_na(aphy_normalized) %>%
  group_by(spectra_id, station, depth_m) %>%
  summarise(avw = sum(aphy_normalized) / sum(aphy_normalized / wavelength)) %>%
  ungroup()

df_avw

# Visualize spectra colored by AVW ----------------------------------------

df_avw %>%
  ggplot(aes(x = avw)) +
  geom_histogram(binwidth = 1)

df_avw %>%
  count(depth_m)

df_avw %>%
  filter(avw == min(avw) | avw == max(avw)) %>%
  inner_join(df, by = c("spectra_id", "station", "depth_m")) %>%
  ggplot(aes(x = wavelength, y = aphy_normalized, color = factor(avw))) +
  geom_line()

p <- df_avw %>%
  inner_join(df, by = c("spectra_id", "station", "depth_m")) %>%
  ggplot(aes(
    x = wavelength,
    y = aphy_normalized,
    group = spectra_id,
    color = avw
  )) +
  geom_line() +
  paletteer::scale_color_paletteer_c("grDevices::Spectral") +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[phi]~normalized~(m^{-1})),
    fill = "AVW (nm)"
  ) +
  facet_wrap(~station, scales = "free_y")

ggsave(
  here("graphs","32_apparent_visible_wavelength_per_spectra.pdf"),
  device = cairo_pdf,
  height = 6,
  width = 10
)

# Calculate the average AVW at each depth ---------------------------------

df_viz <- df_avw %>%
  group_by(depth_m) %>%
  summarise(
    mean_avw = mean(avw, na.rm = TRUE),
    sd_avw = sd(avw, na.rm = TRUE) / sqrt(n()),
    n = n()
  ) %>%
  filter(n > 1)

# Check where is the isolume, maybe this can explain why AVW increases after ~
# 50m

unique(df$isolume)
range(df$isolume_depth_m)
mean(df$isolume_depth_m)
median(df$isolume_depth_m)
sd(df$isolume_depth_m)

df_viz %>%
  ggplot(aes(x = mean_avw, y = depth_m)) +
  geom_pointrange(
    aes(xmin = mean_avw - sd_avw, xmax = mean_avw + sd_avw),
    shape = 20,
    fatten = 20,
    size = 0.25
  ) +
  scale_y_reverse() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
  labs(
    x = "Apparent visible wavelength (nm)",
    y = "Depth (m)"
  )

ggsave(
  here("graphs","32_average_apparent_visible_wavelength_by_depth.pdf"),
  device = cairo_pdf
)

# Check what is going on at depth > 75 meters where AVW decreases
df_avw %>%
  filter(depth_m >= 75) %>%
  inner_join(df) %>%
  ggplot(aes(
    x = wavelength,
    y = aphy_normalized,
    color = avw,
    group = spectra_id
  )) +
  geom_line() +
  paletteer::scale_color_paletteer_c("grDevices::Spectral") +
  facet_wrap(~depth_m)


# Only with the data above the isolume ------------------------------------

isolume <- df %>%
  ungroup() %>%
  distinct(spectra_id, above_isolume, ice_covered)

df_viz <- df_avw %>%
  inner_join(isolume) %>%
  group_by(depth_m, above_isolume) %>%
  summarise(
    mean_avw = mean(avw, na.rm = TRUE),
    sd_avw = sd(avw, na.rm = TRUE) / sqrt(n()),
    n = n()
  ) %>%
  ungroup() %>%
  filter(n > 1)

df_viz

df_viz <- df_viz %>%
  filter(str_detect(above_isolume, "Above"))

df_viz <- df_viz %>%
  group_nest() %>%
  mutate(mod = map(data, ~mgcv::gam(mean_avw ~ s(depth_m), data = .))) %>%
  mutate(pred = map(mod, predict)) %>%
  unnest(c(data, pred))

df_viz$mod[[1]]
summary(df_viz$mod[[1]])

df_viz %>%
  ggplot(aes(x = mean_avw, y = depth_m)) +
  geom_pointrange(
    aes(xmin = mean_avw - sd_avw, xmax = mean_avw + sd_avw),
    shape = 20,
    fatten = 20,
    size = 0.25
  ) +
  geom_line(aes(x = pred), color = "red") +
  # geom_smooth() +
  scale_y_reverse() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
  labs(
    x = "Apparent visible wavelength (nm)",
    y = "Depth (m)"
  )

ggsave(
  here("graphs","32_average_apparent_visible_wavelength_by_depth_only_above_isolume.pdf"),
  device = cairo_pdf
)
