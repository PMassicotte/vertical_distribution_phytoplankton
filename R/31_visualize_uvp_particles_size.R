# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Visualize the UVP data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","interpolate_fun.R"))

uvp <- read_csv(here("data","clean","uvp_small_medium_large_class_size.csv"))

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

# Basic visualization -----------------------------------------------------

df_viz <- uvp %>%
  pivot_longer(c(count_per_liter, biovolume_ppm))

p <- df_viz %>%
  ggplot(aes(x = particle_size_class_label, y = value, fill = name)) +
  geom_boxplot(size = 0.1, outlier.size = 0.5, key_glyph = "rect") +
  scale_y_log10(labels = scales::label_number()) +
  annotation_logticks(sides = "l", size = 0.25) +
  paletteer::scale_fill_paletteer_d(
    "ggthemes::wsj_colors6",
    guide = guide_legend(
      label.position = "top",
      keywidth = unit(3, "cm"),
      keyheight = unit(0.25, "cm"),
    )
  ) +
  # facet_wrap(~transect) +
  labs(
    x = NULL,
    title = "Particle biovolume and concentration (UVP)"
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    panel.border = element_blank()
  )

ggsave(
  here("graphs","31_boxplot_uvp_particle_size.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 5
)

# Particle biovolume/concentration vs owd ---------------------------------

uvp

df_viz <- uvp %>%
  filter(between(depth_m, 0, 20)) %>%
  group_by(owd, particle_size_class_label) %>%
  summarise(across(c(count_per_liter, biovolume_ppm), ~mean(., na.rm = TRUE)), n = n()) %>%
  ungroup()

df_viz

p <- df_viz %>%
  ggplot(aes(x = owd, y = count_per_liter)) +
  geom_point() +
  facet_wrap(~particle_size_class_label, scales = "free_y", ncol = 1) +
  geom_smooth() +
  labs(
    title = str_wrap("Average particle concentration for the first 20 meters", 30),
    x = "OWD",
    y = "Count per liter"
  ) +
  theme(
    panel.border = element_blank()
  )

ggsave(
  here("graphs","31_uvp_particle_concentration_averaged_20_meters_owd.pdf"),
  device = cairo_pdf,
  width = 5,
  height = 7
)

# Prepare data for the 3D plot --------------------------------------------

# Looks like we have to average
uvp %>%
  count(depth_m, owd, sort = TRUE)

df_viz <- uvp %>%
  filter(depth_m <= 250) %>%
  group_by(depth_m, owd, particle_size_class) %>%
  summarise(across(c(count_per_liter, biovolume_ppm), ~mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  pivot_longer(c(count_per_liter, biovolume_ppm), names_to = "particle_indicator")

df_viz

# Interpolation -----------------------------------------------------------

df_viz <- df_viz %>%
  group_nest(particle_indicator, particle_size_class) %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, value, h = 5)) %>%
  unnest(res) %>%
  rename(owd = x, depth_m = y, value = z) %>%
  select(-data) %>%
  mutate(value = ifelse(value < 0, 0, value))

df_viz

df_viz <- df_viz %>%
  pivot_wider(names_from = particle_indicator, values_from = value)

df_viz

# An idea on the min/max values to make the fill scales in the plots
df_viz %>%
  group_by(particle_size_class) %>%
  summarise(across(
    c(biovolume_ppm, count_per_liter),
    .fns = list(min = min, max = max),
    na.rm = TRUE
  ))

df_viz

# Plot function -----------------------------------------------------------

plot_uvp <- function(df, fraction_size, variable) {

  df_viz <- df %>%
    filter(particle_size_class == {{ fraction_size }}) %>%
    rename(variable := {{ variable }})

  max_val <- max(df_viz$variable, na.rm = TRUE) * 1.5

  p <- df_viz %>%
    ggplot(aes(x = owd, y = depth_m, z = variable, fill = variable)) +
    geom_isobands(color = NA, bins = 100) +
    paletteer::scale_fill_paletteer_c(
      "oompaBase::jetColors",
      trans = "sqrt",
      # oob = scales::squish,
      breaks = scales::breaks_pretty(n = 10),
      guide =
        guide_colorbar(
          barwidth = unit(8, "cm"),
          barheight = unit(0.2, "cm"),
          direction = "horizontal",
          title.position = "top",
          title.hjust = 0.5
        )
    ) +
    scale_y_reverse(expand = c(0, 0)) +
    scale_x_continuous(
      expand = expansion(mult = c(0.01, 0.05)),
      breaks = scales::breaks_pretty(n = 8)
    ) +
    # geom_line(data = isolume, size = 1, aes(x = owd, y = depth_m, color = isolume), inherit.aes = FALSE) +
    paletteer::scale_color_paletteer_d("wesanderson::GrandBudapest1") +
    labs(
      y = "Depth (m)",
      x = "OWD",
      fill = variable,
      title = variable,
      subtitle = glue("{fraction_size}")
    ) +
    theme(
      panel.grid = element_line(color = "gray60", size = 0.1),
      panel.background = element_rect(fill = NA),
      panel.ontop = TRUE,
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0, size = 10, face = "bold"),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 8)
    )

  invisible(p)

}

# Plot biovolume ----------------------------------------------------------

p1 <- plot_uvp(df_viz, "particle_class_small", "biovolume_ppm")
p2 <- plot_uvp(df_viz, "particle_class_medium", "biovolume_ppm")
p3 <- plot_uvp(df_viz, "particle_class_large", "biovolume_ppm")

# Plot particle counts ----------------------------------------------------

p4 <- plot_uvp(df_viz, "particle_class_small", "count_per_liter")
p5 <- plot_uvp(df_viz, "particle_class_medium", "count_per_liter")
p6 <- plot_uvp(df_viz, "particle_class_large", "count_per_liter")

# Combine plots -----------------------------------------------------------

p <- wrap_plots(list(p1, p2, p3, p4, p5, p6), ncol = 2, byrow = FALSE) +
  plot_annotation(
    title = "Visualization of particle size distribution (UVP)"
  )

ggsave(
  here("graphs","31_uvp_particle_size_class_vs_owd.pdf"),
  device = cairo_pdf,
  width = 12,
  height = 12
)

# Correlations ------------------------------------------------------------

df_viz <- uvp %>%
  # filter(between(depth_m, 0, 20)) %>%
  mutate(depth_bin = chop_equally(depth_m, groups = 5, lbl_intervals(raw = TRUE)))

p <- df_viz %>%
  ggplot(aes(x = biovolume_ppm, y = count_per_liter)) +
  # geom_hex(bins = 50) +
  # scale_fill_viridis_c() +
  geom_point(color = "#3c3c3c", size = 0.5) +
  facet_grid(glue("{depth_bin} meters") ~ particle_size_class_label, scales = "free") +
  scale_x_log10(labels = scales::label_number()) +
  scale_y_log10(labels = scales::label_number()) +
  annotation_logticks(sides = "bl") +
  geom_smooth(method = "lm", color = "#bf1d28", size = 0.5) +
  labs(
    x = "Biovolume (PPM)",
    y = "Count per liter"
  )

ggsave(
  here("graphs","31_uvp_concentration_vs_biovolume_per_depth_bin.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 10
)

# Autocorrelation lag -----------------------------------------------------

df_viz <- uvp %>%
  select(
    station,
    depth_m,
    particle_size_class_label,
    count_per_liter,
    biovolume_ppm
  ) %>%
  group_nest(station, particle_size_class_label) %>%
  mutate(ccf_mod = map(
    data,
    ~ ccf(
      log10(.$count_per_liter),
      log10(.$biovolume_ppm),
      plot = FALSE,
      lag.max = 50
    )
  )) %>%
  mutate(ccf_tidied = map(ccf_mod, broom::tidy))

df_viz

df_viz <- df_viz %>%
  select(-data, -ccf_mod) %>%
  unnest(ccf_tidied) %>%
  group_by(particle_size_class_label, lag) %>%
  summarise(mean_acf = mean(acf, na.rm = TRUE)) %>%
  ungroup()

df_viz

# The distances between measurements in the water column is always 5 meters.
unique(uvp$depth_m) %>% sort() %>% diff()

# When $h < 0$ (left side of plots in Fig. 2), $x$ leads $y$.
# When $h > 0$ (right side of plots in Fig. 2), $x$ lags $y$.

p <- df_viz %>%
  mutate(lag = lag * 5) %>%
  ggplot(aes(x = lag, y = 0)) +
  geom_segment(
    aes(xend = lag, yend = mean_acf),
    size = 1,
    lineend = "round",
    color = "#3c3c3c"
  ) +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  geom_vline(
    xintercept = 0,
    color = "#bf1d28",
    lty = 2,
    size = 0.5
  ) +
  facet_wrap(~particle_size_class_label, ncol = 1) +
  labs(
    x = "Distance lag (meters)",
    y = "Pearson's correlation"
  ) +
  theme(panel.border = element_blank())

ggsave(
  here("graphs","31_uvp_vertical_crosscorrelation.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 10
)

uvp
