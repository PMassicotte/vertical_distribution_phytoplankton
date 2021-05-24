rm(list = ls())

uvp <- fread(here("data", "clean", "uvp_small_medium_large_class_size.csv"))

uvp <- uvp[, lapply(.SD, mean, na.rm = TRUE),
  .SDcols = c("biovolume_ppm", "count_per_liter"),
  by = list(station, transect, owd, depth_m, particle_size_class)
]

uvp <- uvp[, lapply(.SD, sum, na.rm = TRUE),
  .SDcols = c("biovolume_ppm", "count_per_liter"),
  by = list(station, transect, owd, depth_m)
]

ctd <- fread(here("data", "clean", "ctd.csv"))
avw <- fread(here("data", "clean", "apparent_visible_wavelength.csv"))
hydroscat <- fread(here("data", "clean", "hydroscat.csv"))
hydroscat <- setnames(hydroscat, "depth", "depth_m")

hydroscat <- hydroscat[wavelength == 532]

uvp
ctd
hydroscat
avw

ctd <- ctd[, ctd_depth_m := depth_m]
hydroscat <- hydroscat[, hydroscat_depth_m := depth_m]

df <- ctd[uvp, roll = "nearest", on = .(station, transect, owd, depth_m)]
df <- hydroscat[df, roll = "nearest", on = .(station, transect, owd, depth_m)]
df_viz <- avw[df, roll = "nearest", on = .(station, depth_m)]

# Plot --------------------------------------------------------------------

p <- df_viz %>%
  as_tibble() %>%
  mutate(bbp_cp = bbp / cp) %>%
  drop_na(bbp_cp) %>%
  ggplot(aes(x = bbp_cp, y = count_per_liter, color = owd <= 0, size = flor_mg_m3)) +
  geom_point(alpha = 0.5, stroke = 0) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(size = 0.1) +
  geom_smooth(method = "lm", show.legend = FALSE) +
  scale_color_manual(
    breaks = c("TRUE", "FALSE"),
    values = c("#bb3e03", "#023047"),
    labels = c("Ice covered", "Open water"),
    guide = guide_legend(
      override.aes = list(alpha = 1, size = 3),
      title = element_blank(),
      title.position = "top",
      title.theme = element_text(size = 8, family = "Poppins"),
      ncol = 1
    )
  ) +
  scale_size_continuous(
    breaks = scales::breaks_pretty(n = 6),
    guide = guide_legend(
      nrow = 1,
      title.position = "top",
      title = quote("Chlorophyll-a"~(mg~m^{-3})),
      title.theme = element_text(
        size = 8,
        family = "Poppins",
        margin = margin(t = 8),
        hjust = 0.5)
    )
  ) +
  labs(
    x = quote(b[bp]/C[p]),
    y = "Total particle count per liter"
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    # legend.title = element_blank(),
    legend.position = "top"
  )

ggsave(
  here("graphs/fig09.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 5
)
