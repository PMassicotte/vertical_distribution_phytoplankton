# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Compare primary production rates measured in-situ and derived
# from PvsE parameters.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# In-situ primary production ----------------------------------------------

pp_insitu <-
  read_csv(here::here("data","raw","greenedge_primary_prod.csv"))

pp_insitu %>%
  count(mission, sample_type, sample_source, method)

pp_insitu <- pp_insitu %>%
  filter(mission == "amundsen_2016" & sample_source == "rosette")

pp_insitu <- pp_insitu %>%
  select(
    station,
    depth_m,
    date,
    cast,
    bottle,
    longitude,
    latitude,
    pp_mean_ugc_l_24h
  ) %>%
  drop_na(station) %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100, .after = station)

pp_insitu


# Pvse primary production -------------------------------------------------

pp_pvse <-
  read_csv(here::here(
    "data",
    "clean",
    "daily_primary_production_at_depth_from_pvse.csv"
  ))

pp_pvse

# Combine primary production measurements ---------------------------------

pp_insitu %>%
  count(station, cast)

pp_pvse %>%
  count(station, cast)

setDT(pp_insitu)
setDT(pp_pvse)

pp_pvse <- pp_pvse %>%
  mutate(depth2_m = depth_m)

# Matching on the closest depth
df_viz <- pp_pvse[pp_insitu,
  on = c("station", "transect", "cast", "depth_m"),
  roll = "nearest"
] %>%
  as_tibble()

df_viz

# Check the depth difference ----------------------------------------------

# Depths were matched on the closest distance, check the difference and remove
# matches that are too far in the water column.

df_viz <- df_viz %>%
  mutate(depth_difference = abs(depth2_m - depth_m)) %>%
  relocate(contains("depth"), .after = cast)

# Obviously, some matching have large depth differences. This is because I only
# calculated PP from PvsE at the 0.1 isolume and in-situ measurements were
# sometimes made much deeper in the water column.
df_viz %>%
  ggplot(aes(x = depth_difference)) +
  geom_histogram()

# Only keep observations where the depth difference is <= 1
df_viz <- df_viz %>%
  filter(depth_difference <= 1)

# Compare both pp estimations ---------------------------------------------

p <- df_viz %>%
  ggplot(aes(
    x = daily_primary_production_at_depth,
    y = pp_mean_ugc_l_24h,
    color = depth_m
  )) +
  geom_point(aes(shape = deployement), size = 2.5) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.25) +
  geom_smooth(method = "lm", size = 0.5, color = "#36558F", show.legend = FALSE) +
  geom_abline(size = 0.5, lty = 2, color = "#F0544F") +
  scale_color_viridis_c(
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(10, "cm"),
      barheight = unit(0.25, "cm"),
      order = 1
    ),
    breaks = scales::breaks_pretty(n = 10),
  ) +
  scale_shape_discrete(
    guide = guide_legend(
      override.aes = list(size = 3),
      order = 2
    )
  ) +
  # annotate(
  #   "text",
  #   x = 0.15,
  #   y = 10,
  #   label = str_wrap("At low PP, in-situ values are overestimated", 20),
  #   size = 2,
  #   family = "Exo"
  # ) +
  # annotate(
  #   "curve",
  #   x = 0.25,
  #   xend = 0.5,
  #   y = 10,
  #   yend = 7,
  #   size = 0.25,
  #   arrow = arrow(length = unit(0.25, "cm")),
  #   curvature = -0.2
  # ) +
  # annotate(
  #   "text",
  #   x = 110,
  #   y = 3,
  #   label = str_wrap("At high PP, in-situ values are underestimated", 20),
  #   size = 2,
  #   family = "Exo"
  # ) +
  # annotate(
  #   "curve",
  #   x = 90,
  #   xend = 70,
  #   y = 4.5,
  #   yend = 8,
  #   size = 0.25,
  #   arrow = arrow(length = unit(0.25, "cm")),
  #   curvature = 0.2
  # ) +
  labs(
    title = "Comparing primary production (in-situ vs PvsE)",
    x = bquote("Daily primary production (PvsE)" ~"["~mgC~m^{-3}~d^{-1}~"]"),
    y = bquote("Daily primary production (in situ)" ~"["~mgC~m^{-3}~d^{-1}~"]"),
    color = "Depth (m)",
    subtitle = "The <span style = 'color:#36558F;'>**blue line**</span> is a linear regression model whereas the <span style = 'color:#F0544F;'>**red line**</span> is the 1:1 line.",
    shape = NULL
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top",
    plot.subtitle = element_markdown(),
    legend.box = "vertical"
  )

ggsave(
  here::here("graphs","21_01_scatterplot_primary_production_insitu_vs_pvse.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 6
)

# Plot by type of deployment ----------------------------------------------

p2 <- p +
  facet_wrap(~deployement, scales = "free") +
  theme(
    strip.background = element_rect(fill = "#3c3c3c"),
    strip.text = element_text(color = "white", size = 14, face = "bold")
  )

ggsave(
  here::here("graphs","21_01_scatterplot_primary_production_insitu_vs_pvse_by_deployment.pdf"),
  device = cairo_pdf,
  width = 12,
  height = 6
)
