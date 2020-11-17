# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Get insights on the depths where PAR reach 0.1 mol photons m-2
# day-1.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv(here::here("data/clean/propagated_hourly_par_water_column.csv"))

df

df <- df %>%
  group_by(station, deployement, depth_m) %>%
  summarise(
    daily_par = pracma::trapz(hour * 3600, hourly_par_z_umol_m2_s1) / 1e6,
    n = n()
  ) %>%
  assertr::verify(n == 24) %>%
  ungroup()

depth_m_0.1 <- df %>%
  group_by(station, deployement) %>%
  slice(which.min(abs(0.1 - daily_par))) %>%
  ungroup()

# Plot --------------------------------------------------------------------

p1 <- df %>%
  ggplot(aes(x = daily_par, y = depth_m, color = deployement)) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~station) +
  geom_hline(
    data = depth_m_0.1,
    aes(yintercept = depth_m),
    size = 0.25,
    lty = 2
  ) +
  labs(
    x = bquote("PAR" ~(mol~photons~m^{-2}~day^{-1})),
    y = "Depth (m)",
    title = bquote(bold("Depth at which PAR is 0.1"~mol~photons~m^{-2}~day^{-1})),
    subtitle = str_wrap(glue("The maximum depth where 0.1 is reached is {max(depth_m_0.1$depth_m)} m (at station {depth_m_0.1$station[which.max(depth_m_0.1$depth_m)]}). Based this graph, I think that calculating primary production over the first 50 meters is reasonable."), 120)
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    legend.position = "top"
  ) +
  paletteer::scale_color_paletteer_d(
    "ggthemes::wsj_rgby",
    guide = guide_legend(
      override.aes = list(size = 2),
      label.position = "top",
      keywidth = unit(4, "cm"),
      label.theme = element_text(size = 10, face = "bold")
    )
  )

p2 <- depth_m_0.1 %>%
  ggplot(aes(x = depth_m)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~deployement) +
  labs(
    x = "Depth (m)",
    y = "Count",
    title = bquote(bold("Histograms of depths at which PAR is 0.1"~mol~photons~m^{-2}~day^{-1}))
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

p <- p1 / p2 +
  plot_layout(heights = c(1, 0.5)) +
  plot_annotation(
    tag_levels = "A",
    title = str_wrap(
      "These graphs serve as helpers to determine the depth at which primary production should be integrated",
      60
    ),
    theme = theme(plot.title = element_text(
      size = 24,
      lineheight = 1.25,
      hjust = 0.5
    ))
  ) &
  theme(plot.tag = element_text(size = 20, face = "bold"))

ggsave(
  here::here("graphs/13_daily_par_in_water_column.pdf"),
  device = cairo_pdf,
  width = 10,
  height = 12
)
