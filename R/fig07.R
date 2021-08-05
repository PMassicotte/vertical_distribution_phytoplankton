# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Co-variability among fluorescence of chla, CP and bbp. At
# each OWD, calculate the depth of the maximum value (ex.: chla).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# CTD data ----------------------------------------------------------------

ctd <- read_csv(here::here("data", "clean", "ctd.csv"))

ctd

## Correct for CP offset ----

ctd <- ctd %>%
  group_by(station, cast) %>%
  # mutate(cp = cp - min(cp, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(depth_m <= 100)

# Hydroscat data (for bbp) ------------------------------------------------

hydroscat <-
  read_csv(here::here("data", "clean", "hydroscat.csv")) %>%
  rename(depth_m = depth) %>%
  filter(depth_m <= 100) %>%
  filter(wavelength == 620)

hydroscat

# Combine ctd and hydroscat data ------------------------------------------

df <- ctd %>%
  full_join(hydroscat, by = c("station", "transect", "depth_m", "owd"))

# Calculate the depth at which each variable has a maximum value ----------

df_viz <- df %>%
  select(owd, depth_m, flor_mg_m3, cp, bbp) %>%
  pivot_longer(c(flor_mg_m3, cp, bbp)) %>%
  drop_na() %>%
  group_by(owd, name) %>%
  filter(value == max(value, na.rm = TRUE)) %>%
  ungroup()

df_viz

# Plot --------------------------------------------------------------------

p <- df_viz %>%
  ggplot(aes(x = owd, y = depth_m, color = name)) +
  geom_point(
    size = 0.5,
    alpha = 0.5,
    show.legend = FALSE
  ) +
  scale_y_reverse() +
  geom_smooth(se = FALSE) +
  geom_smooth(aes(fill = name), key_glyph = "rect", show.legend = FALSE) +
  scale_fill_manual(
    breaks = c("bbp", "cp", "flor_mg_m3"),
    values = c("#e07a5f", "#3d405b", "#81b29a")
  ) +
  scale_color_manual(
    breaks = c("bbp", "cp", "flor_mg_m3"),
    values = c("#e07a5f", "#3d405b", "#81b29a"),
    labels = parse(text = c(
      "b[bp]~(620)", "Cp~(m^{-1})", "Chla~(mg~m^{-3})"
    )),
    guide = guide_legend(
      override.aes = list(alpha = 1, size = 2),
      label.hjust = 0.5,
      label.position = "top",
      keywidth = unit(3, "cm"),
      keyheight = unit(0.01, "cm")
    )
  ) +
  labs(
    x = "Number of open water days (OWD)",
    y = "Depth (m)"
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    legend.position = "top"
  )

ggsave(here("graphs","fig07.pdf"),
  device = cairo_pdf,
  width = 7.19,
  height = 5.22
)
