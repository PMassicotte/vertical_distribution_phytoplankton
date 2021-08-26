# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Relationship between POC and two classes of particle.
#
#
# In this study, the particulate organic carbon (POC) is divided
# into small POC (sPOC) and big POC (bPOC), with a nominal cutoff at 100 μm
# (Table 1; section 2.2.2).
#
# Galí et al., “Bridging the Gaps between Particulate Backscattering
# Measurements and Modeled Particulate Organic Carbon in the Ocean.”
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz_colors.R"))

# Prepare POC -------------------------------------------------------------

poc <- read_csv(here::here("data","clean","poc.csv"))  %>%
  mutate(poc_mg_m3 = ((poc_umol_l / 0.000001) * 12) / 1000000)

poc %>%
  count(station, transect, owd, depth_m, sort = TRUE)

poc <- poc %>%
  group_by(station, transect, owd, depth_m) %>%
  summarise(poc_mg_m3 = mean(poc_mg_m3)) %>%
  ungroup()

# Prepare UVP -------------------------------------------------------------

uvp <- read_csv(here("data","clean","uvp_tidy.csv"))

unique(uvp$particle_size_min_mm)
unique(uvp$particle_size_max_mm)

uvp <- uvp %>%
  filter(particle_size_max_mm <= 2.05)

# A lot of 0, remove them because I think they are too small to be measured by
# the UVP and not real values.

uvp %>%
  ggplot(aes(x = count_per_liter)) +
  geom_histogram()

uvp <- uvp %>%
  filter(count_per_liter > 0)

unique(uvp$particle_size_range_mm)

uvp <- uvp %>%
  mutate(particle_size_class = ifelse(particle_size_max_mm <= 0.102, "small", "large"))

uvp %>%
  count(particle_size_class)

uvp <- uvp %>%
  group_by(station, transect, date, depth_m, particle_size_class) %>%
  summarise(total_count_per_liter = sum(count_per_liter)) %>%
  ungroup() %>%
  pivot_wider(names_from = particle_size_class, values_from = total_count_per_liter)

# Merge with UVP ----------------------------------------------------------

setDT(uvp)
setDT(poc)

uvp[, uvp_depth := depth_m]

df <- uvp[poc, on = c("station", "transect", "depth_m"), roll = "nearest"]

df <- df %>%
  as_tibble() %>%
  filter(abs(depth_m - uvp_depth) <= 5)

df

df %>%
  count(station, depth_m, sort = TRUE) %>%
  assertr::verify(n == 1)

# Pivot longer ------------------------------------------------------------

df <- df %>%
  pivot_longer(c(small, large),
    names_to = "particle_size_class",
    values_to = "total_count_per_liter"
  ) %>%
  mutate(
    particle_size_class =
      factor(particle_size_class,
        levels = c("small", "large")
      )
  )

df <- df %>%
  mutate(is_open_water = is_open_water(owd))

# Plot --------------------------------------------------------------------

df

mylabels <- c(
  "small" = "Small particles (0.064 - 0.102 mm)",
  "large" = "Large particles (0.102 - 2.05 mm)"
)

p <- df %>%
  ggplot(aes(x = poc_mg_m3, y = total_count_per_liter, color = is_open_water)) +
  geom_point(aes(size = depth_m)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(
    values = owd_colors,
    labels = c("Ice covered", "Open water"),
    guide = guide_legend(
      override.aes = list(alpha = 1, size = 3),
      title = element_blank(),
      title.position = "top",
      title.theme = element_text(size = 8, family = "Poppins"),
      label.theme = element_text(size = 10, family = "Poppins"),
      ncol = 1
    )
  ) +
  scale_size_continuous(
    breaks = scales::breaks_pretty(n = 6),
    guide = guide_legend(
      nrow = 1,
      title.position = "top",
      title = "Depth (m)",
      title.theme = element_text(
        size = 10,
        family = "Poppins",
        margin = margin(t = 8),
        hjust = 0.5),
      label.theme = element_text(size = 10, family = "Poppins")
    )
  ) +
  annotation_logticks(size = 0.25) +
  geom_smooth(method = "lm", color = lm_color, size = 1) +
  ggpubr::stat_regline_equation(
    label.y.npc = 1,
    aes(
      x = poc_mg_m3,
      y = total_count_per_liter,
    ),
    size = 4,
    inherit.aes = FALSE
  ) +
  ggpubr::stat_regline_equation(
    label.y.npc = 0.90,
    size = 4,
    inherit.aes = FALSE,
    aes(
      x = poc_mg_m3,
      y = total_count_per_liter,
      label = ..rr.label..
    )
  ) +
  facet_wrap(
    ~particle_size_class,
    ncol = 2,
    labeller = labeller(particle_size_class = mylabels)
  ) +
  labs(
    x = quote("POC"~(mg~m^{-3})),
    y = "Total particle count per liter"
  ) +
  theme(
    legend.position = "top",
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  here("graphs","fig08.pdf"),
  device = cairo_pdf,
  height = 6,
  width = 10
)
