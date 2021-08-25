# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Relationship between bbp/cp and particle size distribution from
# the UVP.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# uvp <- read_csv(here("data", "clean", "uvp_small_medium_large_class_size.csv"))
uvp <- read_csv(here("data/clean/uvp_tidy.csv")) %>%
  filter(particle_size_max_mm <= 2.05)

unique(uvp$particle_size_range)

# Make sure there is only 1 observation per group
uvp %>%
  dtplyr::lazy_dt() %>%
  count(station, transect, depth_m, particle_size_range) %>%
  as_tibble() %>%
  assertr::verify(n == 1)

# Calculate to total amount of particle (small + medium + large)
uvp <- uvp %>%
  dtplyr::lazy_dt() %>%
  group_by(station, transect, depth_m) %>%
  summarise(across(c(biovolume_ppm, count_per_liter), sum, na.rm = TRUE)) %>%
  as_tibble()

setDT(uvp)

ctd <- fread(here("data", "clean", "ctd.csv"))
hydroscat <- fread(here("data", "clean", "hydroscat.csv"))
hydroscat <- setnames(hydroscat, "depth", "depth_m")

hydroscat <- hydroscat[wavelength == 470]

uvp
ctd
hydroscat

ctd <- ctd[, ctd_depth_m := depth_m]
hydroscat <- hydroscat[, hydroscat_depth_m := depth_m]

# Merge data on the closest depth -----------------------------------------

df <- ctd[uvp, roll = "nearest", on = .(station, transect, depth_m)]
df <- hydroscat[df, roll = "nearest", on = .(station, transect, owd, depth_m)]

df <- df %>%
  as_tibble()

# Check if the depths are not too different -------------------------------

df

df %>%
  mutate(depth_diff_hydroscat = abs(depth_m - hydroscat_depth_m)) %>%
  mutate(depth_diff_ctd = abs(depth_m - ctd_depth_m)) %>%
  select(station, depth_m, contains("depth_diff")) %>%
  pivot_longer(contains("depth_diff")) %>%
  filter(value > 0) %>% # Remove perfect match (0 m depth difference)
  ggplot(aes(x = value)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~name)

# Looks like there are some important depth difference. Gonna use a maximum
# depth difference of 5 meters.

df <- df %>%
  mutate(depth_diff_hydroscat = abs(depth_m - hydroscat_depth_m)) %>%
  mutate(depth_diff_ctd = abs(depth_m - ctd_depth_m)) %>%
  filter(if_all(contains("depth_diff"), ~. <= 5))

range(df$depth_m)

df %>%
  count(owd, sort = TRUE)

# Plot --------------------------------------------------------------------

p <- df %>%
  # filter(depth_m <= 100) %>%
  mutate(bbp_cp = bbp / cp) %>%
  drop_na(bbp_cp) %>%
  ggplot(aes(
    x = bbp_cp,
    y = count_per_liter,
    color = owd <= 0,
    size = flor_mg_m3
  )) +
  geom_point(alpha = 0.5, stroke = 0) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(size = 0.1) +
  geom_smooth(method = "lm", show.legend = FALSE) +
  ggpubr::stat_regline_equation(
    label.y.npc = 0,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    size = 3
  ) +
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
  )+
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
    x = quote(b[bp]~(470)/C[p]~(657)),
    y = "Total particle count per liter"
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    # legend.title = element_blank(),
    legend.position = "top"
  )

ggsave(
  here("graphs","fig09.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 5
)
