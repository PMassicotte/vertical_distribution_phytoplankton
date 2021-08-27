# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Correlation between CTD chla and HPLC pigments.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz_colors.R"))

# Pigments ----------------------------------------------------------------

pigments <- read_csv(here("data", "clean", "pigments_grouped.csv")) %>%
  pivot_wider(names_from = pigment_group, values_from = sum_conc_mg_m3) %>%
  select(-longitude, -latitude)

pigments

pigments %>%
  count(station, transect, depth_m) %>%
  assertr::verify(n == 1)

# CTD ---------------------------------------------------------------------

ctd <- read_csv(here("data", "clean", "ctd.csv")) %>%
  drop_na(flor_mg_m3)

ctd

# Merge pigments and CTD on the closest depth -----------------------------

setDT(pigments)
setDT(ctd)

ctd[, ctd_depth_m := depth_m]

df <-
  ctd[pigments, on = c("station", "transect", "owd", "depth_m"), roll = "nearest"] %>%
  as_tibble()

df

# The maximum depth difference is about 3 meters, I will not discard any data.
df %>%
  mutate(depth_diff = abs(depth_m - ctd_depth_m)) %>%
  ggplot(aes(x = depth_diff)) +
  geom_histogram()

# Tidy the pigment groups -------------------------------------------------

df <- df %>%
  pivot_longer(
    chlorophyllide_a:phaeophorbide_a,
    names_to = "pigment_group",
    values_to = "sum_conc_mg_m3"
  )

df

df %>%
  count(station, transect, depth_m) %>%
  assertr::verify(n == 6)

# Visualization -----------------------------------------------------------

df

unique(df$pigment_group)

my_labels <- c(
  "chlorophyllide_a" = "Chlorophyllide-a",
  "phaeophytin_a" = "Phaeophytin-a",
  "photoprotection" = "Photoprotection",
  "photosynthetic_pigments" = "Photosynthetic",
  "tchla" = "Total chlorophyll-a",
  "phaeophorbide_a" = "Phaeophorbide-a"
)

p <- df %>%
  mutate(is_open_water = is_open_water(owd)) %>%
  ggplot(aes(x = sum_conc_mg_m3, y = flor_mg_m3)) +
  geom_point(aes(color = is_open_water), size = 1) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(size = 0.1) +
  scale_color_owd(ncol = 2) +
  geom_smooth(method = "lm", color = lm_color, size = 0.5) +
  ggpubr::stat_regline_equation(
    label.y.npc = 1,
    size = 3,
    label.x.npc = 0.05,
  ) +
    ggpubr::stat_regline_equation(
      label.y.npc = 0.93,
      label.x.npc = 0.05,
      size = 3,
      aes(label = ..rr.label..)
  ) +
  facet_wrap(
    ~pigment_group,
    scales = "free_x",
    labeller = labeller(pigment_group = my_labels)
  ) +
  labs(
    x = quote("HPLC pigment concentration"~(mg~m^{-3})),
    y = quote("CTD chlorophyll-a fluorescence"~(mg~m^{-3})),
  ) +
  theme(
    legend.position = "top",
    legend.background = element_blank(),
    legend.key = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(size = 10)
  )

ggsave(
  here("graphs","fig05.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 6
)
