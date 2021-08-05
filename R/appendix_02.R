# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  How change the distribution of particle size with OWD.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

uvp <- read_csv(here("data","clean","uvp_small_medium_large_class_size.csv"))

uvp <- uvp %>%
  select(station, owd, depth_m, particle_size_class, count_per_liter) %>%
  group_by(station, owd, depth_m, particle_size_class) %>%
  summarise(count_per_liter = mean(count_per_liter)) %>%
  ungroup()

uvp

uvp_wide <- uvp %>%
  pivot_wider(names_from = particle_size_class, values_from = count_per_liter)

uvp_wide

uvp_wide <- uvp_wide %>%
  mutate(particle_class_small_to_large = particle_class_small / particle_class_large) %>%
  mutate(particle_class_small_to_medium = particle_class_small / particle_class_medium)

uvp_wide

df_viz <- uvp_wide %>%
  filter(depth_m <= 100) %>%
  group_by(owd) %>%
  summarise(across(contains("_to_"), .fns = list(mean = mean))) %>%
  pivot_longer(-owd)

df_viz

# Plot --------------------------------------------------------------------

lab <- c(
  "particle_class_small_to_medium_mean" = "Particle count small/medium ratio",
  "particle_class_small_to_large_mean" = "Particle count small/large ratio"
)

p <- df_viz %>%
  ggplot(aes(x = owd, y = value)) +
  geom_point(color = "#393E41") +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 5)) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
  geom_smooth(
    color = "#bf1d28",
    size = 0.5,
    method = "gam"
  ) +
  facet_wrap(
    ~name,
    scales = "free_y",
    ncol = 1,
    labeller = labeller(name = lab)
  ) +
  labs(
    x = "Number of open water days (OWD)",
    y = "Particle count ratio"
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  here("graphs/fig08.pdf"),
  device = cairo_pdf,
  width = 5,
  height = 6
)

# See if there is relation with bbp/cp ratio ------------------------------

## CTD data ----

ctd <- read_csv(here::here("data", "clean", "ctd.csv")) %>%
  select(station, owd, depth_m, flor_mg_m3, cp) %>%
  filter(depth_m <= 100) %>%
  drop_na() %>%
  arrange(station, depth_m) %>%
  mutate(ctd_depth_m = depth_m)

ctd

ctd %>%
  dtplyr::lazy_dt() %>%
  count(station, owd, depth_m) %>%
  as_tibble() %>%
  assertr::verify(n == 1)

## Hydroscat data (for bbp) ----

hydroscat <- read_csv(here::here("data", "clean", "hydroscat.csv")) %>%
  rename(depth_m = depth) %>%
  filter(depth_m <= 100) %>%
  filter(wavelength == 620) %>%
  select(station, owd, depth_m, bbp)

hydroscat

hydroscat %>%
  count(station, depth_m) %>%
  assertr::verify(n == 1)

## Combine ctd and hydroscat data ----

hydroscat <- setDT(hydroscat)
ctd <- setDT(ctd)

bbp_cp <- ctd[hydroscat, roll = "nearest", on = .(station, owd, depth_m)]

bbp_cp <- bbp_cp %>%
  mutate(bbp_cp = bbp / cp) %>%
  as_tibble() %>%
  filter(abs(depth_m - ctd_depth_m) <= 0.5) %>%
  select(-ctd_depth_m)

bbp_cp

bbp_cp %>%
  count(station, owd, depth_m) %>%
  assertr::verify(n == 1)

### bbp vs cp ----

p <- bbp_cp %>%
  ggplot(aes(x = cp, y = bbp)) +
  geom_hex(
    bins = 50,
    fill = "#393E41",
    color = "white",
    size = 0.1
  ) +
  scale_x_log10(
    labels = scales::label_number(),
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  scale_y_log10(
    labels = scales::label_number(),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  annotation_logticks(size = 0.25) +
  geom_smooth(
    color = "#bf1d28",
    size = 0.5,
    method = "lm"
  ) +
  labs(
    x = quote(C[p~(657)]~(m^{-1})),
    y = quote(b[bp~(620)]~(m^{-1}))
  ) +
  theme(
    aspect.ratio = 1,
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

filename <- here("graphs","appendix02.pdf")

ggsave(
  filename,
  device = cairo_pdf,
  width = 6,
  height = 6
)

knitr::plot_crop(filename)

### Is there a trend of bbp/cp ratio over OWD? ----

bbp_cp

bbp_cp %>%
  ggplot(aes(x = owd, y = bbp_cp)) +
  geom_point()

bbp_cp %>%
  filter(depth_m <= 100) %>%
  group_by(owd) %>%
  summarise(cp = mean(cp)) %>%
  ggplot(aes(x = owd, y = cp)) +
  geom_point() +
  geom_smooth()

## Combine bbp/cp with uvp ----

bbp_cp
uvp_wide

bbp_cp <- setDT(bbp_cp)
uvp_wide <- setDT(uvp_wide)

df <- uvp_wide[bbp_cp, roll = "nearest", on = .(station, owd, depth_m)]

df %>%
  as_tibble() %>%
  count(station, depth_m) %>%
  assertr::verify(n == 1)

df <- df %>%
  as_tibble()

## Visualization with bbp_cp ----

df

df %>%
  count(station, owd, depth_m) %>%
  assertr::verify(n == 1)

df %>%
  ggplot(aes(x = particle_class_small, y = bbp_cp)) +
  # geom_point() +
  geom_hex(bins = 50) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "loess")

df %>%
  filter(particle_class_small_to_large < 1e3) %>%
  ggplot(aes(x = particle_class_small_to_large, y = bbp_cp)) +
  # geom_point() +
  geom_hex(bins = 50) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm")

# Anything with the pigments? ---------------------------------------------

pigments <- read_csv(here("data","clean","pigments_grouped.csv")) %>%
  # pivot_wider(names_from = pigment_group, values_from = sum_conc_mg_m3) %>%
  select(-longitude, -latitude, -transect)

pigments

pigments <- setDT(pigments)
df <- setDT(df)

pigments <- df[pigments, roll = "nearest", on = .(station, owd, depth_m)]

pigments <- pigments %>%
  as_tibble()

# pigments %>%
#   count(station, depth_m) %>%
#   assertr::verify(n == 1)

pigments %>%
  count(station)

pigments %>%
  ggplot(aes(x = bbp_cp, y = sum_conc_mg_m3)) +
  geom_point(color = "#393E41") +
  geom_smooth(
    color = "#bf1d28",
    size = 0.5,
    method = "lm"
  ) +
  facet_wrap(~pigment_group) +
  scale_x_log10() +
  scale_y_log10()

pigments %>%
  pivot_wider(names_from = pigment_group, values_from = sum_conc_mg_m3) %>%
  select(
    particle_class_large:particle_class_small,
    flor_mg_m3:phaeophorbide_a
  ) %>%
  mutate(across(everything(), log10)) %>%
  GGally::ggpairs()

ggsave("~/Desktop/ggpairs.pdf", device = cairo_pdf, width = 18, height = 18)


pigments %>%
  pivot_wider(names_from = pigment_group, values_from = sum_conc_mg_m3) %>%
  select(particle_class_large:phaeophorbide_a) %>%
  mutate(across(everything(), log10)) %>%
  select(photosynthetic_pigments, particle_class_large:particle_class_small) %>%
  lm(photosynthetic_pigments ~ ., data = .) %>%
  relaimpo::calc.relimp() %>%
  plot()

# TODO: PCA

pigments %>%
  pivot_wider(names_from = pigment_group, values_from = sum_conc_mg_m3) %>%
  select(particle_class_large:phaeophorbide_a) %>%
  mutate(across(everything(), log10)) %>%
  # skimr::skim()
  select(-chlorophyllide_a, -contains("_to_"), -bbp_cp) %>%
  drop_na() %>%
  prcomp() %>%
  autoplot(
    loadings = TRUE,
    loadings.label = TRUE,
    loadings.label.repel = TRUE,
    label = FALSE
  )

ggsave(
  "~/Desktop/pca.pdf",
  device = cairo_pdf,
  width = 6,
  height = 6
)
