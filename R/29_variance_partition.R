rm(list = ls())

# bbp data from the hydroscat ---------------------------------------------

hydroscat <- fread(here("data","clean","hydroscat.csv")) %>%
  select(
    station,
    transect,
    depth_m = depth,
    wavelength,
    bbp
  ) %>%
  filter(wavelength == 532) # Use 1 wl for now.

hydroscat

# Remove spectra if negative bbp values at any wavelengths
hydroscat <- hydroscat %>%
  group_by(station, depth_m) %>%
  filter(all(bbp > 0)) %>%
  ungroup() %>%
  setDT()

hydroscat

# cp from the ctd ---------------------------------------------------------

ctd <- fread(here("data","clean","ctd.csv")) %>%
  select(station, transect, depth_m, flor_mg_m3, cp)

ctd

# poc ---------------------------------------------------------------------

poc <- fread(here("data","clean","poc.csv")) %>%
  select(station, transect, depth_m, owd, poc_umol_l)

# Joining -----------------------------------------------------------------

# setkey(x = hydroscat, wavelength)

# Thank to data.table rolling join!
df <- ctd[poc, roll = "nearest", on = .(station, transect, depth_m), nomatch = 0]
df <- hydroscat[df, roll = Inf, on = .(station, transect, depth_m), nomatch = 0]

df <- df %>%
  as_tibble() %>%
  drop_na(cp, flor_mg_m3, poc_umol_l)

# Isolume data ------------------------------------------------------------

isolume <-
  read_csv(
    "https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/Randelhoff-et-al-2019_GreenEdge_per-station_v1.0.csv"
  ) %>%
  janitor::clean_names() %>%
  select(station, owd, isolume_m_at_0_1_einm_2d_1) %>%
  pivot_longer(starts_with("isolume"),
    names_to = "isolume",
    values_to = "isolume_depth_m"
  )

isolume

df %>%
  anti_join(isolume, by = c("station", "owd"))

df <- df %>%
  inner_join(isolume, by = c("station", "owd"))

# Classify observations ---------------------------------------------------

df <- df %>%
  mutate(
    above_isolume = case_when(
      depth_m < isolume_depth_m ~ "Above the 0.1 isolume",
      TRUE ~ "Below the 0.1 isolume"
    )
  ) %>%
  mutate(ice_covered = case_when(
    owd < 0 ~ "Ice covered",
    TRUE ~ "Open water"
  ))

df %>%
  count(station, wavelength, above_isolume, ice_covered)

# Scatterplot -------------------------------------------------------------

p1 <- df %>%
  distinct(flor_mg_m3, poc_umol_l, .keep_all = TRUE) %>%
  ggplot(aes(x = flor_mg_m3, y = poc_umol_l)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm")

p2 <- df %>%
  distinct(cp, poc_umol_l, .keep_all = TRUE) %>%
  ggplot(aes(x = cp, y = poc_umol_l)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm")

p3 <- df %>%
  distinct(cp, poc_umol_l, .keep_all = TRUE) %>%
  ggplot(aes(x = bbp, y = poc_umol_l)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm")

p1 + p2 + p3

# Multiple regression -----------------------------------------------------

# TODO: Attention to the VIF...

df <- df %>%
  group_nest(wavelength, above_isolume, ice_covered) %>%
  mutate(mod = map(data, ~ lm(
    log10(poc_umol_l) ~ log10(bbp) + log10(flor_mg_m3),
    data = .
  ))) %>%
  mutate(relative_importance = map(mod, relaimpo::calc.relimp))

df <- df %>%
  mutate(relative_importance_tidy = map(relative_importance, function(x) {
    x@lmg %>%
      enframe(name = "predictor", value = "r2")
  }))

# Calculate the VIF -------------------------------------------------------

df <- df %>%
  rowid_to_column("model_number") %>%
  mutate(vif = map(mod, car::vif)) %>%
  unnest(c(relative_importance_tidy, vif))

df %>%
  select(!where(is.list))

# Plot --------------------------------------------------------------------

p <- df %>%
  group_by(ice_covered, above_isolume) %>%
  mutate(total_r2 = sum(r2)) %>%
  mutate(above_isolume = glue("{above_isolume} (R2 = {round(total_r2, digits = 2)})")) %>%
  ungroup() %>%
  ggplot(aes(
    x = str_wrap(above_isolume, 20),
    y = r2,
    fill = predictor
  )) +
  geom_col() +
  # geom_text(aes(label = round(r2, digits = 2)), color = "white") +
  facet_wrap(~ice_covered, scales = "free_x") +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 8),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = NULL,
    y = quote(R^2),
    fill = NULL,
    title = "Modeling particulate organic carbon (POC)",
    subtitle = str_wrap(
      "These results show the variance partitioning of different predictors used to model POC. Each bar presents the contribution of each regressor to the total R2.",
      120
    )
  ) +
  paletteer::scale_fill_paletteer_d("ggthemes::wsj_colors6") +
  theme(
    legend.position = "top",
    plot.subtitle = element_text(size = 8, lineheight = 1.5)
  )

ggsave(
  here("graphs","29_variance_partitioning_poc.pdf"),
  device = cairo_pdf,
  height = 5,
  width = 8
)
