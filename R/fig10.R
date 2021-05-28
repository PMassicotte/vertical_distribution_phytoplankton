# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Relations between pigments and particle size.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# UVP ---------------------------------------------------------------------

uvp <- read_csv(here("data","clean","uvp_small_medium_large_class_size.csv"))

uvp %>%
  count(station, transect, owd, depth_m)

uvp <- uvp %>%
  group_by(station, transect, owd, depth_m, particle_size_class) %>%
  summarise(count_per_liter = mean(count_per_liter, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = particle_size_class, values_from = count_per_liter) %>%
  rowwise() %>%
  mutate(total_particle = sum(c_across(contains("class")))) %>%
  select(-contains("class"))

# Note that there are lot more stations sampled by the UVP than, for example,
# pigments or hydroscat

uvp %>%
  count(station, transect, owd, depth_m) %>%
  assertr::verify(n == 1)

# Pigments ----------------------------------------------------------------

pigments <- read_csv(here("data","clean","pigments_grouped.csv"))

pigments <- pigments %>%
  # mutate(depth_uvp = 2.5 + (depth_m) %/% 5 * 5, .after = depth_m) %>%
  # group_by(station, transect, owd, depth_uvp, pigment_group) %>%
  # summarise(sum_conc_mg_m3 = mean(sum_conc_mg_m3, na.rm = TRUE)) %>%
  # ungroup() %>%
  pivot_wider(names_from = pigment_group, values_from = sum_conc_mg_m3)

pigments %>%
  count(station, transect, owd, depth_m) %>%
  assertr::verify(n == 1)

skimr::skim(pigments)

pigments <- pigments %>%
  select(-chlorophyllide_a)

# Backscattering ----------------------------------------------------------

hydroscat <- read_csv(here("data","clean","hydroscat.csv")) %>%
  # filter(depth <= 100) %>%
  filter(wavelength == 620) %>%
  select(station, transect, owd, depth_m = depth, bbp_620 = bbp)
  # mutate(depth_uvp = 2.5 + (depth_m) %/% 5 * 5, .after = depth_m) %>%
  # group_by(station, transect, owd, depth_uvp) %>%
  # summarise(bbp_620 = mean(bbp_620, na.rm = TRUE)) %>%
  # ungroup()

hydroscat %>%
  count(station, transect, owd, depth_m) %>%
  assertr::verify(n == 1)

# Absorption --------------------------------------------------------------

absorption <- read_csv(here("data","clean","phytoplankton_absorption.csv")) %>%
  # filter(depth_m <= 100) %>%
  filter(wavelength == 443) %>%
  select(station, owd, depth_m, aphy_443 = aphy, aphy_specific)
  # mutate(depth_uvp = 2.5 + (depth_m) %/% 5 * 5, .after = depth_m) %>%
  # group_by(station, owd, depth_uvp) %>%
  # summarise(across(contains("aphy"), ~mean(., na.rm = TRUE))) %>%
  # ungroup()

absorption %>%
  count(station, owd, depth_m) %>%
  assertr::verify(n == 1)

# POC ---------------------------------------------------------------------

poc <- read_csv(here("data","clean","poc.csv")) %>%
  select(station, transect, owd, depth_m, poc_umol_l) %>%
  # mutate(depth_uvp = 2.5 + (depth_m) %/% 5 * 5, .after = depth_m) %>%
  group_by(station, transect, owd, depth_m) %>%
  summarise(across(poc_umol_l, ~mean(., na.rm = TRUE))) %>%
  ungroup()

poc %>%
  count(station, transect, owd, depth_m, sort = TRUE) %>%
  assertr::verify(n == 1)

# Combine everything ------------------------------------------------------

# Ishhhh, not many observations in common...
# df <- uvp %>%
#   inner_join(pigments, by = c("station", "transect", "owd", "depth_m" = "depth_uvp")) %>%
#   inner_join(hydroscat, by = c("station", "transect", "owd", "depth_m" = "depth_uvp")) %>%
#   inner_join(absorption, by = c("station", "owd", "depth_m" = "depth_uvp")) %>%
#   inner_join(poc, by = c("station", "transect", "owd", "depth_m" = "depth_uvp"))

absorption <- setDT(absorption)
hydroscat <- setDT(hydroscat)
pigments <- setDT(pigments)
poc <- setDT(poc)
uvp <- setDT(uvp)

df <- absorption[hydroscat, roll = "nearest", on = .(station, owd, depth_m)] %>%
  pigments[., roll = "nearest", on = .(station, owd, transect, depth_m)] %>%
  poc[., roll = "nearest", on = .(station, owd, transect, depth_m)] %>%
  uvp[., roll = "nearest", on = .(station, owd, transect, depth_m)]

df <- df %>%
  as_tibble() %>%
  filter(depth_m <= 100)

# Create classification model ---------------------------------------------

df_owd <- df %>%
  mutate(owd_class = case_when(
    between(owd, -30, -11) ~ "pre_bloom",
    between(owd, -10, 9) ~ "bloom",
    between(owd, 10, 40) ~ "post_bloom",
    TRUE ~ NA_character_
  ))

# Pretty much balanced
df_owd %>%
  count(owd_class)

set.seed(2021)

owd_split <- initial_split(df_owd, strata = owd_class)
owd_train <- training(owd_split)
owd_test <- testing(owd_split)

owd_rec <- recipe(owd_class ~ ., data = owd_train) %>%
  update_role(station, transect, owd, depth_m, longitude, latitude, new_role = "id") %>%
  step_corr(all_predictors(), threshold = 0.9) %>%
  step_impute_knn(all_predictors()) %>%
  step_zv(all_predictors())
# %>%
#   step_normalize(all_predictors())

owd_prep <- prep(owd_rec)

rf_spec <- rand_forest(trees = 100) %>%
  set_mode("classification") %>%
  set_engine("ranger")

owd_wf <- workflow() %>%
  add_recipe(owd_rec) %>%
  add_model(rf_spec)

owd_wf

set.seed(2021)

owd_cv <- vfold_cv(owd_train, strata = owd_class)

doParallel::registerDoParallel()

owd_res <- fit_resamples(
  owd_wf,
  resamples = owd_cv,
  control = control_resamples(save_pred = TRUE)
)

owd_res

# Explore the results -----------------------------------------------------

owd_res %>%
  collect_metrics()

owd_res %>%
  collect_predictions() %>%
  conf_mat(owd_class, .pred_class)

owd_res %>%
  collect_predictions() %>%
  ppv(owd_class, .pred_class)

rf_spec %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(
    owd_class ~ .,
    data = juice(owd_prep) %>% select(-c(station:depth_m), -longitude, -latitude)
  ) %>%
  vip() +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  here("graphs","fig10.pdf"),
  device = cairo_pdf,
  width = 7.15,
  height = 5.21
)

owd_pred <- owd_res %>%
  collect_predictions() %>%
  mutate(correct = owd_class == .pred_class) %>%
  left_join(
    owd_train %>% rowid_to_column(var = ".row"),
    by = c(".row", "owd_class")
  )

owd_pred

# owd_pred %>%
#   distinct(station, .keep_all = TRUE) %>%
#   ggplot(aes(x = longitude, y = latitude)) +
#   stat_summary_hex(aes(z = as.integer(correct)), fun = "mean")

final_fit <- last_fit(owd_wf, owd_split)

final_fit %>%
  collect_metrics()

final_fit %>%
  pluck(".workflow")

