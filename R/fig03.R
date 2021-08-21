# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Scatterplot of CP vs Chla.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

ctd <- read_csv(here::here("data/clean/ctd.csv")) %>%
  filter(depth_m <= 100)

hydroscat <- read_csv(here("data","clean","hydroscat.csv")) %>%
  filter(wavelength == 470) %>%
  rename(depth_m = depth) %>%
  filter(depth_m <= 100)

hydroscat %>%
  ggplot(aes(x = bbp)) +
  geom_histogram() +
  scale_x_log10()

hydroscat %>%
  arrange(bbp) %>%
  head(100) %>%
  rowid_to_column() %>%
  ggplot(aes(x = rowid, y = bbp)) +
  geom_point()

hydroscat <- hydroscat %>%
  filter(bbp >= 0.0002)

# Correlation between CP and fluorescence ---------------------------------

p1 <- ctd %>%
  ggplot(aes(x = flor_mg_m3, y = cp)) +
  geom_hex(bins = 50) +
  scale_y_log10() +
  scale_x_log10() +
  scale_fill_viridis_c() +
  annotation_logticks(size = 0.25) +
  labs(
    x = quote("Chla"~(mg~m^{-3})),
    y = quote(C[p]~(657)~(m^{-1}))
  ) +
  geom_smooth(method = "lm", color = "red", size = 1) +
  ggpubr::stat_regline_equation(label.y.npc = 1, size = 3) +
  ggpubr::stat_regline_equation(label.y.npc = 0.93, aes(label = ..rr.label..), size = 3) +
  theme(
    legend.position = "none",
    aspect.ratio = 1,
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

# chla vs bbp 470 nm ------------------------------------------------------

## Merge CTD and Hydroscat on the OWD and the closest depth ----

setDT(ctd)
setDT(hydroscat)

ctd[, ctd_depth := depth_m]

chl_bbp <- ctd[hydroscat, on = c("owd", "depth_m"), roll = "nearest"]

chl_bbp <- chl_bbp %>%
  as_tibble() %>%
  filter(abs(depth_m - ctd_depth) <= 1)

## Scatterplot chla vs bbp ----

p2 <- chl_bbp %>%
  ggplot(aes(x = flor_mg_m3, y = bbp)) +
  geom_hex(bins = 50) +
  scale_y_log10(labels = scales::label_number()) +
  scale_x_log10() +
  scale_fill_viridis_c() +
  annotation_logticks(size = 0.25) +
  labs(
    x = quote("Chla"~(mg~m^{-3})),
    y = quote(b[bp]~(470)~(m^{-1}))
  ) +
  geom_smooth(method = "lm", color = "red", size = 1) +
  ggpubr::stat_regline_equation(label.y.npc = 1, size = 3) +
  ggpubr::stat_regline_equation(label.y.npc = 0.93, aes(label = ..rr.label..), size = 3) +
  theme(
    legend.position = "none",
    aspect.ratio = 1,
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

# Scatterplot Cp vs bbp ---------------------------------------------------

p3 <- chl_bbp %>%
  ggplot(aes(x = cp, y = bbp)) +
  geom_hex(bins = 50) +
  scale_y_log10(labels = scales::label_number()) +
  scale_x_log10() +
  scale_fill_viridis_c() +
  annotation_logticks(size = 0.25) +
  labs(
    x = quote(C[p]~(657)~(m^{-1})),
    y = quote(b[bp]~(470)~(m^{-1}))
  ) +
  geom_smooth(method = "lm", color = "red", size = 1) +
  ggpubr::stat_regline_equation(label.y.npc = 1, size = 3) +
  ggpubr::stat_regline_equation(label.y.npc = 0.93, aes(label = ..rr.label..), size = 3) +
  theme(
    legend.position = "none",
    aspect.ratio = 1,
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

# Chla vs bbp/cp ----------------------------------------------------------

p4 <- chl_bbp %>%
  drop_na(bbp, cp, flor_mg_m3) %>%
  ggplot(aes(x = bbp / cp, y = flor_mg_m3)) +
  geom_hex(bins = 50) +
  scale_y_log10() +
  scale_x_log10() +
  scale_fill_viridis_c() +
  annotation_logticks(size = 0.25) +
  labs(
    x = quote(b[bp]~(470)/C[p]~(657)),
    y = quote("Chla"~(mg~m^{-3}))
  ) +
  geom_smooth(method = "lm", color = "red", size = 1) +
  ggpubr::stat_regline_equation(label.y.npc = 0.07, size = 3) +
  ggpubr::stat_regline_equation(label.y.npc = 0, aes(label = ..rr.label..), size = 3) +
  theme(
    legend.position = "none",
    aspect.ratio = 1,
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

# Combine and save the plots ----------------------------------------------

p <- p1 / p2 / p3 / p4 +
  plot_layout(ncol = 2) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(plot.tag = element_text(face = "bold"))

filename <- here::here("graphs","fig03.pdf")

ggsave(
  filename,
  device = cairo_pdf,
  width = 8,
  height = 8
)

knitr::plot_crop(filename)

# Calculate the correlation -----------------------------------------------

chl_cp_mean

set.seed(2021)

df_split <- initial_split(chl_cp_mean)
df_train <- training(df_split)
df_test <- testing(df_split)

lm_spec <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

lm_rec <- recipe(mean_cp ~ ., data = df_train) %>%
  update_role(owd, depth_m, new_role = "id") %>%
  step_log(mean_cp, mean_flor_mg_m3, base = 10)

lm_wf <- workflow() %>%
  add_model(lm_spec) %>%
  add_recipe(lm_rec)

df_folds <- vfold_cv(df_train)

# This make fit_resamples very long... Much faster if I do not use parallel...
# doParallel::registerDoParallel(10)

set.seed(1234)

lm_res <- fit_resamples(
  lm_wf,
  resamples = df_folds,
  control = control_resamples(save_pred = TRUE)
)

lm_res %>%
  collect_metrics()

lm_res %>%
  collect_predictions() %>%
  ggplot(aes(x = mean_cp, y = .pred)) +
  geom_point() +
  geom_abline(lty = 2, color = "red") +
  facet_wrap(~id)

res <- reg_intervals(
  mean_cp ~ mean_flor_mg_m3,
  data = lm_rec %>% prep() %>% juice(),
  type = "percentile",
  keep_reps = TRUE
)

res

res %>%
  unnest(.replicates) %>%
  ggplot(aes(x = estimate)) +
  geom_histogram()
