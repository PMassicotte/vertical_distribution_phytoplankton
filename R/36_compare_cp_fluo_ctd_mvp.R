# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Compare CP calculated from the CTD and the MVP to see if values
# match.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

ctd <- read_csv(here("data","clean","ctd.csv")) %>%
  drop_na(cp) %>%
  dtplyr::lazy_dt() %>%
  group_by(owd, depth_m) %>%
  summarise(across(c(cp, flor_mg_m3), ~mean(., na.rm  = TRUE), .names = "{.col}_ctd")) %>%
  as_tibble()

mvp <- read_csv(here("data","clean","mvp_with_owd.csv")) %>%
  drop_na(cp) %>%
  rename(depth_m = pres) %>%
  dtplyr::lazy_dt() %>%
  group_by(owd, depth_m) %>%
  summarise(across(c(cp, fluo), ~mean(., na.rm  = TRUE), .names = "{.col}_mvp")) %>%
  as_tibble()

setDT(ctd)
setDT(mvp)

ctd[, depth_ctd := depth_m]

df <- ctd[mvp, on = c("owd", "depth_m"), roll = "nearest"] %>%
  as_tibble() %>%
  filter(abs(depth_m - depth_ctd) <= 1)

df

# CP ----------------------------------------------------------------------

p1 <- df %>%
  # filter(between(depth_m, 200, 400)) %>%
  filter(cp_mvp >= 0.0001) %>%
  ggplot(aes(x = cp_ctd, y = cp_mvp)) +
  # geom_point() +
  geom_hex(bins = 75) +
  geom_abline(color = "red", lty = 2) +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::label_number()) +
  scale_y_log10(labels = scales::label_number()) +
  scale_fill_viridis_c() +
  coord_equal()  +
  theme(
    legend.position = "none"
  )

# Chla fluorescence -------------------------------------------------------

p2 <- df %>%
  # filter(between(depth_m, 200, 400)) %>%
  # filter(cp_mvp >= 0.0001) %>%
  filter(if_all(c(flor_mg_m3_ctd, fluo_mvp), ~ . > 0)) %>%
  ggplot(aes(x = flor_mg_m3_ctd, y = fluo_mvp)) +
  # geom_point() +
  geom_hex(bins = 75) +
  geom_abline(color = "red", lty = 2) +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::label_number()) +
  scale_y_log10(labels = scales::label_number()) +
  scale_fill_viridis_c() +
  coord_equal() +
  theme(
    legend.position = "none"
  )

# Combine plots -----------------------------------------------------------

p <- p1 / p2 +
  plot_layout(ncol = 2)

ggsave(
  here("graphs","36_scatterplots_cp_flor_ctd_vs_mvp.pdf"),
  device = cairo_pdf(),
  width = 8,
  height = 4
)

# Correlation between ctd and mvp data ------------------------------------

df %>%
  select(owd, depth_m, cp_ctd, cp_mvp, flor_mg_m3_ctd, fluo_mvp) %>%
  filter(if_all(c(cp_ctd, cp_mvp, flor_mg_m3_ctd, fluo_mvp), ~ . > 0)) %>%
  mutate(across(-c(owd, depth_m), log10)) %>%
  select(-owd, -depth_m) %>%
  corrr::correlate() %>%
  corrr::focus(c(cp_ctd, flor_mg_m3_ctd))

