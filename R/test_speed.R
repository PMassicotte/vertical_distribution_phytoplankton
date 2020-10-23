ctd <- ctd %>%
  group_by(station, cast, date_time, longitude, latitude)

ctd

tic()
res <- ctd %>%
  mutate(across(
    c(flor_mg_m3, sigt_kg_m3),
    ~ zoo::rollmedian(.x, k = 11, fill = NA, align = "center"),
    .names = "{.col}_rollmedian"
  ))
toc()

tic()
res <- ctd %>%
  mutate(across(
    c(flor_mg_m3, sigt_kg_m3),
    ~ RcppRoll::roll_median(.x, n = 11, fill = NA, align = "center"),
    .names = "{.col}_rollmedian"
  ))
toc()
