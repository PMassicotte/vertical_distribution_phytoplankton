# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Extract OWD days for the MVP localisations.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

library(furrr)

plan(multisession(workers = availableCores() - 1))

# MVP data ----------------------------------------------------------------

mvp <- read_csv(here("data","raw","greenedge_mvp.csv")) %>%
  mutate(initial_longitude_deg = -initial_longitude_deg) %>%
  group_by(initial_longitude_deg, initial_latitude_deg) %>%
  mutate(measurement_id = cur_group_id(), .before = 1) %>%
  ungroup()

mvp %>%
  distinct(measurement_id)

mvp_sf <- mvp %>%
  distinct(
    measurement_id,
    section_number,
    date,
    initial_longitude_deg,
    initial_latitude_deg
  ) %>%
  st_as_sf(
    coords = c("initial_longitude_deg", "initial_latitude_deg"),
    crs = 4326
  )

mvp_sf %>%
  ggplot() +
  geom_sf(aes(color = section_number))

# Sea ice concentration ---------------------------------------------------

latlon <-
  tidync::hyper_tibble(
    "../green_edge/data/ice_concentration/LongitudeLatitudeGrid_3.125km_Arctic.nc"
  ) %>%
  filter(between(latitude, 68, 71)) %>%
  filter(between(longitude, -65, -53))

sic_files <-
  fs::dir_ls(
    "../green_edge/data/ice_concentration/amundsen"
  )

extract_sic <- function(sic_file, mvp_sf, latlon) {

  # Attach lat/long with sic data
  sic <- tidync::hyper_tibble(sic_file) %>%
    inner_join(latlon, by = c("x", "y")) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  # Extract SIC
  res <- mvp_sf %>%
    st_join(sic, join = st_nearest_feature) %>%
    as_tibble() %>%
    select(-geometry, -x, -y, -time) %>%
    mutate(date_sic = str_extract(sic_file, "\\d{8}"), .after = date) %>%
    mutate(date_sic = anytime::anydate(date_sic, calcUnique = TRUE))

  return(res)
}

## Loop all SIC files ----

sic <- future_map_dfr(
    sic_files,
    extract_sic,
    mvp_sf = mvp_sf,
    latlon = latlon,
    .progress = TRUE
  )

sic

# Rolling window ----------------------------------------------------------

# Use a rolling windows to remove influence of noisy data

sic <- sic %>%
  group_by(measurement_id) %>%
  mutate(rolling_sic = RcppRoll::roll_mean(sea_ice_concentration,
    n = 14,
    fill = NA,
    align = "center"
  )) %>%
  ungroup()

# Visualize ---------------------------------------------------------------

set.seed(2021)

df_viz <- sic %>%
  group_nest(measurement_id) %>%
  sample_n(36) %>%
  unnest(data)

df_viz %>%
  ggplot(aes(x = date_sic, sea_ice_concentration, group = measurement_id)) +
  geom_line() +
  geom_line(aes(y = rolling_sic), color = "red") +
  geom_hline(yintercept = 15, color = "blue", lty = 2) +
  geom_vline(aes(xintercept = date), color = "green", lty = 2) +
  facet_wrap(~measurement_id, scales = "free_x")

write_csv(df_viz, here("data","clean","sic.csv"))

# Calculate the number of open water days (OWD) ---------------------------

sic

# 15% SIC
sic_threshold <- 15

res <- sic %>%
  group_nest(measurement_id) %>%
  mutate(date_sic_15_percent = map(data, function(df) {

    i <- df %>%
      mutate(sic_smaller_than_15_percent = rolling_sic <= sic_threshold) %$%
      rle(sic_smaller_than_15_percent) %>%
      do.call(data.frame, .) %>%
      as_tibble() %>%
      mutate(n = cumsum(lengths)) %>%
      mutate(nn = n - lengths) %>%
      # At least 3 days with SIC >= 15%
      filter(lengths >= 3 & values == TRUE) %>%
      pull(nn)

    return(df[i, ]$date_sic)

  })) %>%
  unnest(date_sic_15_percent) %>%
  unnest(data)

res

owd <- res %>%
  select(measurement_id, rolling_sic, date, date_sic, date_sic_15_percent) %>%
  drop_na() %>%
  filter(date_sic == date_sic_15_percent)

# Visualize to see if it makes sens

sic %>%
  semi_join(df_viz, by = "measurement_id") %>%
  ggplot(aes(x = date_sic, sea_ice_concentration, group = measurement_id)) +
  geom_line() +
  geom_line(aes(y = rolling_sic), color = "red") +
  geom_point(
    data = owd %>% semi_join(df_viz, by = "measurement_id"),
    aes(x = date_sic_15_percent, y = rolling_sic),
    color = "blue",
    size = 2
  ) +
  geom_hline(
    yintercept = 15,
    color = "blue",
    lty = 2
  ) +
  geom_vline(aes(xintercept = date), color = "green", lty = 2) +
  facet_wrap(~measurement_id, scales = "free_x")

# Calculate the number of OWD. It is possible that there are multiple "moments"
# where SIC is at least 3 days <= 15%. In these cases, I will use the latest
# date.

owd <- owd %>%
  mutate(owd = date - date_sic_15_percent) %>%
  group_by(measurement_id) %>%
  filter(date_sic == max(date_sic)) %>%
  ungroup() %>%
  select(measurement_id, owd)

owd %>%
  count(measurement_id, sort = TRUE)

sic %>%
  filter(measurement_id == 530) %>%
  ggplot(aes(x = date_sic, y = rolling_sic)) +
  geom_line() +
  geom_hline(yintercept = 15)

# Merge MVP and OWD -------------------------------------------------------

mvp
owd

mvp_with_owd <- mvp %>%
  inner_join(owd, by = "measurement_id")

# Visualize OWD -----------------------------------------------------------

# Most OWD are positive. This makes sens since the MVP was deployed in open
# water.

mvp_with_owd %>%
  ggplot(aes(x = owd)) +
  geom_histogram(bins = 50)

# There are some obervations with OWD at 50, see if it makes sens.

mvp_with_owd %>%
  filter(owd == max(owd)) %>%
  semi_join(sic, .) %>%
  ggplot(aes(x = date_sic, y = rolling_sic)) +
  geom_line() +
  geom_hline(yintercept = 15) +
  geom_vline(aes(xintercept = date)) +
  scale_x_date(breaks = "10 days")

# Export ------------------------------------------------------------------

fwrite(mvp_with_owd, here("data","clean","mvp_with_owd.csv"))
