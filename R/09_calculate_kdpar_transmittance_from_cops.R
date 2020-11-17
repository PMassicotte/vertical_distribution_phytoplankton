# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Calculate KdPAR and extract PAR transmittance.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# cops <- fread("/media/4TB/work-ulaval/projects/green_edge/cops/data/raw/cops/amundsen/2016/RES.EXCEL/GE2016.AMOW_COPS_160609_CAST_001/d.fit.v.01.txt", na.strings = "-999") %>%
#   as_tibble() %>%
#   janitor::clean_names()
#
# range(cops$par_d_fit_percent_percent)
#
# cops %>%
#   ggplot(aes(y = depth_edz_m, x = par_d_fit_percent_percent)) +
#   geom_line() +
#   scale_y_reverse()

files <- fs::dir_ls(
  "/media/4TB/work-ulaval/projects/green_edge/cops/data/raw/cops/amundsen/2016/RES.EXCEL/",
  recurse = TRUE,
  regexp = "d\\.fit.*\\.txt"
)

read_cops <- function(file) {
  df <- fread(file, na.strings = "-999") %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    select(
      longitude = longitude_dec_degrees,
      latitude = latitude_dec_degrees,
      depth_m = depth_edz_m,
      par_d_fit_percent_percent
    ) %>%
    drop_na(par_d_fit_percent_percent) %>%
    mutate(par_d_fit_percent_percent = par_d_fit_percent_percent / 100) %>%
    mutate(cast = str_match(file, "CAST_(\\d{3})")[, 2], .before = longitude) %>%
    mutate(filename = file)

  return(df)
}

df <- map_df(files, read_cops)

df

# Looks like some transmittances are > 100%...
df %>%
  ggplot(aes(x = par_d_fit_percent_percent)) +
  geom_histogram(binwidth = 0.05) +
  scale_x_continuous(
    labels = scales::label_percent(),
    breaks = scales::breaks_pretty(n = 10)
  )

df <- df %>%
  filter(par_d_fit_percent_percent <= 1) %>%
  filter(depth_m <= 50)

df %>%
  ggplot(aes(x = par_d_fit_percent_percent)) +
  geom_histogram(binwidth = 0.05) +
  scale_x_continuous(
    labels = scales::label_percent(),
    breaks = scales::breaks_pretty(n = 10)
  )

# Calculate Kd PAR and extract transmittance ------------------------------

df <- df %>%
  group_nest(cast, longitude, latitude, filename) %>%
  mutate(par_transmittance = map_dbl(data, ~ .$par_d_fit_percent_percent[which.min(.$depth_m)])) %>%
  mutate(mod = map(data, function(df) {
    mod <- minpack.lm::nlsLM(
      par_d_fit_percent_percent ~ a0 * exp(-kd_par * depth_m),
      start = list(a0 = 1, kd_par = 0.8),
      data = df
    )

    return(mod)
  })) %>%
  mutate(kd_par = map_dbl(mod, ~ coef(.)[2])) %>%
  mutate(pred = map2(data, mod, modelr::add_predictions)) %>%
  mutate(r2 = map_dbl(pred, ~ cor(.$par_d_fit_percent_percent, .$pred)^2))

df

# Visualize
p <- df %>%
  mutate(r2_ok = r2 >= 0.98) %>%
  # slice(47) %>%
  unnest(pred) %>%
  ggplot(aes(x = par_d_fit_percent_percent, y = depth_m)) +
  geom_point(color = "black") +
  geom_line(aes(x = pred, color = r2_ok)) +
  scale_y_reverse() +
  scale_x_continuous(labels = scales::label_percent()) +
  labs(
    title = "Vertical profiles of PAR transmittance and fitted model to calculate KdPAR",
    subtitle = "Only models with R2 >= 0.98 are kept for further analyses.",
    color = "R2 >= 0.98",
    x = "PAR transmittance (%)",
    y = "Depth (m)"
  ) +
  facet_wrap(~glue("{basename(dirname(filename))}"), scales = "free") +
  theme(
    strip.text = element_text(size = 4),
    legend.position = "top"
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 2))
  )

ggsave(
  here::here("graphs/09_01_cops_kd_par_and_transmittance.pdf"),
  device = cairo_pdf,
  width = 20,
  height = 20
)

# Only keep very good fits
df <- df %>%
  filter(r2 >= 0.98) %>%
  select(!where(is.list))

# Find stations -----------------------------------------------------------

# The COPS data do not have the station numbers. I will use another file to find
# it.

station <- read_csv("/media/4TB/work-ulaval/projects/green_edge/cops/data/raw/cops/amundsen/2016/station.correspondance.GE2016.AMIS.AMOW.csv") %>%
  janitor::clean_names() %>%
  select(station, longitude = lon, latitude = lat, deployement) %>%
  separate(longitude, into = c("deg", "minute"), sep = "W", convert = TRUE) %>%
  mutate(longitude = deg + (minute / 60)) %>%
  mutate(longitude = -longitude) %>%
  separate(latitude, into = c("deg", "minute"), sep = "N", convert = TRUE) %>%
  mutate(latitude = deg + (minute / 60)) %>%
  select(-deg, -minute) %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100, .after = station) %>%
  group_by(station, transect, deployement) %>%
  summarise(across(c(longitude, latitude), .fns = mean)) %>%
  ungroup()

station

station %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  ggplot() +
  geom_sf()

# Match with station, using a maximum distance of 1 km
df <- df %>%
  fuzzyjoin::geo_left_join(
    station,
    max_dist = 1,
    distance_col = "dist",
    unit = "km",
    by = c("longitude", "latitude")
  ) %>%
  drop_na(dist) %>%
  group_by(filename) %>%
  filter(dist == min(dist)) %>%
  ungroup()

df %>%
  ggplot(aes(
    x = longitude.x,
    y = latitude.x,
    color = factor(transect)
  )) +
  geom_point(size = 3) +
  geom_point(aes(x = longitude.y, y = latitude.y), color = "black")

# Averaging ---------------------------------------------------------------

# There are many cast per station, is it ok to average Kd PAR and PAR
# transmittance?

df %>%
  count(station, deployement)

p1 <- df %>%
  ggplot(aes(x = cast, y = kd_par)) +
  geom_col() +
  facet_wrap(~station, scales = "free") +
  labs(
    y = bquote(Kd[PAR]~(m^{-1}))
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(size = 14, face = "bold")
  )

p2 <- df %>%
  ggplot(aes(x = cast, y = par_transmittance)) +
  geom_col() +
  facet_wrap(~station, scales = "free") +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    y = "PAR transmittance (%)"
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(size = 14, face = "bold")
  )

p <- p1 / p2 +
  plot_annotation(
    title = "Comparing Kd PAR and PAR transmittance at each station",
    subtitle = "Each bar represents a cast. when there are more than one cast, it looks like it is ok to average by station.",
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(size = 30),
      plot.subtitle = element_text(size = 20)
    )
  )

ggsave(
  here::here("graphs/09_02_barplots_comparing_kdpar_transmittance_per_station.pdf"),
  device = cairo_pdf,
  width = 20,
  height = 20
)

# Average
df <- df %>%
  select(-contains(".y"), -filename) %>%
  rename(
    longitude = longitude.x,
    latitude = latitude.x
  ) %>%
  group_by(station, transect, deployement) %>%
  summarise(across(c(longitude, latitude, par_transmittance, kd_par), mean)) %>%
  ungroup()

# Final visualization -----------------------------------------------------

p1 <- df %>%
  ggplot(aes(x = deployement, y = kd_par)) +
  geom_boxplot(size = 0.25) +
  labs(
    x = "Type of deployment",
    y = bquote(Kd[PAR] ~ (m^{-1}))
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

p2 <- df %>%
  ggplot(aes(x = deployement, y = par_transmittance)) +
  geom_boxplot(size = 0.25) +
  labs(
    x = "Type of deployment",
    y = "Transmittance"
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

p <- p1 / p2 +
  plot_annotation(
    title = str_wrap(
      "Boxplot comparing Kd and transmittance for the 42 COPS stations during the Amundsen cruise",
      60
    ),
    tag_levels = "A",
    theme = theme(plot.title = element_text(size = 14))
  )

ggsave(
  here::here("graphs/09_03_boxplot_comparing_kdpar_transmittance_per_deployment.pdf"),
  width = 6,
  height = 6
)

p3 <- df %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  ggplot() +
  geom_sf(aes(color = deployement)) +
  labs(
    title = glue("Localisation of the {nrow(df)} COPS measurements")
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank()
  )

ggsave(
  here::here("graphs/09_04_map_cops_type_of_deployment.pdf"),
  width = 8,
  height = 6
)

# Export ------------------------------------------------------------------

df %>%
  write_csv(here::here("data/clean/cops_kd_par_transmittance.csv"))
