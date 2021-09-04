# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Function to process SBDART irradiance data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

read_irradiance <- function(file) {
  df <- fread(file, col.names = paste0("hour_", 0:23), header = FALSE) %>%
    as_tibble() %>%
    mutate(wavelength = seq(290, 700, by = 5)) %>%
    pivot_longer(
      starts_with("hour"),
      names_to = "hour",
      values_to = "irradiance",
      names_transform = list(hour = parse_number)
    )

  return(df)
}

# List all files and read them --------------------------------------------

files <- fs::dir_ls(
  here::here("data","raw","SBDART_GreenEdge","AM2016","AM2016_SBDART_AllCasts"),
  recurse = TRUE,
  glob = "*.txt"
)

df <- files %>%
  enframe(name = NULL, value = "filename") %>%
  mutate(cast = str_match(filename, "cast(\\d{3})")[, 2]) %>%
  mutate(file = basename(filename)) %>%
  mutate(file = str_remove(file, ".txt")) %>%
  separate(
    file,
    into = c(NA, "year", "yday", "latitude", "longitude"),
    sep = "_",
    convert = TRUE
  ) %>%
  filter(latitude >= 65)

df

df %>%
  count(cast)

df %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point()

df <- df %>%
  rowwise() %>%
  mutate(irradiance = list(read_irradiance(filename))) %>%
  ungroup()

df

# Export tidied data ------------------------------------------------------

df %>%
  select(-filename) %>%
  unnest(irradiance) %>%
  fwrite(here::here("data","clean","irradiance_sbdart_ed0p.csv"))

# Calculate hourly --------------------------------------------------------

# From the CYBER website:
# Spectral irradiance Ed0 (SBDART output): [µmol photons m-2 s-1 nm-1]
# PAR0 Daily Metrics: [µmol photons m-2 s-1]

df <- df %>%
  mutate(hourly_par_0p_umol_m2_s1 = map(irradiance, function(irradiance) {
    res <- irradiance %>%
      filter(between(wavelength, 400, 700)) %>%
      group_by(hour) %>%
      summarise(hourly_par_0p_umol_m2_s1 = pracma::trapz(wavelength, irradiance))

    return(res)
  }))

df

# Looks like there are 3 replicates per cast. In fact, it seems that Marti
# produced data for the day before and after the sampling.
df %>%
  unnest(hourly_par_0p_umol_m2_s1) %>%
  ggplot(aes(x = hour, y = hourly_par_0p_umol_m2_s1, group = yday)) +
  geom_line() +
  facet_wrap(~cast)

# Yup, 3 ydays per cast
df %>%
  count(cast) %>%
  assertr::verify(n == 3)

# Average the hourly PAR for the 3 days
df <- df %>%
  select(-irradiance) %>%
  unnest(hourly_par_0p_umol_m2_s1) %>%
  group_by(cast, year, hour, latitude, longitude) %>%
  summarise(hourly_par_0p_umol_m2_s1 = mean(hourly_par_0p_umol_m2_s1)) %>%
  ungroup()

df %>%
  unnest(hourly_par_0p_umol_m2_s1) %>%
  ggplot(aes(x = hour, y = hourly_par_0p_umol_m2_s1)) +
  geom_line() +
  facet_wrap(~cast)

# Check that there are 24 hours of data per cast
df %>%
  count(cast, longitude, latitude) %>%
  assertr::verify(n == 24)

# Associate station number ------------------------------------------------

station <- readxl::read_excel(
  "/media/4TB/work-ulaval/projects/green_edge/green_edge/data/GE_Amundsen_Station_Coordinates.xlsx"
) %>%
  janitor::clean_names() %>%
  select(station, longitude = long_deg_w, latitude = lat_deg_n) %>%
  mutate(longitude = -longitude) %>%
  filter(str_starts(station, "G")) %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100) %>%
  filter(station <= 1000)

res <- fuzzyjoin::distance_join(df,
  station,
  max_dist = 1,
  distance_col = "dist"
) %>%
  group_by(cast) %>%
  filter(dist == min(dist)) %>%
  ungroup()

res

res %>%
  ggplot(aes(x = longitude.x, y = latitude.x)) +
  geom_point() +
  geom_point(aes(x = longitude.y, y = latitude.y), color = "red") +
  facet_wrap(~station)

res <- res %>%
  select(-contains(".y")) %>%
  rename(longitude = longitude.x, latitude = latitude.x) %>%
  group_by(station, transect, longitude, latitude, hour) %>%
  summarise(hourly_par_0p_umol_m2_s1 = mean(hourly_par_0p_umol_m2_s1)) %>%
  ungroup()

p <- res %>%
  ggplot(aes(x = hour, y = hourly_par_0p_umol_m2_s1, group = station)) +
  geom_line(size = 0.1) +
  facet_wrap(~transect) +
  labs(
    title = "Hourly PAR by transect",
    subtitle = "Data from SBDART (calculated by Marti). Each black line represents a station.",
    x = "Hour",
    y = bquote("Hourly PAR" ~ (mu*mol~m^{-2}~s^{-1}))
  ) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(size = 14, face = "bold")
  )

ggsave(
  here::here("graphs","08_01_sbdart_hourly_par.pdf"),
  device = cairo_pdf,
  width = 10,
  height = 7
)

# Export ------------------------------------------------------------------

res %>%
  write_csv(here::here("data","clean","sbdart_hourly_par_0p.csv"))

