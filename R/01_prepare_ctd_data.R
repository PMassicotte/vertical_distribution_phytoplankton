# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Some cleanup of the CTD data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

owd <- read_csv(
  "https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/Randelhoff-et-al-2019_GreenEdge_per-station_v1.0.csv",
  na = "NaN"
) %>%
  janitor::clean_names() %>%
  select(station, owd)

ctd <- fread(here::here("data/raw/ctd.csv")) %>%
  as_tibble()

ctd <- ctd %>%
  filter(str_detect(mission, "amundsen")) %>%
  filter(str_detect(station, "^G\\d*")) %>%
  mutate(station = parse_number(station)) %>%
  mutate(transect = station %/% 100 * 100, .after = station)

ctd

ctd %>%
  distinct(station)

# Select a subset of data -------------------------------------------------

# Let's select only the variables needed for this study

ctd <- ctd %>%
  select(
    station,
    transect,
    date_time,
    latitude,
    longitude,
    cast,
    depth_m,
    flor_mg_m3,
    tran_percent
  )

ctd <- ctd %>%
  inner_join(owd, by = "station")

ctd <- ctd %>%
  group_by(station, cast) %>%
  arrange(depth_m) %>%
  ungroup()

# Smooth the vertical profiles --------------------------------------------

ctd <- ctd %>%
  # filter(depth_m <= 100) %>%
  group_by(station, cast) %>%
  mutate(across(
    c(flor_mg_m3, tran_percent),
    ~ RcppRoll::roll_median(., n = 25, fill = NA, na.rm = TRUE, align = "center"),
    .names = "roll_{.col}"
  )) %>%
  arrange(station, cast, depth_m) %>%
  ungroup()

p1 <- ctd %>%
  filter(station == 115) %>%
  ggplot(aes(
    x = tran_percent,
    y = depth_m,
    group = interaction(station, cast)

      )) +
  geom_path(, size = 0.25) +
  geom_path(aes(x = roll_tran_percent),
    color = "red",
    size = 0.25
  ) +
  scale_y_reverse() +
  facet_wrap(~ glue("station {station} (cast {cast})"), scales = "free") +
  labs(
    title = "Vertical profiles of CTD transmittance",
    subtitle = str_wrap(
      "We can clearly see unwanted peaks in the data. The red line is a median rolling window using 25 observations.",
      80
    ),
    x = "Transmittance (%)",
    y = "Depth (m)"
  )

ggsave(
  here("graphs/01_ctd_transmittance_with_peaks.pdf"),
  device = cairo_pdf,
  height = 4,
  width = 7
)

ctd

# Rename smoothed columns to the original ones ----------------------------

ctd <- ctd %>%
  select(-flor_mg_m3, -tran_percent) %>%
  rename(
    flor_mg_m3 = roll_flor_mg_m3,
    tran_percent = roll_tran_percent
  )

# Correct transmittance data ----------------------------------------------

# A lot of transmittance >= 100, problem! Email sent to Pascal Guillot to
# understand what is going on.

ctd %>%
  drop_na(tran_percent) %>%
  mutate(
    tran_class = case_when(
      tran_percent > 100 ~ "Transmittance above 100%",
      tran_percent <= 100 ~ "Transmittance below 100%",
      TRUE ~ NA_character_
    )
  ) %>%
  ggplot(aes(x = tran_percent)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~tran_class, scales = "free") +
  labs(
    title = "CTD transmittance",
    subtitle = glue("Range of transmittance {paste(range(ctd$tran_percent, na.rm = TRUE), collapse = ' - ')}")
  )

# Because of this, rescale the transmittance values. After a discussion with
# Pascal, he told me to re-scale the transmittance data between xxx-100%.

range(ctd$tran_percent, na.rm = TRUE)

mint <- min(ctd$tran_percent, na.rm = TRUE)

ctd <- ctd %>%
  mutate(tran_percent = scales::rescale(tran_percent, to = c(mint, 99.999)))

range(ctd$tran_percent, na.rm = TRUE)

# Average data because multiple casts -------------------------------------

# There are at least two cast per station.
ctd %>%
  mutate(date = as.Date(date_time)) %>%
  distinct(station, cast, date) %>%
  count(station, sort = TRUE)

# Is it ok to average?
ctd %>%
  filter(station == 207) %>%
  ggplot(aes(x = flor_mg_m3, y = depth_m, group = cast, color = factor(cast))) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~glue("station {station} / {date_time}"), scales = "fixed")

setDT(ctd)

ctd <- ctd[,
  lapply(.SD, mean, na.rm = TRUE),
  by = .(station, transect, longitude, latitude, depth_m),
  .SDcols = c("owd", "flor_mg_m3", "tran_percent")
] %>%
  as_tibble()

# The minimun transmittance was at 19.34, but is now around 43% because of
# station 713 cast 194 seems to be an outlier
range(ctd$tran_percent, na.rm = TRUE)

# Particle beam attenuation coefficient (CP) ------------------------------

# ftp://ftp.nodc.noaa.gov/nodc/archive/arc0022/0001155/1.1/data/1-data/docs/PI-NOTES/arabian/Gardner-beamcp.htm

# Beam transmission was converted to beam attenuation coefficients using
# c=-(1/r)*ln(%Tr/100) where c=beam attenuation coefficient (m^-1), r=beam path
# length (m), and Tr=% beam transmission.

# Check Table 1 for an idea of the range of Cp.
# https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2015JC010878

# 25 cm beam, confirmed by Pascal Guillot
r <- 0.25

# From Pascal:

# Voici les infos (Thomas, un des techs de AS a contacté SeaBird):
#
# All the instruments listed have a wavelength of 657nm (using a red LED):
#
# CST-558DR – 657nm
# CST-671DR – 657nm
# CST-2021 – 657nm
# CST-2022 - 657nm

ctd <- ctd %>%
  mutate(cp = -(1 / r) * log10(tran_percent / 100))

fwrite(ctd, here("data/clean/ctd.csv"))

# Check that we have all the stations -------------------------------------

ctd %>%
  distinct(station, longitude, latitude, .keep_all = TRUE) %>%
  ggplot(aes(x = longitude, y = latitude, color = owd)) +
  geom_point(size = 2) +
  scale_color_viridis_c()

ctd %>%
  distinct(station, transect, longitude, latitude, depth_m) %>%
  ggplot(aes(x = longitude, y = depth_m, group = station)) +
  geom_line() +
  scale_y_reverse() +
  facet_wrap(~transect, scales = "free_x")

ctd %>%
  filter(transect == 200) %>%
  distinct(station, transect, longitude, latitude, depth_m) %>%
  ggplot(aes(x = longitude, y = depth_m)) +
  geom_line(aes(color = factor(station))) +
  scale_y_reverse() +
  facet_wrap(~transect, scales = "free_x")

ctd %>%
  count(station, depth_m) %>%
  assertr::verify(n == 1)
