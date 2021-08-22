# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Calculate CP using MVP data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

mvp <- read_csv(here("data","clean","mvp_with_owd.csv"))

# Beam attenuation (cp) ---------------------------------------------------

# https://www.seabird.com/c-star-25-cm-red-6000-m/product-details?id=54627910468&callback=pf

# Path Length: 25 cm
# Spectral Bandwidth: ~ 20 nm
# Wavelength: 650 nm

r <- 0.25

mvp <- mvp %>%
  mutate(cp = -(1 / r) * log10(trans / 100))

fwrite(mvp, here("data","clean","mvp_with_owd.csv"))
