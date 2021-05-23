# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# FILE:         main.R
#
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Load libraries and script recipes.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Setup -------------------------------------------------------------------

library(tidyverse)
# library(ggtern)
library(ggpmthemes)
library(ggisoband)
library(data.table)
library(glue)
library(MBA)
library(sf)
library(patchwork)
library(ggtext)
library(here)
library(lubridate)
library(santoku)
library(magrittr)
library(tidymodels)
library(ggfortify)

# Set default ggplot2 font size and font family
# devtools::install_github("PMassicotte/ggpmthemes")
theme_set(theme_poppins(base_size = 10))

theme_update(
  strip.background = element_blank()
)

# Run all -----------------------------------------------------------------

source("R/01_prepare_ctd_data.R")
source("R/02_visualize_phytoplankton_biomass_fluorescence_cp_from_ctd.R")
source("R/03_visualize_phytoplankton_biomass_bbp_from_hydroscat.R")
source("R/04_visualize_phytoplankton_biomass_pigments_from_rosette.R")
source("R/05_visualize_phytoplankton_biomass_pigments_from_mvp.R")
source("R/06_visualize_ek.R")
source("R/07_visualize_insitu_primary_production.R")
source("R/08_calculate_hourly_par_from_sbdart.R")
source("R/09_calculate_kdpar_transmittance_from_cops.R")
source("R/10_quenching_correction_ctd_fluorescence.R")
source("R/11_map_spatial_distribution_primary_production_parameters.R")
source("R/12_vertical_propagation_par_water_column.R")
source("R/13_daily_par_in_water_column.R")
source("R/14_vertical_propagation_pvse_water_column.R")
source("R/15_vertical_propagation_fluorescence_water_column.R")
source("R/16_calculate_daily_primary_production_from_pvse.R")
source("R/17_compare_ctd_and_hplc_chla.R")
source("R/18_visualize_poc.R")
source("R/19_histograms_pvse_parameters.R")
source("R/20_visualize_primary_production_from_pvse.R")
source("R/21_compare_primary_production_insitu_vs_pvse.R")
source("R/22_absorption_phyto_nap.R")
source("R/23_explore_blue_red_phytoplankton_absorption.R")
source("R/24_hydroscat_bbp_vs_ctd_fluo.R")
source("R/25_scatterplot_poc_vs_cp.R")
source("R/26_visualize_bbp_cp_ratio.R")
source("R/27_xxxx_marcel.R")

source("R/30_visualize_uvp_particles_size.R")
