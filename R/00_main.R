# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# FILE:         main.R
#
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Load libraries and script recipes.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Setup -------------------------------------------------------------------

library(tidyverse)
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
library(ggfortify)
library(tidymodels)
library(vip)

# Set default ggplot2 font size and font family
# devtools::install_github("PMassicotte/ggpmthemes")
theme_set(theme_poppins(base_size = 10))

theme_update(
  strip.background = element_blank(),
  strip.text = element_text(face = "bold", size = 14)
)

# Run all -----------------------------------------------------------------

source(here("R","01_prepare_ctd_data.R"))
source(here("R","02_visualize_phytoplankton_biomass_fluorescence_cp_from_ctd.R"))
source(here("R","03_visualize_phytoplankton_biomass_bbp_from_hydroscat.R"))
source(here("R","04_visualize_phytoplankton_biomass_pigments_from_rosette.R"))
source(here("R","05_visualize_phytoplankton_biomass_pigments_from_mvp.R"))
source(here("R","06_visualize_ek.R"))
source(here("R","07_visualize_insitu_primary_production.R"))
source(here("R","08_calculate_hourly_par_from_sbdart.R"))
source(here("R","09_calculate_kdpar_transmittance_from_cops.R"))
source(here("R","10_quenching_correction_ctd_fluorescence.R"))
source(here("R","11_map_spatial_distribution_primary_production_parameters.R"))
source(here("R","12_vertical_propagation_par_water_column.R"))
source(here("R","13_daily_par_in_water_column.R"))
source(here("R","14_vertical_propagation_pvse_water_column.R"))
source(here("R","15_vertical_propagation_fluorescence_water_column.R"))
source(here("R","16_calculate_daily_primary_production_from_pvse.R"))
source(here("R","17_compare_ctd_and_hplc_chla.R"))
source(here("R","18_visualize_poc.R"))
source(here("R","19_histograms_pvse_parameters.R"))
source(here("R","20_visualize_primary_production_from_pvse.R"))
source(here("R","21_compare_primary_production_insitu_vs_pvse.R"))
source(here("R","22_absorption_phyto_nap.R"))
source(here("R","23_explore_blue_red_phytoplankton_absorption.R"))
source(here("R","24_hydroscat_bbp_vs_ctd_fluo.R"))
source(here("R","25_scatterplot_poc_vs_cp.R"))
source(here("R","26_visualize_bbp_cp_ratio.R"))
source(here("R","27_xxxx_marcel.R"))

source(here("R","30_visualize_uvp_particles_size.R"))

# Figures for the paper ---------------------------------------------------

source(here("R","fig01.R"))
source(here("R","fig02.R"))
source(here("R","fig03.R"))
source(here("R","fig04.R"))
source(here("R","fig05.R"))
source(here("R","fig06.R"))
# source(here("R","fig07.R"))
source(here("R","fig08.R"))
source(here("R","fig09.R"))
