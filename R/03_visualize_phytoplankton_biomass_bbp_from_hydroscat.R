# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore the vertical distribution of the phytoplankton biomass
# using bbp.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/interpolate_fun.R")

# Isolume data ------------------------------------------------------------

isolume <-
  read_csv(
    "https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/FIGURE_9-c-d-e.csv"
  ) %>%
  janitor::clean_names() %>%
  select(owd, starts_with("isolume")) %>%
  pivot_longer(starts_with("isolume"), names_to = "isolume", values_to = "depth_m")

# Data preparation --------------------------------------------------------

files <-
  fs::dir_ls(
    here::here(
      "data/raw/GreenEdge_Hydroscat_Asphere/GreenEdge_Hydroscat_Asphere/HS6/"
    )
  )

df <- read_csv(files,
  id = "filename",
  col_select = list(filename, depth, fchla = FCHLa, starts_with("bb_p"))
) %>%
  mutate(station = str_match(filename, "(G\\d+\\S+)\\_\\d+")[, 2], .after = filename) %>%
  select(-filename) %>%
  pivot_longer(starts_with("bb_p"), names_to = "wavelength", values_to = "bbp") %>%
  mutate(across(c(station, wavelength), parse_number)) %>%
  mutate(transect = station %/% 100 * 100, .after = station)

df

owd <- read_csv("https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/Randelhoff-et-al-2019_GreenEdge_per-station_v1.0.csv", na = "NaN") %>%
  janitor::clean_names() %>%
  select(station, owd)

df <- df %>%
  inner_join(owd, by = "station")

# Export clean hydroscat data ---------------------------------------------

df %>%
  write_csv(here("data/clean/hydroscat.csv"))

# Visualize the data ------------------------------------------------------

df %>%
  count(wavelength)

df %>%
  # filter(depth == 2) %>%
  # filter(station == 100) %>%
  ggplot(aes(x = wavelength, y = bbp, color = depth, group = depth)) +
  geom_line(size = 0.1) +
  facet_wrap(~station, scales = "free")

p <- df %>%
  filter(depth <= 100) %>%
  ggplot(aes(x = bbp, y = depth, color = factor(wavelength))) +
  geom_path(size = 0.25) +
  facet_wrap(~station, scales = "free", ncol = 3) +
  scale_y_reverse() +
  geom_vline(xintercept = 0.0012, lty = 2, size = 0.5) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3), labels = scales::label_number()) +
  labs(
    color = "Wavelength (nm)",
    title = "Vertical profiles of bbp by station and wavelength",
    subtitle = str_wrap("The vertical dashed lines show the bbp threshold of 0.0012 below which there are no relationships with chla (Behrenfeld, 2005). Data from the Hydroscat.", 100)
  ) +
  theme(
    legend.position = "bottom",
    panel.grid = element_line(color = "gray60", size = 0.1)
  )

ggsave(
  here::here("graphs/03_vertical_profiles_bbp.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 16
)

# Chla
df %>%
  filter(depth <= 100) %>%
  distinct(station, transect, depth, fchla, owd) %>%
  mutate(station = fct_reorder(as.character(station), owd)) %>%
  ggplot(aes(x = fchla, y = depth, group = station, color = owd)) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~station, scales = "free")

# ggsave("~/Desktop/test.pdf", device = cairo_pdf, width = 12, height = 8)


# Summarize data before interpolation -------------------------------------

# Summarize by depth and open water day

df_viz <- df %>%
  dtplyr::lazy_dt() %>%
  group_by(owd, depth, wavelength) %>%
  summarise(mean_bbp = mean(bbp, na.rm = TRUE), n = n()) %>%
  as_tibble() %>%
  drop_na() %>%
  filter(depth <= 100)

df_viz

# Interpolation -----------------------------------------------------------

df_viz <- df_viz %>%
  group_nest(wavelength) %>%
  mutate(res = map(data, interpolate_2d, owd, depth, mean_bbp)) %>%
  unnest(res) %>%
  rename(owd = x, depth_m = y, mean_bbp = z) %>%
  select(-data) %>%
  mutate(mean_bbp = ifelse(mean_bbp < 0, 0, mean_bbp))

df_viz

# Visualize the interpolated data -----------------------------------------

# Make sure that isolume data span the same range of owd as the hydroscat data.
isolume <- isolume %>%
  filter(owd <= max(df_viz$owd, na.rm = TRUE))

p <- df_viz %>%
  drop_na(mean_bbp) %>%
  mutate(wavelength = paste(wavelength, "nm")) %>%
  ggplot(aes(x = owd, y = depth_m, z = mean_bbp, fill = mean_bbp)) +
  geom_isobands(color = NA, bins = 15) +
  paletteer::scale_fill_paletteer_c(
    "oompaBase::jetColors",
    trans = "sqrt",
    breaks = scales::breaks_pretty(n = 4),
    guide =
      guide_colorbar(
        barwidth = unit(8, "cm"),
        barheight = unit(0.2, "cm"),
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
  ) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::breaks_pretty(n = 8)
  ) +
  geom_line(data = isolume, size = 1, aes(x = owd, y = depth_m, color = isolume), inherit.aes = FALSE) +
  paletteer::scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  labs(
    y = "Depth (m)",
    fill = bquote(italic(b[bp])~(m^{-1})),
    title = str_wrap("Vertical distribution of bbp based on the number of open water days", 60),
    subtitle = "For the first 100 meters of the water column. Data from the Hydroscat."
  ) +
  facet_wrap(~wavelength, ncol = 2) +
  theme(
    panel.grid = element_line(color = "gray60", size = 0.1),
    panel.background = element_rect(fill = NA),
    panel.ontop = TRUE,
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 10, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  )

ggsave(
  here::here("graphs/03_phytoplankton_biomass_hydroscat.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 8
)
