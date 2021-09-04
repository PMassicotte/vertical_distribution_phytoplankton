# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Visualize the bbp/cp ratio. For the CP data, I will use the CTD
# data and for the bbp the Hydroscat.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","interpolate_fun.R"))

hydroscat <- read_csv(here::here("data","clean","hydroscat.csv")) %>%
  rename(depth_m = depth) %>%
  select(station, transect, owd, wavelength, depth_m, bbp)

ctd <- fread(here("data","clean","ctd.csv")) %>%
  dtplyr::lazy_dt() %>%
  select(station, transect, depth_m, owd, cp) %>%
  mutate(ctd_depth_m = depth_m) %>%
  as_tibble()

setDT(hydroscat)
setDT(ctd)

# Thank to data.table rolling join!
df <- ctd[hydroscat, roll = "nearest", on = .(station, transect, owd, depth_m)]

df <- df %>%
  as_tibble() %>%
  mutate(depth_difference_m = abs(depth_m - ctd_depth_m))

# The matched depths are pretty close, I will keep all the data
df %>%
  ggplot(aes(x = depth_difference_m)) +
  geom_histogram(bins = 60)

# Interpolation -----------------------------------------------------------


df_viz <- df %>%
  drop_na(cp, bbp) %>%
  filter(bbp >= 0) %>%
  filter(cp >= 0) %>%
  mutate(bbp_cp_ratio = bbp / cp) %>%
  dtplyr::lazy_dt() %>%
  group_by(wavelength, owd, depth_m) %>%
  summarise(mean_bbp_cp_ratio = mean(bbp_cp_ratio, na.rm = TRUE), n = n()) %>%
  as_tibble() %>%
  drop_na() %>%
  filter(depth_m <= 100)

df_viz <- df_viz %>%
  group_nest(wavelength)  %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, mean_bbp_cp_ratio, h = 6))

res <- df_viz %>%
  unnest(res) %>%
  rename(owd = x, depth_m = y, mean_bbp_cp_ratio = z) %>%
  select(-data) %>%
  drop_na(mean_bbp_cp_ratio) %>%
  mutate(mean_bbp_cp_ratio = ifelse(mean_bbp_cp_ratio < 0, 0, mean_bbp_cp_ratio)) %>%
  mutate(wavelength = glue("{wavelength} (nm)"))

# Isolume -----------------------------------------------------------------

isolume <-
  read_csv(here("data", "raw", "randelhoff2019", "FIGURE_9-c-d-e.csv")) %>%
  janitor::clean_names() %>%
  select(owd, starts_with("isolume")) %>%
  pivot_longer(starts_with("isolume"),
    names_to = "isolume",
    values_to = "depth_m"
  )

# Plot --------------------------------------------------------------------

range(res$mean_bbp_cp_ratio)

p <- res %>%
  ggplot(aes(x = owd, y = depth_m, z = mean_bbp_cp_ratio, fill = mean_bbp_cp_ratio)) +
  geom_isobands(color = NA, breaks = seq(0, 2, by = 0.005)) +
  paletteer::scale_fill_paletteer_c(
    "oompaBase::jetColors",
    # trans = "sqrt",
    breaks = scales::breaks_pretty(n = 8),
    guide =
      guide_colorbar(
        barwidth = unit(8, "cm"),
        barheight = unit(0.2, "cm"),
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
  ) +
  geom_line(
    data = isolume,
    size = 1,
    aes(x = owd, y = depth_m, color = isolume),
    inherit.aes = FALSE) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.05)),
    breaks = scales::breaks_pretty(n = 8)
  ) +
  paletteer::scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  labs(
    y = "Depth (m)",
    title = "bbp/cp ratio at different wavelengths",
    subtitle = "Data from the Hydroscat (bbp) and the CTD (cp)."
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
    legend.box = "vertical",
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  )

ggsave(
  here("graphs","26_bbp_cp_by_wavelength.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 8
)
