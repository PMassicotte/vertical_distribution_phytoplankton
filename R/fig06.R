# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Visualize the bbp/cp ratio. For the CP data, I will use the CTD
# data and for the bbp the Hydroscat.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# TODO: It seems that offseting CP by its minimum has an important effect...

rm(list = ls())

source(here("R","interpolate_fun.R"))
source(here("R","plot_funs.R"))
source(here("R","utils.R"))

breaks <- c(-30, -10, 10, 40)

# Hydroscat ---------------------------------------------------------------

# For bbp data
hydroscat <- vroom::vroom(here::here("data/clean/hydroscat.csv"), altrep = TRUE) %>%
  rename(depth_m = depth) %>%
  select(station, transect, owd, wavelength, depth_m, bbp) %>%
  filter(wavelength == 620)

# CTD ---------------------------------------------------------------------

# For cp data
ctd <- fread(here("data/clean/ctd.csv")) %>%
  dtplyr::lazy_dt() %>%
  select(station, transect, depth_m, owd, cp) %>%
  mutate(ctd_depth_m = depth_m) %>%
  as_tibble()

# Make sure there is only 1 measurement per station/depth
ctd %>%
  dtplyr::lazy_dt() %>%
  count(station, depth_m, sort = TRUE) %>%
  as_tibble() %>%
  assertr::verify(n == 1)

## Correct for CP offset ----

# Using the minimum CP value along the complete vertical profile before keeping
# only measurements above 100 m for the other analyses.

range(ctd$cp, na.rm = TRUE)

ctd %>%
  ggplot(aes(x = cp)) +
  geom_histogram()

ctd <- ctd %>%
  group_by(station) %>%
  mutate(cp = cp - min(cp, na.rm = TRUE)) %>%
  ungroup()

ctd %>%
  ggplot(aes(x = cp)) +
  geom_histogram()

# %>%
#   filter(depth_m <= 100)

range(ctd$cp, na.rm = TRUE)

# Merge Hydroscat and CTD data --------------------------------------------

# Merge on the nearest depth

setDT(hydroscat)
setDT(ctd)

# Thank to data.table rolling join!
df <- ctd[hydroscat, roll = "nearest", on = .(station, transect, owd, depth_m)]

df <- df %>%
  as_tibble() %>%
  mutate(depth_difference_m = abs(depth_m - ctd_depth_m))

df

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
  mutate(mean_bbp_cp_ratio = ifelse(mean_bbp_cp_ratio < 0, 0, mean_bbp_cp_ratio))

# Isolume data ------------------------------------------------------------

isolume <-
  read_csv(
    "https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/FIGURE_9-c-d-e.csv"
  ) %>%
  janitor::clean_names() %>%
  select(owd, isolume_01) %>%
  pivot_longer(starts_with("isolume"), names_to = "isolume", values_to = "depth_m") %>%
  filter(owd <= max(df$owd, na.rm = TRUE))

# Plot --------------------------------------------------------------------

range(res$mean_bbp_cp_ratio)

p1 <- res %>%
  ggplot(aes(x = owd, y = depth_m, z = mean_bbp_cp_ratio, fill = mean_bbp_cp_ratio)) +
  geom_isobands(color = NA, breaks = seq(0, 2, by = 0.005)) +
  paletteer::scale_fill_paletteer_c(
    "oompaBase::jetColors",
    trans = "log10",
    breaks = scales::breaks_log(n = 6),
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
    aes(x = owd, y = depth_m),
    inherit.aes = FALSE) +
  scale_y_reverse(
    expand = c(0, 0),
    breaks = seq(0, 100, by = 10)
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.05)),
    breaks = scales::breaks_pretty(n = 8)
  ) +
  paletteer::scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  labs(
    y = "Depth (m)",
    x = "Number of open water days (OWD)",
    fill = quote(b[bp]/C[p])
  ) +
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

## Average vertical profiles ----

df_average_profiles <- average_vertical_profiles(res, mean_bbp_cp_ratio, breaks = breaks)

p2 <- gg2dprofiles(df_average_profiles, mean_bbp_cp_ratio, depth_m, owd_bin)

# Save plots --------------------------------------------------------------

p <- p1 + p2 +
  plot_layout(ncol = 2, widths = c(0.7, 0.3)) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here::here("graphs","fig06.pdf"),
  device = cairo_pdf,
  width = 9,
  height = 4
)

