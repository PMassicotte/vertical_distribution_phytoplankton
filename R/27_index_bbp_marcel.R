# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  plot 2-D de log(bbp(532)/bbp(700)) / 0,274 (ici, je suppose que
# bbp varie spectralement suivant une loi de puissance lambda**-n, et on veut
# retrouver n qui fournit un indice de taille).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/interpolate_fun.R")

hydroscat <- read_csv(here::here("data/clean/hydroscat.csv")) %>%
  rename(depth_m = depth) %>%
  select(-fchla) %>%
  filter(wavelength %in% c(532, 700)) %>%
  filter(bbp >= 0)

hydroscat

df <- hydroscat %>%
  pivot_wider(names_from = wavelength, values_from = bbp, names_prefix = "bbp_") %>%
  mutate(index_marcel = log(bbp_532 / bbp_700) / 0.274)

df %>%
  ggplot(aes(x = index_marcel)) +
  geom_histogram(binwidth = 0.1)

df %>%
  ggplot(aes(x = owd, y = depth_m, fill = index_marcel)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_viridis_c()

# 3D plot of bbp/chla -----------------------------------------------------

isolume <-
  read_csv(
    "https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/FIGURE_9-c-d-e.csv"
  ) %>%
  janitor::clean_names() %>%
  select(owd, starts_with("isolume")) %>%
  pivot_longer(starts_with("isolume"), names_to = "isolume", values_to = "depth_m")

df

df_viz <- df %>%
  dtplyr::lazy_dt() %>%
  group_by(owd, depth_m) %>%
  summarise(mean_index_marcel = mean(index_marcel, na.rm = TRUE), n = n()) %>%
  as_tibble() %>%
  drop_na() %>%
  filter(depth_m <= 100)

df_viz <- df_viz %>%
  nest(data = everything())  %>%
  mutate(res = map(data, interpolate_2d, owd, depth_m, mean_index_marcel, h = 5))

res <- df_viz %>%
  unnest(res) %>%
  rename(owd = x, depth_m = y, mean_index_marcel = z) %>%
  select(-data) %>%
  drop_na(mean_index_marcel)

range(res$mean_index_marcel)

res %>%
  ggplot(aes(x = owd, y = depth_m, z = mean_index_marcel, fill = mean_index_marcel)) +
  geom_isobands(color = NA, breaks = seq(-5, 10, by = 0.05)) +
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
    title = "Marcel's index!",
    subtitle = "plot 2-D de log(bbp(532)/bbp(700)) / 0.274. Data from the Hydroscat."
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

ggsave(
  here::here("graphs/27_index_marcel.pdf"),
  device = cairo_pdf,
  height = 6,
  width = 8
)

# fs::file_info(here::here("graphs/27_index_marcel.pdf"))
