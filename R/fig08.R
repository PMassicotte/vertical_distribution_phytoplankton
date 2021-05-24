# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore how the different classes of particle size change over
# the time.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Read the UVP data -------------------------------------------------------

uvp <- vroom::vroom(here("data","clean","uvp_small_medium_large_class_size.csv"))

uvp

uvp_wide <- uvp %>%
  dtplyr::lazy_dt() %>%
  filter(depth_m <= 100) %>%
  group_by(station, owd, depth_m, particle_size_range) %>%
  summarise(count_per_liter = mean(count_per_liter, na.rm = TRUE)) %>%
  as_tibble() %>%
  pivot_wider(names_from = particle_size_range, values_from = count_per_liter) %>%
  rowwise() %>%
  mutate(total_particle_count = sum(c_across(matches(" mm$")))) %>%
  ungroup()

unique(uvp_wide$depth_m)

# Average contribution as OWD increase ------------------------------------

df_viz <- uvp_wide %>%
  select(-total_particle_count) %>%
  group_by(depth_m, owd) %>%
  summarise(across(matches(" mm$"), ~ mean(., na.rm = TRUE))) %>%
  arrange(depth_m, owd) %>%
  rowwise() %>%
  mutate(total_particle_count = sum(c_across(matches(" mm$")))) %>%
  mutate(across(matches(" mm$"), ~ . / total_particle_count)) %>%
  pivot_longer(matches(" mm$")) %>%
  filter(depth_m <= 30) %>%
  mutate(depth_label = glue("{depth_m} m"), .after = "depth_m") %>%
  mutate(depth_label = fct_reorder(depth_label, depth_m))

df_viz

# Plot --------------------------------------------------------------------

# Make nice labels
df_viz <- df_viz %>%
  mutate(
    class_label = case_when(
      name == "0.102-0.323 mm" ~ "0.102-0.323 mm (small)",
      name == "0.323-1.02 mm" ~ "0.323-1.02 mm (medium)",
      name == "1.02-26 mm" ~ "1.02-26 mm (large)",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(class_label = factor(
    class_label,
    levels = c(
      "0.102-0.323 mm (small)",
      "0.323-1.02 mm (medium)",
      "1.02-26 mm (large)"
    )
  )) %>%
  mutate(class_label = fct_rev(class_label))

p <- df_viz %>%
  ggplot(aes(x = owd, y = value, fill = class_label)) +
  geom_area() +
  facet_wrap(~depth_label, scales = "free_x") +
  scale_x_continuous(expand = c(0, 0), breaks = scales::breaks_pretty(n = 8)) +
  scale_y_continuous(expand = c(0, 0), labels = scales::label_percent()) +
  # scale_fill_viridis_d(option = "D", direction = -1) +
  paletteer::scale_fill_paletteer_d(
    "ggthemes::wsj_rgby"
  ) +
  labs(
    x = "Number of open water days (OWD)",
    y = "Relative contribution"
  ) +
  theme(
    legend.position = "top",
    panel.border = element_blank(),
    legend.title = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  here("graphs","fig08.pdf"),
  device = cairo_pdf,
  width = 7.15,
  height = 5.21
)
