# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Compare POC with other bio-optical parameters.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz_colors.R"))

# Do not forget to convert umol L to mg m3.
# 1 molC = 12 gC
poc <- read_csv(here("data","clean","poc.csv")) %>%
  select(station, transect, depth_m, poc_umol_l) %>%
  mutate(poc_mg_m3 = ((poc_umol_l / 0.000001) * 12) / 1000000)

# Will use this to have the same scale on the y-axes

min_poc <- min(poc$poc_mg_m3, na.rm = TRUE) * 0.5
max_poc <- max(poc$poc_mg_m3, na.rm = TRUE) * 1.5

# POC vs CP ---------------------------------------------------------------

ctd <- read_csv(here("data","clean","ctd.csv")) %>%
  select(station, transect, depth_m, owd, cp, flor_mg_m3) %>%
  mutate(depth_ctd_m = depth_m)

setDT(ctd)
setDT(poc)

poc_cp <- ctd[poc, roll = "nearest", on = .(station, transect, depth_m)]

poc_cp <- poc_cp %>%
  as_tibble() %>%
  filter(abs(depth_m - depth_ctd_m) <= 1) %>%
  mutate(is_open_water = is_open_water(owd)) %>%
  filter(depth_m <= 100)

p1 <- poc_cp %>%
  ggplot(aes(x = cp, y = poc_mg_m3)) +
  geom_point(aes(color = is_open_water)) +
  ggpubr::stat_regline_equation(label.y.npc = 1) +
  ggpubr::stat_regline_equation(label.y.npc = 0.93, aes(label = ..rr.label..)) +
  scale_y_log10(limits = c(min_poc, max_poc)) +
  scale_x_log10() +
  annotation_logticks(size = 0.1) +
  scale_color_owd() +
  labs(
    x = quote(C[p]~(657)~(m^{-1})),
    y = quote("POC"~(mg~m^{-3}))
  ) +
  geom_smooth(method = "lm", color = lm_color, size = 0.5) +
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.9, 0.1),
    legend.background = element_blank(),
    legend.key = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

# Note: Exponentiate regression coefficient.
# https://daviddalpiaz.github.io/appliedstats/transformations.html Bear in mind
# that the equation of a line in the log-log plane is not y=m⋅x+b, but rather
# logy=m⋅logx+b.
# https://www.wolframalpha.com/input/?i=log10%28y%29+%3D+a+%2B+b+*log10%28x%29+isolate+y
# y = 10^a * x^b where a and b are the intercept and the slope
tibble(
  x = seq(0.006668996, 1.000743797, length.out = 100),
  y = 10^2.9 * x^0.74
) %>%
  ggplot(aes(x, y)) +
  geom_point()+
  scale_x_log10() +
  scale_y_log10(limits = c(min_poc, max_poc)) +
  annotation_logticks(sides = "l")

poc_cp %>%
  mutate(pred = 10^2.9 * cp^0.74) %>%
  ggplot(aes(x = cp, y = poc_mg_m3)) +
  geom_point(color = "#393E41") +
  geom_line(aes(y = pred), color = lm_color, size = 0.5)

# POC vs chla -------------------------------------------------------------

p2 <- poc_cp %>%
  ggplot(aes(x = flor_mg_m3, y = poc_mg_m3)) +
  geom_point(aes(color = is_open_water)) +
  ggpubr::stat_regline_equation(label.y.npc = 1) +
  ggpubr::stat_regline_equation(label.y.npc = 0.93, aes(label = ..rr.label..)) +
  scale_y_log10(limits = c(min_poc, max_poc)) +
  scale_x_log10() +
  annotation_logticks(size = 0.1) +
  scale_color_owd() +
  labs(
    x = quote("Chlorophyll-a"~(mg~m^{-3})),
    y = quote("POC"~(mg~m^{-3}))
  ) +
  geom_smooth(method = "lm", color = lm_color, size = 0.5) +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

# POC vs CP ---------------------------------------------------------------

poc

hydroscat <- read_csv(here("data","clean","hydroscat.csv")) %>%
  filter(wavelength == 470) %>%
  rename(depth_m = depth) %>%
  mutate(depth_hydroscat_m = depth_m)

setDT(hydroscat)

poc_bbp <- hydroscat[poc, roll = "nearest", on = .(station, transect, depth_m)]

poc_bbp <- poc_bbp %>%
  as_tibble() %>%
  filter(abs(depth_m - depth_hydroscat_m) <= 1) %>%
  mutate(is_open_water = is_open_water(owd)) %>%
  filter(depth_m <= 100)

# https://bg.copernicus.org/preprints/bg-2021-123/bg-2021-123.pdf
p3 <- poc_bbp %>%
  # mutate(stramski_pred_poc = 71002 * bbp - 5.5) %>%
  # mutate(loisel_pred_poc = 37550 * bbp + 1.3) %>%
  # mutate(cetinic_pred_poc = 35422 * bbp - 14.4) %>%
  ggplot(aes(x = bbp, y = poc_mg_m3)) +
  geom_point(aes(color = is_open_water)) +
  # geom_line(aes(y = stramski_pred_poc, color = "Stramski (2008)")) +
  # geom_line(aes(y = loisel_pred_poc, color = "Loisel (2011)")) +
  # geom_line(aes(y = cetinic_pred_poc, color = "Cetinic (2012)")) +
  ggpubr::stat_regline_equation(label.y.npc = 1) +
  ggpubr::stat_regline_equation(label.y.npc = 0.93, aes(label = ..rr.label..)) +
  scale_y_log10(limits = c(min_poc, max_poc)) +
  scale_x_log10(labels = scales::label_number()) +
  annotation_logticks(size = 0.1) +
  scale_color_owd() +
  labs(
    x = quote(b[bp]~(470)~(m^{-1})),
    y = quote("POC"~(mg~m^{-3}))
  ) +
  geom_smooth(method = "lm", color = lm_color, size = 0.5) +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

# Combine plots -----------------------------------------------------------

p <- p1 / p2 + p3 +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here("graphs","fig07.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 12
)

# Correlation statistics --------------------------------------------------

cor(log10(poc_cp$cp), log10(poc_cp$poc_mg_m3), "complete.obs")
cor(log10(poc_cp$flor_mg_m3), log10(poc_cp$poc_mg_m3), "complete.obs")
cor(log10(poc_bbp$bbp), log10(poc_bbp$poc_mg_m3), "complete.obs")

# POC vs bbp at all wavelengths -------------------------------------------

# poc <- read_csv(here("data","clean","poc.csv")) %>%
#   select(station, transect, depth_m, poc_umol_l) %>%
#   mutate(poc_mg_m3 = ((poc_umol_l / 0.000001) * 12) / 1000000)
#
# setDT(poc)
#
# hydroscat <- read_csv(here("data","clean","hydroscat.csv")) %>%
#   rename(depth_m = depth) %>%
#   mutate(depth_hydroscat_m = depth_m)
#
# setDT(hydroscat)
#
# # https://github.com/Rdatatable/data.table/issues/2444
# hydroscat[poc, roll = "nearest", on = .(station, transect, depth_m), mult = "all"] %>%
#   filter(station == 100)
#
# poc_bbp %>%
#   count(station, transect, depth_m) %>%
#   assertr::verify(n == 6)
#
# poc_bbp <- poc_bbp %>%
#   as_tibble() %>%
#   filter(abs(depth_m - poc_depth_m) <= 1) %>%
#   filter(depth_m <= 100)
#
# # The vertical resolution of the hydroscat is much higher (every 1 m) than that
# # of the POC. Therefore, for 1 given POC measurement at 1 particular depth, it
# # is possible to have more than 1 bbp measurement.
#
# poc_bbp %>%
#   ggplot(aes(x = bbp, y = poc_mg_m3)) +
#   geom_point() +
#   scale_x_log10() +
#   scale_y_log10() +
#   facet_wrap(~wavelength)
#
# # For example, at station 100, there are two values of bbp394 associated to 1
# # single value of POC. This should be averaged to have only 1 bbp value per POC
# # value. This will results in small differences compared to what is seen in Fig.
# # 7.
#
# poc_bbp %>%
#   count(station, poc_depth_m) %>%
#   slice(2) %>%
#   semi_join(poc_bbp, .)
#
# poc_bbp <- poc_bbp %>%
#   group_by(station, wavelength, poc_depth_m) %>%
#   summarise(across(c(poc_mg_m3, bbp), ~mean(., na.rm = TRUE))) %>%
#   ungroup()
#
# poc_bbp %>%
#   ggplot(aes(x = bbp, y = poc_mg_m3)) +
#   geom_point(color = "#393E41") +
#   scale_x_log10(labels = scales::label_number()) +
#   scale_y_log10() +
#   annotation_logticks(size = 0.1) +
#   geom_smooth(method = "lm", color = lm_color, size = 0.5) +
#   ggpubr::stat_regline_equation(label.y.npc = 1) +
#   ggpubr::stat_regline_equation(label.y.npc = 0.90, aes(label = ..rr.label..)) +
#   labs(
#     x = quote(b[bp]~(m^{-1})),
#     y = quote("POC"~(mg~m^{-3}))
#   ) +
#   facet_wrap(~glue("{wavelength} nm"), scales = "free_x") +
#   theme(
#     panel.border = element_blank(),
#     axis.ticks = element_blank()
#   )
