# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Compare POC with other bio-optical parameters.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

poc <- read_csv(here("data","clean","poc.csv")) %>%
  select(station, transect, depth_m, poc_umol_l)

# Will use this to have the same scale on the y-axes

min_poc <- min(poc$poc_umol_l, na.rm = TRUE) * 0.5
max_poc <- max(poc$poc_umol_l, na.rm = TRUE) * 1.5

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
  filter(depth_m <= 100)

p1 <- poc_cp %>%
  ggplot(aes(x = cp, y = poc_umol_l)) +
  geom_point(color = "#393E41") +
  ggpubr::stat_regline_equation(label.y = log10(80)) +
  ggpubr::stat_regline_equation(label.y = log10(50), aes(label = ..rr.label..)) +
  scale_y_log10(limits = c(min_poc, max_poc)) +
  scale_x_log10() +
  annotation_logticks(size = 0.1) +
  labs(
    x = quote(C[p]~(657)~(m^{-1})),
    y = quote("POC"~(mg~m^{-3}))
  ) +
  geom_smooth(method = "lm", color = "#bf1d28", size = 0.5) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

# Note: Exponentiate regression coefficient.
# https://daviddalpiaz.github.io/appliedstats/transformations.html
# https://www.wolframalpha.com/input/?i=log10%28y%29+%3D+a+%2B+b+*log10%28x%29+isolate+y
# y = 10^a * x^b where a and b are the intercept and the slope
tibble(
  x = seq(0.006668996, 1.000743797, length.out = 100),
  y = 10^1.9 * x^0.74
) %>%
  ggplot(aes(x, y)) +
  geom_point()+
  scale_x_log10() +
  scale_y_log10(limits = c(min_poc, max_poc)) +
  annotation_logticks(sides = "l")

poc_cp %>%
  mutate(pred = 10^1.9 * cp^0.74) %>%
  ggplot(aes(x = cp, y = poc_umol_l)) +
  geom_point(color = "#393E41") +
  geom_line(aes(y = pred), color = "#bf1d28", size = 0.5)

# POC vs chla -------------------------------------------------------------

p2 <- poc_cp %>%
  ggplot(aes(x = flor_mg_m3, y = poc_umol_l)) +
  geom_point(color = "#393E41") +
  ggpubr::stat_regline_equation(label.y = log10(80)) +
  ggpubr::stat_regline_equation(label.y = log10(50), aes(label = ..rr.label..)) +
  scale_y_log10(limits = c(min_poc, max_poc)) +
  scale_x_log10() +
  annotation_logticks(size = 0.1) +
  labs(
    x = quote("Chlorophyll-a"~(mg~m^{-3})),
    y = quote("POC"~(mg~m^{-3}))
  ) +
  geom_smooth(method = "lm", color = "#bf1d28", size = 0.5) +
  theme(
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
  filter(depth_m <= 100)

p3 <- poc_bbp %>%
  ggplot(aes(x = bbp, y = poc_umol_l)) +
  geom_point(color = "#393E41") +
  ggpubr::stat_regline_equation(label.y = log10(80)) +
  ggpubr::stat_regline_equation(label.y = log10(50), aes(label = ..rr.label..)) +
  scale_y_log10(limits = c(min_poc, max_poc)) +
  scale_x_log10(labels = scales::label_number()) +
  annotation_logticks(size = 0.1) +
  labs(
    x = quote(b[bp]~(470)~(m^{-1})),
    y = quote("POC"~(mg~m^{-3}))
  ) +
  geom_smooth(method = "lm", color = "#bf1d28", size = 0.5) +
  theme(
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

cor(log10(poc_cp$cp), log10(poc_cp$poc_umol_l), "complete.obs")
cor(log10(poc_cp$flor_mg_m3), log10(poc_cp$poc_umol_l), "complete.obs")
cor(log10(poc_bbp$bbp), log10(poc_bbp$poc_umol_l), "complete.obs")

