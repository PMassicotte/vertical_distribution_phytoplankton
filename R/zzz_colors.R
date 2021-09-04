is_open_water <- function(owd) {
  ifelse(owd >= 0, "open", "close")
}

owd_colors <- list(
  "open" = "#023047",
  "close" = "#00bcd7"
)

owd_labels <- list(
  "open" = "Open water",
  "close" = "Ice covered"
)

lm_color <- "#d64933"


scale_color_owd <- function(ncol = 1) {

  scale_color_manual(
    values = owd_colors,
    labels = owd_labels,
    guide = guide_legend(
      override.aes = list(alpha = 1, size = 3),
      title = element_blank(),
      title.position = "top",
      title.theme = element_text(size = 8, family = "Poppins"),
      label.theme = element_text(size = 10, family = "Poppins"),
      ncol = ncol
    )
  )

}

scale_depth_continuous <- function(nbreaks = 6, min_size = 1, max_size = 4) {

  scale_size_continuous(
    breaks = scales::breaks_pretty(n = nbreaks),
    range = c(min_size, max_size),
    guide = guide_legend(
      nrow = 1,
      title.position = "top",
      title = "Depth (m)",
      title.theme = element_text(
        size = 10,
        family = "Poppins",
        margin = margin(t = 8),
        hjust = 0.5),
      label.theme = element_text(size = 10, family = "Poppins")
    )
  )

}
