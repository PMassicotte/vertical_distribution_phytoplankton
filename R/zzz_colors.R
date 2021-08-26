is_open_water <- function(owd) {
  ifelse(owd >= 0, TRUE, FALSE)
}

owd_colors <- list(
  "TRUE" = "#023047",
  "FALSE" = "#00bcd7"
)

lm_color <- "#d64933"


scale_color_owd <- function() {

  scale_color_manual(
    values = owd_colors,
    labels = c("Ice covered", "Open water"),
    guide = guide_legend(
      override.aes = list(alpha = 1, size = 3),
      title = element_blank(),
      title.position = "top",
      title.theme = element_text(size = 8, family = "Poppins"),
      label.theme = element_text(size = 10, family = "Poppins"),
      ncol = 1
    )
  )

}
