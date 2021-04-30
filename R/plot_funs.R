#' 3D surface ggplot
#'
#' @param df Dataframe with the data
#' @param x x variable
#' @param y y variable (depth)
#' @param z color variable
#' @param iso_breaks How many brakes we want
#' @param fill_text Text to display over the colorbar
#' @param isolume Datframe with the isolume data
#' @param nbreaks How many breaks for the isobands
#'
#' @return A ggplot2
gg3d <- function(df, x, y, z, iso_breaks, fill_text, isolume, nbreaks, trans_fun = "sqrt") {
  df %>%
    ggplot(aes(x = {{ x }}, y = {{ y }}, z = {{ z }}, fill = {{ z }}
    )) +
    geom_isobands(color = NA, breaks = iso_breaks) +
    paletteer::scale_fill_paletteer_c(
      "oompaBase::jetColors",
      trans = trans_fun,
      breaks = scales::breaks_pretty(n = nbreaks),
      guide = guide_colorbar(
        title.hjust = 0.5,
        title.position = "top",
        barwidth = unit(8, "cm"),
        barheight = unit(0.2, "cm")
      )
    ) +
    scale_y_reverse(
      expand = c(0, 0),
      breaks = seq(0, 100, by = 10)
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0.01, 0.05)),
      breaks = scales::breaks_pretty(n = 8)
    ) +
    geom_line(
      data = isolume,
      size = 1,
      lineend = "round",
      color = "red",
      aes(x = owd, y = depth_m),
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    labs(
      y = "Depth (m)",
      x = NULL,
      fill = parse(text = fill_text)
    ) +
    theme(
      panel.grid = element_line(color = "gray60", size = 0.1),
      panel.background = element_rect(fill = NA),
      panel.ontop = TRUE,
      legend.position = "top",
      strip.background = element_blank(),
      strip.text = element_text(
        hjust = 0,
        size = 10,
        face = "bold"
      ),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      legend.title = element_text(size = 8)
    )
}


#' Plot 2D vertical averaged profiles
#'
#' @param df Dataframe with the data
#' @param x x variable
#' @param y y variable (depth)
#' @param bin bin variable (used for the color classes)
#'
#' @return A ggplot2
gg2dprofiles <- function(df, x, y, bin) {

  p <- df %>%
    ggplot(aes(x = {{ x }}, y = {{ y }}, color = {{ bin }})) +
    geom_path() +
    scale_y_reverse(
      expand = c(0, 0),
      breaks = seq(0, 100, by = 10)
    ) +
    labs(
      y = NULL,
      x = NULL
    ) +
    paletteer::scale_color_paletteer_d(
      "jcolors::pal3",
      guide = guide_legend(
        label.position = "top",
        keywidth = unit(1, "cm"),
        override.aes = list(size = 2)
      )
    ) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      panel.border = element_blank(),
      axis.ticks = element_blank()
    )


  return(p)
}
