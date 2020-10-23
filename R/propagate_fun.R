propagate_vertically <- function(df, x, y, depth_m) {
  x1 <- df %>%
    pull({{ x }})

  y1 <- df %>%
    pull({{ y }})

  res <- approx(x1, y1, xout = depth_m, rule = 2) %>%
    as_tibble() %>%
    select(
      {{ y }} := y
    )

  return(res)
}
