#' Calculate average vertical profiles
#'
#' @param df Dataframe with the data
#' @param var Which variable to average
#' @param breaks Breaks used to bin the data
#'
#' @return A dataframe with the averaged profiles
average_vertical_profiles <- function(df, var, breaks) {
  res <- df %>%
    drop_na({{ var }}) %>%
    mutate(owd_bin = santoku::chop(owd, breaks, close_end = TRUE)) %>%
    group_by(depth_m, owd_bin) %>%
    summarise({{ var }} := mean({{ var }}, na.rm = TRUE))

  return(res)
}
