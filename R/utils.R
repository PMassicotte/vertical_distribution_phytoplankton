#' Calculate average vertical profiles
#'
#' @param df Dataframe with the data
#' @param var Which variable to average
#' @param nprofiles How many profiles do we want
#'
#' @return A dataframe with the averaged profiles
average_vertical_profiles <- function(df, var, nprofiles) {
  res <- df %>%
    drop_na({{ var }}) %>%
    mutate(owd_bin = santoku::chop_evenly(owd, nprofiles)) %>%
    group_by(depth_m, owd_bin) %>%
    summarise({{ var }} := mean({{ var }}, na.rm = TRUE))

  return(res)
}
