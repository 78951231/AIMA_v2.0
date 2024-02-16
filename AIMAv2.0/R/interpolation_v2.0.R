#' prepares data for linear interpolation by calling a function linear_interp_v2.0'.

interpolation_v2.0 = function(data, rate, interpolation_time, trial_length){
  ## check that all the columns are numeric
  x = data %>% dplyr::select(SBP)
  if (!all(map_lgl(x, is.numeric))) {
    stop(
      "Make sure your input structure is correct. You should not have
      character strings except for the comments column"
    )
  }
  ## check to make sure we don't have any NAs
  if (any(is.na(x))) {
    stop(
      "Double check your data, there appears to be some NA values.
      These can be fixed by running the function `clean_artifacts()`"
    )
  }
  # remove comments column
  data %<>%
    mutate(time = seq_len(nrow(.))) %>%
    dplyr::select(-Comments)
  ## interpolate data ; see description provided in header for further details.
  if (interpolation_time) {
    data %<>%
      linear_interp_v2.0(data.frame(x = seq(1, (trial_length * 60)))) %>%
      mutate(time = seq_len(nrow(.)))
  }
  return(data)
}
