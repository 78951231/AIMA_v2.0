# cleans data by removing the artifacts.

clean_artifacts_v2.0 = function(data, n_sd = 10){
  BaselineData_ArtifactsFree= tibble()
  TiltData_ArtifactsFree = tibble()

  # setup an internal function to clean up each column
  clean_col = function(x, n_sd) {
    if (is.numeric(x)) {
      dvec = c(0,diff(x, 1))
      outliers = which(abs(dvec) > n_sd*(sd(dvec)))
      x[outliers] = NA
      return(x)
    }
  }

  # extracts BaselineData from the list coming out of the function 'extract_data_segments_v2.0.R'
  BaselineData_withNoRepetitions = data[[1]]
  No_of_BaselineRepetitions = BaselineData_withNoRepetitions$No_of_BaselineRepetitions
  BaselineData = BaselineData_withNoRepetitions$BaselineData
  No_of_Channels = length(BaselineData)/No_of_BaselineRepetitions

  # extracts TiltData from the list coming out of the function 'extract_data_segments_v2.0.R'
  TiltData_withNoRepetitions = data[[2]]
  No_of_TiltRepetitions = TiltData_withNoRepetitions$No_of_TiltRepetitions
  TiltData= TiltData_withNoRepetitions$TiltData

  # clean up the BaselineData
  Channels_for_oneSegment = 1:No_of_Channels
  for (SegmentNo in 1:No_of_BaselineRepetitions) {
    data = as_tibble(BaselineData[Channels_for_oneSegment])
    Channels_for_oneSegment = Channels_for_oneSegment+No_of_Channels

    # grab the comments and time to avoid interpolation issues
    Comments = data$Comments
    time = data$time
    Channel_4 = data$Channel_4
    data %<>% dplyr::select(-Comments,-time, -Channel_4)

    Baseline_CleanData = data %>%
      # make sure we don't have any NaNs
      mutate_all(function(x) ifelse(is.nan(x), NA, x)) %>%
      # clean all the cols
      mutate_all( clean_col, n_sd) %>%
      # do a two part NA removal
      na.locf(na.rm = F) %>%
      na.locf(na.rm = T, fromLast = T) %>%
      mutate(Comments = Comments, time = time)

    BaselineData_ArtifactsFree = append(BaselineData_ArtifactsFree, Baseline_CleanData)
  }

  # clean up the TiltData
  Channels_for_oneSegment = 1:No_of_Channels
  for (SegmentNo in 1:No_of_TiltRepetitions) {
    data = as_tibble(TiltData[Channels_for_oneSegment])
    Channels_for_oneSegment = Channels_for_oneSegment+No_of_Channels

    # grab the comments and time to avoid interpolation issues
    Comments = data$Comments
    time = data$time
    Channel_4 = data$Channel_4
    data %<>% dplyr::select(-Comments, -time, -Channel_4)

    Tilt_CleanData = data %>%
      # make sure we don't have any NaNs
      mutate_all(function(x) ifelse(is.nan(x), NA, x)) %>%
      # clean all the cols
      mutate_all( clean_col, n_sd) %>%
      # do a two part NA removal
      na.locf(na.rm = F) %>%
      na.locf(na.rm = T, fromLast = T) %>%
      mutate(Comments = Comments, time = time)

    TiltData_ArtifactsFree = append(TiltData_ArtifactsFree, Tilt_CleanData)
  }

  BaselineDataArtFree_withNoRepetitions = lst(No_of_BaselineRepetitions, BaselineData_ArtifactsFree)
  TiltDataArtFree_withNoRepetitions = lst(No_of_TiltRepetitions, TiltData_ArtifactsFree)
  Baseline_Tilt_Data_ArtFree = lst(BaselineDataArtFree_withNoRepetitions, TiltDataArtFree_withNoRepetitions)

  return(Baseline_Tilt_Data_ArtFree)
}
