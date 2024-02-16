# Detects and removes of outliers using PCA.
# the required object for this function is the output of clean_artifacts_v2.0()
# this function calls internally a function detect_outliers_v.2.0()

remove_outliers_v2.0 = function(data, variable, rate, bin_length){

  BaselineData_OutliersFree = tibble()
  TiltData_OutliersFree = tibble()

  # extracts BaselineData from the object coming out of the function 'extract_data_segments_v2.0.R'
  BaselineDataArtFree_withNoRepetitions = data[[1]]
  No_of_BaselineRepetitions = BaselineDataArtFree_withNoRepetitions$No_of_BaselineRepetitions
  BaselineArtFreeData = BaselineDataArtFree_withNoRepetitions$BaselineData_ArtifactsFree
  No_of_Channels = length(BaselineArtFreeData) / No_of_BaselineRepetitions

  # extracts TiltData from the object coming out of the function 'extract_data_segments_v2.0.R'
  TiltDataArtFree_withNoRepetitions = data[[2]]
  No_of_TiltRepetitions = TiltDataArtFree_withNoRepetitions$No_of_TiltRepetitions
  TiltArtFreeData = TiltDataArtFree_withNoRepetitions$TiltData_ArtifactsFree

  # Outliers removal from artifacts-free BaselineData
  Channels_for_oneSegment = 1:No_of_Channels
  for (SegmentNo in 1:No_of_BaselineRepetitions) {
    Baseline_SegmentData = as_tibble(BaselineArtFreeData[Channels_for_oneSegment])
    Channels_for_oneSegment = Channels_for_oneSegment + No_of_Channels

    # outliers removal from one segment of baseline data.
    Baseline_OutRemData = detect_outliers_v2.0(Baseline_SegmentData, variable, rate, bin_length)
    BaselineData_OutliersFree = append(BaselineData_OutliersFree, Baseline_OutRemData)
  }

  # Outliers removal from artifacts-free TiltData
  Channels_for_oneSegment = 1:No_of_Channels
  for (SegmentNo in 1:No_of_TiltRepetitions) {
    Tilt_SegmentData = as_tibble(TiltArtFreeData[Channels_for_oneSegment])
    Channels_for_oneSegment = Channels_for_oneSegment + No_of_Channels

    # outliers removal from one segment of tilt data.
    Tilt_OutRemData = detect_outliers_v2.0(Tilt_SegmentData, variable, rate, bin_length)
    TiltData_OutliersFree = append(TiltData_OutliersFree, Tilt_OutRemData)
  }

  BaselineDataOutRem_withNoRepetitions = lst(No_of_BaselineRepetitions, BaselineData_OutliersFree)
  TiltDataOutRem_withNoRepetitions = lst(No_of_TiltRepetitions, TiltData_OutliersFree)
  Baseline_Tilt_Data_OutRem = lst(BaselineDataOutRem_withNoRepetitions,
                                  TiltDataOutRem_withNoRepetitions)

  return(Baseline_Tilt_Data_OutRem)
}
