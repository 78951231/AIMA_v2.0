#' tidy up data to pass to interpolation function.
#' it call function interpolation_v2.0 for interpolation purpose.

preprocess_v2.0 = function(data, rate, interpolation_time, trial_length){

  BaselineData_preprocessed = tibble()
  TiltData_preprocessed = tibble()

  # extracts BaselineData from the object.
  BaselineDataOutRem_withNoRepetitions = data[[1]]
  No_of_BaselineRepetitions = BaselineDataOutRem_withNoRepetitions$No_of_BaselineRepetitions
  BaselineOutRemData = BaselineDataOutRem_withNoRepetitions$BaselineData_OutliersFree
  No_of_Channels = length(BaselineOutRemData) / No_of_BaselineRepetitions

  # extracts TiltData from the object.
  TiltDataOutRem_withNoRepetitions = data[[2]]
  No_of_TiltRepetitions = TiltDataOutRem_withNoRepetitions$No_of_TiltRepetitions
  TiltOutRemData = TiltDataOutRem_withNoRepetitions$TiltData_OutliersFree


  # pre-process the outliers-removed BaselineData
  Channels_for_oneSegment = 1:No_of_Channels
  for (SegmentNo in 1:No_of_BaselineRepetitions) {
    Baseline_SegmentData = as_tibble(BaselineOutRemData[Channels_for_oneSegment])
    Channels_for_oneSegment = Channels_for_oneSegment + No_of_Channels

    # pre-process one segment of data.
    Baseline_preprocessedData = interpolation_v2.0(Baseline_SegmentData, rate, interpolation_time, trial_length)
    Baseline_preprocessedData = mutate(Baseline_preprocessedData, condition = paste("Baseline", SegmentNo, sep = '_'))
    BaselineData_preprocessed = append(BaselineData_preprocessed, Baseline_preprocessedData)
  }

  # pre-process the outliers-removed TiltData
  Channels_for_oneSegment = 1:No_of_Channels
  for (SegmentNo in 1:No_of_TiltRepetitions) {
    Tilt_SegmentData = as_tibble(TiltOutRemData[Channels_for_oneSegment])
    Channels_for_oneSegment = Channels_for_oneSegment + No_of_Channels

    # pre-process one segment of data.
    Tilt_OutRemData = interpolation_v2.0(Tilt_SegmentData, rate, interpolation_time, trial_length)
    Tilt_OutRemData = mutate(Tilt_OutRemData, condition = paste("Tilt", SegmentNo, sep = '_'))
    TiltData_preprocessed = append(TiltData_preprocessed, Tilt_OutRemData)
  }

  BaselineDataPreprocsd_withNoRepetitions = lst(No_of_BaselineRepetitions, BaselineData_preprocessed)
  TiltDataOutPreprocsd_withNoRepetitions = lst(No_of_TiltRepetitions, TiltData_preprocessed)
  Baseline_Tilt_Data_Preprocsd = lst(BaselineDataPreprocsd_withNoRepetitions,
                                     TiltDataOutPreprocsd_withNoRepetitions)

  return(Baseline_Tilt_Data_Preprocsd)

}
