#' analyze each segment of data by calling an internal function analyze_segment().

analyze_data_v2.0 = function(data){

  BaselineData_analysis = tibble()
  TiltData_analysis = tibble()

  # extracts data from the object.
  BaselineDataPreprocsd_withNoRepetitions = data[[1]]
  No_of_BaselineRepetitions = BaselineDataPreprocsd_withNoRepetitions$No_of_BaselineRepetitions
  BaselinePreprcsdData = BaselineDataPreprocsd_withNoRepetitions$BaselineData_preprocessed
  No_of_Channels = length(BaselinePreprcsdData) / No_of_BaselineRepetitions

  # extracts data from the object.
  TiltDataPreprocsd_withNoRepetitions = data[[2]]
  No_of_TiltRepetitions = TiltDataPreprocsd_withNoRepetitions$No_of_TiltRepetitions
  TiltPreprcsdData = TiltDataPreprocsd_withNoRepetitions$TiltData_preprocessed

  # internal function to analyze one segment of data.
  analyze_segment = function(data){
    # check for condition
    if (is.null(data$condition)) {
      data %<>% mutate(condition = '1')
    }
    # summarise the data
    sum = data %>%
      dplyr::select(-time, -index) %>%
      gather(variable, value, -condition) %>%
      group_by(condition, variable) %>%
      summarise(mean = mean(value), min = min(value), max = max(value)) %>%
      ungroup()
    return(sum)
  }

  # analyze the pre-processed BaselineData
  Channels_for_oneSegment = 1:No_of_Channels
  for (SegmentNo in 1:No_of_BaselineRepetitions) {
    Baseline_SegmentData = as_tibble(BaselinePreprcsdData[Channels_for_oneSegment])
    Channels_for_oneSegment = Channels_for_oneSegment + No_of_Channels

    # analyze one segment of data.
    Baseline_analysisResults = analyze_segment(Baseline_SegmentData)
    BaselineData_analysis = rbind(BaselineData_analysis, Baseline_analysisResults)
  }

  # analyze the pre-processed TiltData
  Channels_for_oneSegment = 1:No_of_Channels
  for (SegmentNo in 1:No_of_TiltRepetitions) {
    Tilt_SegmentData = as_tibble(TiltPreprcsdData[Channels_for_oneSegment])
    Channels_for_oneSegment = Channels_for_oneSegment + No_of_Channels

    # analyze one segment of data.
    Tilt_analysisResults = analyze_segment(Tilt_SegmentData)
    TiltData_analysis = rbind(TiltData_analysis, Tilt_analysisResults)
  }

  Baseline_Tilt_Data_Analyzed = rbind(BaselineData_analysis, TiltData_analysis)

  return(Baseline_Tilt_Data_Analyzed)
}
