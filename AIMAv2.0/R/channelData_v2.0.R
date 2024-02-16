#' extracts relevant channel data from all segments data, and calculates one mean value per condition, as well as delta tracing.

channelData_v2.0 = function(data, variable){

  preprocessedData = list_flatten(data)

  No_of_BaselineRepetitions = flatten_int(preprocessedData[1])
  No_of_TiltRepetitions = flatten_int(preprocessedData[3])
  BaselineData = flatten(preprocessedData[2])
  TiltData = flatten(preprocessedData[4])

  Channels_in_BaselineData = names(BaselineData)
  locateChannel_in_BaselineData = which(x = str_detect(Channels_in_BaselineData, variable))

  Channels_in_TiltData = names(TiltData)
  locateChannel_in_TiltData = which(x = str_detect(Channels_in_TiltData, variable))

  # locates relevant channel in the data.
  selectChannel_in_BaselineData = tibble()
  for (i in locateChannel_in_BaselineData) {
    BaselineData_Channel = BaselineData[i]
    selectChannel_in_BaselineData = append(selectChannel_in_BaselineData, BaselineData_Channel)
  }
  meanValuesChannel_BaselineData = rowMeans(as.data.frame(selectChannel_in_BaselineData))
  meanValueChannel_BaselineData = mean(meanValuesChannel_BaselineData)

  # locates relevant channel in the data.
  selectChannel_in_TiltData = tibble()
  for (i in locateChannel_in_TiltData) {
    TiltData_Channel = TiltData[i]
    selectChannel_in_TiltData = append(selectChannel_in_TiltData, TiltData_Channel)
  }
  meanValuesChannel_TiltData = rowMeans(as.data.frame(selectChannel_in_TiltData))
  meanValueChannel_TiltData = mean(meanValuesChannel_TiltData)

  # delta tracing (e.g., deltaSBP).
  DemeanValuesChannel_BaselineTilt = ((
    append(meanValuesChannel_BaselineData, meanValuesChannel_TiltData))
      - mean(meanValuesChannel_BaselineData))

  channelSignal = lst(meanValueChannel_BaselineData, meanValueChannel_TiltData, DemeanValuesChannel_BaselineTilt)

  return(channelSignal)
}
