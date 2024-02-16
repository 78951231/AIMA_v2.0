# Extract data segments as per the experimental 'Conditions' from the file_data (loaded from the txt/CSV file).

extract_data_segments_v2.0 = function(file_data, conditionNames){

  BaselineData = tibble()
  TiltData = tibble()
  RowNos_for_BaselineData = str_which(file_data$Comments, paste0(conditionNames[1],"_"))
  RowNos_for_TiltData = str_which(file_data$Comments, paste0(conditionNames[2],"_"))

  if(is_empty(RowNos_for_BaselineData)){
    print("No Baseline data segment is found!")
  } else {
    Rows_with_BaselineComments = slice(file_data, RowNos_for_BaselineData)
    RowNos_for_BaselineData = matrix(RowNos_for_BaselineData, nrow = 2)
    No_of_BaselineRepetitions = ncol(RowNos_for_BaselineData)
    for (SegmentNo in 1:ncol(RowNos_for_BaselineData)) {
      print(SegmentNo)
      BaselineData_Segment = as_tibble(slice(file_data, RowNos_for_BaselineData[1,SegmentNo]:RowNos_for_BaselineData[2,SegmentNo]))
      BaselineData = append(BaselineData, BaselineData_Segment)
    }
  }

  if(is_empty(RowNos_for_TiltData)){
    print("No Tilt data segment is found!")
  } else {
    Rows_with_TiltComments = slice(file_data, RowNos_for_TiltData)
    RowNos_for_TiltData = matrix(RowNos_for_TiltData, nrow = 2)
    No_of_TiltRepetitions = ncol(RowNos_for_TiltData)
    for (SegmentNo in 1:ncol(RowNos_for_TiltData)){
      print(SegmentNo)
      TiltData_Segment = as_tibble(slice(file_data, RowNos_for_TiltData[1,SegmentNo]:RowNos_for_TiltData[2,SegmentNo]))
      TiltData = append(TiltData, TiltData_Segment)
    }
  }

  BaselineData[] <- lapply(BaselineData, function(x) as.numeric(as.character(x)))
  TiltData[] <- lapply(TiltData, function(x) as.numeric(as.character(x)))

  BaselineData_withNoRepetitions = lst(No_of_BaselineRepetitions, BaselineData)
  TiltData_withNoRepetitions = lst(No_of_TiltRepetitions, TiltData)
  Baseline_Tilt_Data = lst(BaselineData_withNoRepetitions, TiltData_withNoRepetitions)

  return(Baseline_Tilt_Data)
}
