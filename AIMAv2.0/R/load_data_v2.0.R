# Loads txt/CSV file exported from labchart.
# Uses 'file_name' to determine type of file.
# Uses load_labchartCSV_v2.0() to load CSV file OR uses load_labchartTXT_v2.0() to load text file.
# If file is not txt/CSV, an error message will pop up and execution will stop.

load_data_v2.0 = function(file_name){
  if(str_detect(file_name, "txt")){
  # source("load_labchartTXT_v2.0.R")
  file_data = suppressWarnings(
                              file_name %>%
                              load_labchartTXT_v2.0()
                              )
  } else if (str_detect(file_name, "CSV")){
  # source("load_labchartCSV_v2.0.R")
  file_data = suppressWarnings(
                              file_name %>%
                              load_labchartCSV_v2.0()
                              )

  } else {
  print("ERROR: Data file is not compatible with the AIMAv2.0 package!")
  stop()
  }
  return(file_data)
}
