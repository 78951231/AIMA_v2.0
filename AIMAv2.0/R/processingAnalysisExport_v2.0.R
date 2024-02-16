#' this performs pre-processing, analysis, and final exports data in the excel sheets.
#' calls preprocess_v2.0 for pre-processing.
#' calls analyze_data_v2.0 for analysis.
#' calls export_data_v2.0 to export data to xlsx files.

processingAnalysisExport_v2.0 = function(data, rate, interpolation_time, trial_length, myFile){
  Baseline_Tilt_Data_Preprocsd = preprocess_v2.0(data, rate, interpolation_time, trial_length)
  Baseline_Tilt_Data_Analyzed = analyze_data_v2.0(Baseline_Tilt_Data_Preprocsd)
  data = export_data_v2.0(Baseline_Tilt_Data_Analyzed, myFile)

  return(Baseline_Tilt_Data_Preprocsd)

}
