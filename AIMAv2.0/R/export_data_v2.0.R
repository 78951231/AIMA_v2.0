#' export the data into one xlsx file per patient.
#' Two options to export data:
#'     1. write_xlsx() : it only works if JVM is installed.
#'    2. rio::export()

export_data_v2.0 = function(data, myFile) {
  excelFileName = str_replace(myFile, ".txt", " AIMA analysis.xlsx")
  rio::export(x = data, file = excelFileName)

  # #write_xlsx() works only if JVM is installed.
  # write_xlsx(chronic_results, file = "AIMA results.xlsx", sheetName = str_replace(my.files[i], ".txt", ""), append = TRUE)
  # write.xlsx(analyzedData2, "AIMA.xlsx", sep = "\t")

  return(data)
}
