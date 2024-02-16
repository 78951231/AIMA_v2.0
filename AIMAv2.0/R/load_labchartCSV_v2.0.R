# Load CSV files exported from LabChart
#
# The use of "Split=" on strsplot() line is dependent how the CSV file was built using excel
# i.e., 'Text import wizard - Step 2 of 3'  -> Delimiters (if Tab is checked then split=';' OR
# if Semicolon/Comma is checked then split='\t' has to be.

load_labchartCSV_v2.0 = function(file_name)
{

  ### Extraction of channel names
  col_names     = c("Column1")
  col_names1    = read_csv(file_name, col_names=col_names, show_col_types = FALSE)
  col_names2    = col_names1[5,]
  ChannelNames1 = strsplit(col_names2$Column1, split=';')
  ChannelNames2 = unlist(ChannelNames1)
  ChannelNames = c("time", ChannelNames2[-1], "Comments")

  ### Extraction of data from CSV file
  col_names = c("Data")
  data1 =   read_csv(file_name,  skip=9, col_names=col_names, show_col_types = FALSE)
  data2 = strsplit(data1$Data, split=';')
  data3 <- do.call(rbind, data2)
  colnames(data3) = ChannelNames
  file_data = as_tibble(data3)
  #file_data[] <- lapply(file_data, function(x) as.numeric(as.character(x)))

    return(file_data)
}
