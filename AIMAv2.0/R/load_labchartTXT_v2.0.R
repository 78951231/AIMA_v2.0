# Load txt files exported from LabChart


load_labchartTXT_v2.0 = function(file_name)
{

  ### Extraction of channel names
  col_names = c("Column1", "Column2")
  col_names = read_delim(file_name, "\t", col_names=col_names, show_col_types = FALSE)
  col_names = as_tibble(col_names)
  col_names = filter(col_names, col_names$Column1 == 'ChannelTitle=')
  col_names = c("time", as_vector(str_split(col_names$Column2, "\t")))


  ### Extraction of data from txt file
  LastChannel = last(col_names)
  data = read_delim(file_name, "\t", skip=9, col_names=col_names
                 , col_types = cols(LastChannel = col_character()))


  ### Separate the last column into two columns to have last column named 'Comments'
  file_data = separate(data, LastChannel, sep="\t", into = c(LastChannel, "Comments"))


  return(file_data)
}
