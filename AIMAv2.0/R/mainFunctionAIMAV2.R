
mainFunctionAIMAV2 <- function(data_dir,
                               dataFoldersNames,
                               refGroup,
                               conditionNames,
                               variable,
                               samplingRate,
                               trial_length){


bin_length = samplingRate / 20          # the size of the binning window.

interpolation_time = T                  # a logical (TRUE/FALSE). If it is T, data will be interpolated to one datapoint
                                        # per second using interpolation. If it is F, the existing sampling rate
                                        # will be retained. AIMA v2.0 works with T setting only.

groupNames = dataFoldersNames           # names of the groups.  These are same as of the folder names.



channelTracings_groups = tibble()
groupAvg_channelTracings = tibble()
avgChannelvalues_groups = tibble()
deltaChannel_groups = tibble()
list_of_excelSheetsNames = list()

# First loop extracts a list of data files for each group. Second loop evaluates data of each file per group.
for (f in 1:length(dataFoldersNames)){
  dataFolder = paste0(data_dir,dataFoldersNames[f])
  print(paste("Current folder being run by AIMA v2.0 is ", dataFolder))
  my.files = list.files(dataFolder, pattern = ".txt")
  channelTracings_group = tibble()
  avgChannel_groupCondition1 = tibble()
  avgChannel_groupCondition2 = tibble()

  for (i in 1:length(my.files)) {
    file_name = file.path(dataFolder, my.files[i])
    myFile = my.files[i]
    print(file_name)
    channelTracing_individal = suppressWarnings(
                                      file_name %>%
                                      load_data_v2.0() %>%
                                      extract_data_segments_v2.0(conditionNames) %>%
                                      clean_artifacts_v2.0() %>%
                                      remove_outliers_v2.0(variable, samplingRate, bin_length) %>%
                                      processingAnalysisExport_v2.0(samplingRate, interpolation_time, trial_length, myFile) %>%
                                      channelData_v2.0(variable)
    )
    avgChannel_groupCondition1 = append(avgChannel_groupCondition1, channelTracing_individal[1])
    avgChannel_groupCondition2 = append(avgChannel_groupCondition2, channelTracing_individal[2])
    channelTracings_group = append(channelTracings_group, channelTracing_individal[3])
  }

  Channel_avgValues_tracings = lst(avgChannel_groupCondition1, avgChannel_groupCondition2, channelTracings_group)
  groupAvg_SBPTracing = setNames(lst(rowMeans(as.data.frame(channelTracings_group))), dataFoldersNames[f])
  groupAvg_channelTracings = append(groupAvg_channelTracings, groupAvg_SBPTracing)
  # group_name = paste("group", f, sep = "_")
  # assign(group_name, Channel_avgValues_tracings)
  channelTracings_groups = append(channelTracings_groups, Channel_avgValues_tracings[3])
  deltaChannel_groups = append(deltaChannel_groups, as.numeric(avgChannel_groupCondition2) - as.numeric(avgChannel_groupCondition1) )
  avgChannelvalues_group = append(flatten(avgChannel_groupCondition1), flatten(avgChannel_groupCondition2))
  avgChannelvalues_groups = append(avgChannelvalues_groups, avgChannelvalues_group)
}

# source("plotTracings2G_v2.0.R")
# source("plotTracings3G_v2.0.R")
if(length(groupNames) == 2) {
  p = plotTracings2G_v2.0(
    groupAvg_channelTracings,
    channelTracings_groups,
    deltaChannel_groups,
    avgChannelvalues_groups,
    groupNames,
    trial_length,
    conditionNames,
    refGroup
  )
} else {
  p = plotTracings3G_v2.0(
    groupAvg_channelTracings,
    channelTracings_groups,
    deltaChannel_groups,
    avgChannelvalues_groups,
    groupNames,
    trial_length,
    conditionNames,
    refGroup
  )
}
  return(p)
}
