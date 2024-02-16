# Detects and removes outliers.
# it utilizes PCA (principal component analysis).

detect_outliers_v2.0 = function(data, variable, rate, bin_length){
  # clean up the data first
  data = data %>% mutate(index = seq_len(nrow(.)))
  tmp = data %>%
    #clean_artifacts_v2.0() %>%
    # fix the index
    dplyr::select(-index) %>%
    left_join(data %>% dplyr::select(time, index))

  # calculate the number of bins to use based on the sampling rate
  # the sampling rate will default to 1000
  bin_size = bin_length * rate
  n_bins = ceiling(nrow(data) / bin_size)

  # cut the data into bins
  tmp %<>%
    mutate(bin = cut(time, breaks = n_bins, labels = F)) %>%
    group_by(bin) %>%
    mutate(n_points = n(),
           point = seq_len(n_points)) %>%
    group_by(point) %>%
    mutate(bin_n = n()) %>%
    ungroup() %>%
    filter(bin_n == n_bins)

  # convert this into the appropriate matrix, and generate a PCA
  pca = tmp %>%
    dplyr::select_(variable, "bin", "point") %>%
    spread_("bin", variable) %>%
    as.matrix() %>%
    t() %>%
    PCA(graph = F, scale.unit = F)

  # create a conservative definition of outliers based on hierarchical clustering
  outliers = data.frame(
    outlier = as.data.frame(pca$ind$dist)[-1,] %>%
      dist() %>%
      flashClust() %>%
      cutree(k = 2),
    bin = seq_len(n_bins)
  ) %>%
    # convert to logical
    mutate(outlier = !as.logical(outlier - 1))

  # add these into the final structure that has been cleaned up
  tmp %<>%
    left_join(outliers) %>%
    filter(!outlier) %>%
    # remove some of the unneeded meta data
    dplyr::select(-bin, -n_points, -point, -bin_n, -outlier)

  return(tmp)
}

