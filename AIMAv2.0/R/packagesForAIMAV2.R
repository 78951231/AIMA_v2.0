

packagesForAIMAV2 <- function() {

  #' packages required to run AIMA v2.0.
  install.packages("tidyverse")
  install.packages("zoo")
  install.packages("magrittr")
  install.packages("FactoMineR")
  install.packages("flashClust")
  install.packages("purrr")
  install.packages("patchwork")
  # install.packages("xlsx")
  # install.packages("rio")
  # install_formats()
  install.packages("reshape2")
  install.packages("cowplot")
  install.packages("ggprism")
  install.packages("rstatix")


  library(ggprism)
  library(cowplot)
  library(reshape2)
  # library(rio)
  library(FactoMineR)
  library(flashClust)
  library(tidyverse)
  library(readr)
  library(zoo)
  library(magrittr)
  library(purrr)
  library(patchwork)
  # library(xlsx)
  library(rstatix)
}
