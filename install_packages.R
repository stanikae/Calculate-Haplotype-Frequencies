if (!require(fs, quietly=TRUE)) {
install.packages("fs")
  library(fs)
}
if (!require(plyr, quietly=TRUE)) {
  install.packages("plyr")
  library(plyr)
}
if (!require(tidyverse, quietly=TRUE)) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require(tidyr, quietly=TRUE)) {
  install.packages("tidyr")
  library(tidyr)
}
if (!require(dplyr, quietly=TRUE)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(purrr, quietly=TRUE)) {
  install.packages("purrr")
  library(purrr)
}
if (!require(stringr, quietly=TRUE)) {
  install.packages("stringr")
  library(stringr)
}
if (!require(openxlsx, quietly=TRUE)) {
  install.packages("openxlsx")
  library(openxlsx)
}

if (!require(readxl, quietly=TRUE)) {
  install.packages("readxl")
  library(readxl)
}

# create input files directory
in_dir <- path("..", "input-files")
if (! dir_exists(in_dir)){
  dir_create(in_dir)
}