library(readxl)
library(dplyr)
library(tidyverse)
frame <- read_excel("MI_school_frame_head_counts.xls")
sample <- read_excel("sample_school_student_list.xls") %>% 
  drop_na()

