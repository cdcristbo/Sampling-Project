library(readxl)
library(dplyr)
#library(tidyverse)
frame <- read_excel("MI_school_frame_head_counts.xls")
sample <- read_excel("sample_school_student_list.xls")

clusters = frame %>% 
  group_by(Region) %>% 
  summarise(clusters = n()) %>% 
  t()
################
# Week 4
################
#p1
N = 2443
P1 = 0.15
cvP1 = 0.05
s2P1 = P1 * (1 - P1)
v2P1 = P1 * cvP1
# check the value
nP1 = s2P1 / ( v2P1^2 + (s2P1/N) )
################
# Week 5
################
# values given in the description
n = 7500
a = 150
BudgetTot = 500000
CostCluster = 3000
CostEstudent = 50
#
B = n/a
deffP1 = 2
rhpP1 = ( deffP1 - 1 )/( B-1)
