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
N = 830138
P1 = 0.15
cvP1 = 0.05
s2P1 = P1 * (1 - P1)
v2P1 = P1 * cvP1
nP1 = round( s2P1 / ( v2P1^2 + (s2P1/N) ) , 0)
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
rhoP1 = ( deffP1 - 1 )/( B-1)
##

bOptP1 = sqrt((CostCluster/CostEstudent) * (1-rhoP1)/rhoP1)
aOptP1 = BudgetTot/(CostCluster + bOptP1*CostEstudent)
noptP1 = bOptP1*aOptP1

################
# Week 6
################

deff2P1 = 1 + (bOptP1-1)*rhoP1
var2SRSP1 = P1*(1-P1)/noptP1
var2P1 = deff2P1 * var2SRSP1

#########
frame %>% 
  group_by(Region) %>% 
  summarise(tot = sum(tot_all)) %>% 
  ungroup() %>% 
  mutate(weight_estrata = tot /sum(tot)) %>% 
  mutate(n_estrata = round(weight_estrata*noptP1,0))
