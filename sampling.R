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
P2 = 0.25
cvP2 = 0.05 
cvP1 = 0.05

s2P1 = P1 * (1 - P1)
v2P1 = P1 * cvP1
nP1 = round( s2P1 / ( v2P1^2 + (s2P1/N) ) , 0)

s2P2 = P2 * (1 - P2)
v2P2 = P2 * cvP2
nP2 = round( s2P2 / ( v2P2^2 + (s2P2/N) ) , 0)
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
deffP2 = 2.5
rhoP2 = ( deffP2 - 1 )/( B-1)
##

bOptP1 = sqrt((CostCluster/CostEstudent) * (1-rhoP1)/rhoP1)
aOptP1 = BudgetTot/(CostCluster + bOptP1*CostEstudent)
noptP1 = bOptP1*aOptP1

bOptP2 = sqrt((CostCluster/CostEstudent) * (1-rhoP2)/rhoP2)
aOptP2 = BudgetTot/(CostCluster + bOptP2*CostEstudent)
noptP2 = bOptP2*aOptP2
################
# Week 6
################

deff2P1 = 1 + (bOptP1-1)*rhoP1
var2SRSP1 = P1*(1-P1)/noptP1
var2P1 = deff2P1 * var2SRSP1

deff2P2 = 1 + (bOptP2-1)*rhoP2
var2SRSP2 = P2*(1-P2)/noptP2
var2P2 = deff2P2 * var2SRSP2
#########
frame %>% 
  group_by(Region) %>% 
  summarise(tot = sum(tot_all)) %>% 
  ungroup() %>% 
  mutate(weight_estrata = tot /sum(tot)) %>% 
  mutate(n_estrataP1 = round(weight_estrata*noptP1,0),
         n_estrataP2 = round(weight_estrata*noptP2,0),
         school_clustersP1 = round(weight_estrata*aOptP1,0),
         school_clustersP2 = round(weight_estrata*aOptP2,0))

Sample2 = frame %>% 
  group_by(Region) %>% 
  left_join(frameSample)

Final_Sample = slice_sample(Sample2, n = aOptP1,
             weight_by = weight_estrata,
             replace = TRUE)

