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
aopt<-80
bopt<-64
nopt<-5175
sample2 = frame %>% 
  group_by(Region) %>% 
  summarise(tot = sum(tot_all)) %>% 
  ungroup() %>% 
  mutate(weight_estrata = tot /sum(tot)) %>% 
  mutate(n_estrataP1 = round(weight_estrata*nopt,0),
         school_clustersP1 = round(weight_estrata*aopt,0)) %>% 
  mutate(school_clustersP1 = ifelse(school_clustersP1==0,1,school_clustersP1))


week7 = frame %>% 
  left_join(sample2)

sample_sizes = sample2$school_clustersP1

sample3 = sample2 %>% select(Region,school_clustersP1)
sampleProject=NULL

for (i in  1:9){
    sample_df <- week7 %>%
    filter(Region==sample3$Region[i]) %>%
    sample_n(sample3$school_clustersP1[i])
sampleProject=rbind(sampleProject,sample_df)  
}
