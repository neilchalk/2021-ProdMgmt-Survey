library(dplyr)

setwd("~/R/2021-ProdMgmt-Survey/code")

#read in data
raw_responses <- read.csv("../data/2021 Product Management Survey.csv")

#explore data
head(raw_responses)
str(raw_responses)
summary(raw_responses)


#tidy up structure
raw_responses <- raw_responses[-2]
#tidy up NULLS


#Outlier Analysis and Treatment


#et voila, ready for modelling and visulisation 
write.csv(raw_responses, file = "../data/2021-prdmgmt-survey-clean.csv")