library(rpivotTable) 
library(dplyr)
library(tidyr)

setwd("~/R/2021-ProdMgmt-Survey/code")

clean_responses <- read.csv("../data/2021-prdmgmt-survey-clean.csv")

#average happiness with role and roadmap by job title
clean_responses %>%
  select(Job.title, roadmap.happiness ,role.happiness) %>%
  mutate_if(is.character,as.factor) %>%
    group_by(Job.title) %>%
  summarise(n = n(),
            roadmap.happiness = mean(roadmap.happiness),
            role.happiness = mean(role.happiness))

#calculate roadmap process maturity

