library(dplyr)

setwd("~/R/2021-ProdMgmt-Survey/code")

#read in data
raw_responses <- read.csv("../data/2021 Product Management Survey.csv")

#tidy up structure
raw_responses <- raw_responses[-2]
i = grep("B2B SaaS", raw_responses[,5])

raw_responses[,"product.b2b"] <-  FALSE
raw_responses[i,"product.b2b"] <-  TRUE

i = grep("B2c SaaS", raw_responses[,5])

raw_responses[,"product.b2c"] <-  FALSE
raw_responses[i,"product.b2c"] <-  TRUE

i = grep("Mobile", raw_responses[,5])

raw_responses[,"product.mobile"] <-  FALSE
raw_responses[i,"product.mobile"] <-  TRUE

i = grep("Desktop", raw_responses[,5])

raw_responses[,"product.Desktop"] <-  FALSE
raw_responses[i,"product.Desktop"] <-  TRUE

i = grep("Service", raw_responses[,5])

raw_responses[,"product.Service"] <-  FALSE
raw_responses[i,"product.Service"] <-  TRUE

raw_responses <- raw_responses[-5]

i = grep("I am not", raw_responses[,3])

raw_responses[,"profbody.none"] <-  FALSE
raw_responses[i,"profbody.none"] <-  TRUE

i = grep("ACM", raw_responses[,3])

raw_responses[,"profbody.acm"] <-  FALSE
raw_responses[i,"profbody.acm"] <-  TRUE

i = grep("AMI", raw_responses[,3])

raw_responses[,"profbody.ami"] <-  FALSE
raw_responses[i,"profbody.ami"] <-  TRUE

i = grep("Association of Product Management", raw_responses[,3])

raw_responses[,"profbody.apm"] <-  FALSE
raw_responses[i,"profbody.apm"] <-  TRUE

i = grep("AIPMM", raw_responses[,3])

raw_responses[,"profbody.aipmm"] <-  FALSE
raw_responses[i,"profbody.aipmm"] <-  TRUE

i = grep("BCS", raw_responses[,3])

raw_responses[,"profbody.bcs"] <-  FALSE
raw_responses[i,"profbody.bcs"] <-  TRUE

i = grep("IAOIP", raw_responses[,3])

raw_responses[,"profbody.iaoip"] <-  FALSE
raw_responses[i,"profbody.iaoip"] <-  TRUE

i = grep("ISPMA", raw_responses[,3])

raw_responses[,"profbody.ispma"] <-  FALSE
raw_responses[i,"profbody.ispma"] <-  TRUE

i = grep("PDMA", raw_responses[,3])

raw_responses[,"profbody.pdma"] <-  FALSE
raw_responses[i,"profbody.pdma"] <-  TRUE

raw_responses <- raw_responses[-3]

names(raw_responses)[3] <- "org.industry"
names(raw_responses)[4] <- "org.employees"
names(raw_responses)[5] <- "org.TTM"
names(raw_responses)[6] <- "org.releases"
names(raw_responses)[7] <- "org.prodteamsize"
names(raw_responses)[8] <- "org.location"
names(raw_responses)[9] <- "roadmap.happiness"
names(raw_responses)[17] <- "role.happiness"
names(raw_responses)[18] <- "roadmap.detailing"
names(raw_responses)[19] <- "roadmap.items"
names(raw_responses)[20] <- "roadmap.reliability"
names(raw_responses)[21] <- "roadmap.confidence"
names(raw_responses)[22] <- "roadmap.discovery"
names(raw_responses)[23] <- "roadmap.prioritization"
names(raw_responses)[24] <- "roadmap.alignment"
names(raw_responses)[25] <- "roadmap.responsibility"
names(raw_responses)[26] <- "roadmap.ownership"
names(raw_responses)[27] <- "roadmap.tools"
names(raw_responses)[28] <- "info.sources"
#raw_responses[,29] <- as.factor(raw_responses[,29])

raw_responses <- raw_responses[-29]  #remove old duplicate question
raw_responses <- raw_responses[-29]  #remove free text questions from this analysis
raw_responses <- raw_responses[-1]  #remove Timestamp from this analysis

#tidy up NULLS

#explore data
head(raw_responses)
str(raw_responses)
summary(raw_responses)


#Outlier Analysis and Treatment


#et voila, ready for modelling and visulisation 
write.csv(raw_responses, file = "../data/2021-prdmgmt-survey-clean.csv")