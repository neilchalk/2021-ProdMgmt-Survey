## survey-pre-process.R
##    Run this script first to create the clean data set from the Google Forms output collected at https://forms.gle/uQ6jiy44jJe7iUZV9
##
## Acknowledgements
##  
##

library(dplyr)

setwd("~/R/2021-ProdMgmt-Survey/code")

#read in data
raw_responses <- read.csv("../data/2021 Product Management Survey.csv")

#tidy up structure
  
  #split out the product types worked on
  raw_responses <- raw_responses[-2]
  i = grep("B2B", raw_responses[,5])
  raw_responses[,"product.b2b"] <-  FALSE
  raw_responses[i,"product.b2b"] <-  TRUE
  
  i = grep("B2c", raw_responses[,5])
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
  
  raw_responses <- raw_responses[-5] #closed question, so remove column
  
  # split out the professional bodies membership
  i = grep("I am not", raw_responses[,3])
  raw_responses[,"profbody.none"] <-  FALSE
  raw_responses[i,"profbody.none"] <-  TRUE
  
  i = grep("N/A", raw_responses[,3])
  raw_responses[i,"profbody.none"] <-  TRUE
  
  i = grep("None", raw_responses[,3])
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
  
  i = grep("Mind the product", raw_responses[,3], ignore.case = TRUE)
  raw_responses[,"profbody.MTP"] <-  FALSE
  raw_responses[i,"profbody.MTP"] <-  TRUE
  
  
  raw_responses <- raw_responses[-3] #closed question so remove original column

####### split out the info sources
  
  i = grep("Events", raw_responses[,28])
  raw_responses[,"info.events"] <-  FALSE
  raw_responses[i,"info.events"] <-  TRUE
  
  i = grep("Blogs", raw_responses[,28])
  raw_responses[,"info.blogs"] <-  FALSE
  raw_responses[i,"info.blogs"] <-  TRUE
  
  i = grep("Books", raw_responses[,28])
  raw_responses[,"info.books"] <-  FALSE
  raw_responses[i,"info.books"] <-  TRUE
  
  i = grep("Online", raw_responses[,28])
  raw_responses[,"info.communities"] <-  FALSE
  raw_responses[i,"info.communities"] <-  TRUE
  
  i = grep("body", raw_responses[,28])
  raw_responses[,"info.profbody"] <-  FALSE
  raw_responses[i,"info.profbody"] <-  TRUE
  
  i = grep("cert", raw_responses[,28])
  raw_responses[,"info.profcert"] <-  FALSE
  raw_responses[i,"info.profcert"] <-  TRUE
  
  i = grep("training", raw_responses[,28])
  raw_responses[,"info.training"] <-  FALSE
  raw_responses[i,"info.training"] <-  TRUE
  
  i = grep("tool", raw_responses[,28])
  raw_responses[,"info.vendor"] <-  FALSE
  raw_responses[i,"info.vendor"] <-  TRUE
  
  i = grep("Google", raw_responses[,28])
  raw_responses[,"info.google"] <-  FALSE
  raw_responses[i,"info.google"] <-  TRUE
  
  i = grep("network", raw_responses[,28], ignore.case = TRUE)
  raw_responses[,"info.network"] <-  FALSE
  raw_responses[i,"info.network"] <-  TRUE

  i = grep("friend", raw_responses[,28], ignore.case = TRUE)
  raw_responses[i,"info.network"] <-  TRUE
  
  i = grep("colleagues", raw_responses[,28], ignore.case = TRUE)
  raw_responses[i,"info.network"] <-  TRUE
  
#raw_responses <- raw_responses[-28]


#rename column headings to be easier to work with
  names(raw_responses)[3:9] <- c("org.industry","org.employees","org.TTM","org.releases","org.prodteamsize","org.location","roadmap.happiness")
  names(raw_responses)[17:28] <- c("role.happiness","roadmap.detailing", "roadmap.items", "roadmap.reliability", "roadmap.confidence", "roadmap.discovery", "roadmap.prioritization", "roadmap.alignment", "roadmap.responsibility", "roadmap.ownership", "roadmap.tools", "info.sources")

#data cleansing from errors in survey setup in Google Forms
  raw_responses[7, 19] = "Mainly customer and business goals, products, features and for the long-term timeframe topics (e.g., smart home)"
  raw_responses[14, 8] = "Ireland"
  raw_responses[42, "Job.title"] = "CTO"
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
  
  raw_responses <- NULL
