# Set up the environment (or use local alternative `source("utils/config.R")`)
source("~/R/2021-ProdMgmt-Survey/code/config.R")  

# Load in raw data from google forms and do initial data tidying
source('~/R/2021-ProdMgmt-Survey/code/survey-pre-process.R', echo=FALSE, print.eval = TRUE)
# fit the data to the DEEP v1.1 roadmap maturity model and IPSMA v1.3 SPM framwork (refs in source)
source('~/R/2021-ProdMgmt-Survey/code/survey-modelling.R', echo=FALSE, print.eval = TRUE)
# produce some descriptive visualisations and RMarkdown output report
source('~/R/2021-ProdMgmt-Survey/code/survey-descriptive.R', echo=FALSE, print.eval = TRUE)
# look for relationships and produce latest stats on previously identified relationships
source('~/R/2021-ProdMgmt-Survey/code/survey-explore.R', echo=FALSE, print.eval = TRUE)