## survey-modelling.R
##    first run survey-pre-process.R to create the clean data set 
##
## Acknowledgements
##  
## 
library(rpivotTable) 
library(dplyr)
library(tidyr)


setwd("~/R/2021-ProdMgmt-Survey/code")

clean_responses <- read.csv("../data/2021-prdmgmt-survey-clean.csv", stringsAsFactors=FALSE)

#calculate roadmap process maturity based upon:
# Jürgen Münch,Stefan Trieflinger, Dominic Lang (2019) The Product Roadmap Maturity Model DEEP: Validation of a Method for Assessing the Product Roadmap Capabilities of Organizations
# Conference: 10th International Conference on Software Business (ICSOB 2019)At: Jyväskylä
# https://www.researchgate.net/publication/336070112_The_Product_Roadmap_Maturity_Model_DEEP_Validation_of_a_Method_for_Assessing_the_Product_Roadmap_Capabilities_of_Organizations

clean_responses$roadmap.DEEPScore <- case_when(
          clean_responses$roadmap.detailing == "Next steps are planned ad-hoc and there is no mid- to long term planning. Only short-term planning exists."        ~ 1,
          clean_responses$roadmap.detailing == "All tasks are planned and worked out in detail for short-,mid- and long term."        ~ 3,
          clean_responses$roadmap.detailing == "There is some correlation between time and level of detail but the detailing of the items is not done systematically."        ~ 8,
          clean_responses$roadmap.detailing == "There is a clear correlation between time and level of detail. The timelier items are more detailed."        ~ 15,
          clean_responses$roadmap.detailing == "Short-term items are detailed, prioritized, estimated and validated. Mid-term items are under validation or being discovered. The long-term timeframe contains themes."        ~ 20
        ) + case_when(
          clean_responses$roadmap.items == "Mainly products"        ~ 1,
          clean_responses$roadmap.items == "Mainly products, features"        ~ 3,
          clean_responses$roadmap.items == "Mainly business goals, products,  features"        ~ 10,
          clean_responses$roadmap.items == "Mainly customer and business goals, products, features and for the long-term timeframe topics (e.g., smart home)"        ~ 12,
          clean_responses$roadmap.items == "Mainly product vision, customer and business goals, products, features and for the long-term timeframe themes (i.e., high-level customer needs)"        ~ 20,
          TRUE ~ 0
        ) + case_when(
          clean_responses$roadmap.reliability == "Permanent ad-hoc adjustments."        ~ 1,
          clean_responses$roadmap.reliability == "Frequent ad-hoc adjustments."        ~ 3,
          clean_responses$roadmap.reliability == "Mainly in regular review cycles (e.g., every 3 months)"        ~ 10,
          clean_responses$roadmap.reliability == "Adjustments are mainly done reactively on demand."        ~ 10,
          clean_responses$roadmap.reliability == "Adjustments are mainly done proactively."        ~ 16
        ) + case_when(
          clean_responses$roadmap.confidence == "The impacts are not considered."        ~ 1,
          clean_responses$roadmap.confidence == "The impacts are mainly estimated by experts"        ~ 4,
          clean_responses$roadmap.confidence == "The impacts are mainly determined based on data from the past (e.g., statistics)."        ~ 7,
          clean_responses$roadmap.confidence == "The impacts are partly validated"        ~ 10,
          clean_responses$roadmap.confidence == "The impacts are systematically validated."        ~ 14
        ) + case_when(
          clean_responses$roadmap.discovery == "No discovery activities. Typically, a manager is defining the roadmap items."        ~ 1,
          clean_responses$roadmap.discovery == "Product roadmap items are mainly defined based on expert knowledge."        ~ 2,
          clean_responses$roadmap.discovery == "Product roadmap items are mainly defined based on customer requests."        ~ 4,
          clean_responses$roadmap.discovery == "Several discovery activities are conducted (e.g., user research) but they are not or only loosely integrated with  delivery  activities."        ~ 8,
          clean_responses$roadmap.discovery == "Close integration of discovery and delivery activities"        ~ 10
        ) + case_when(
          clean_responses$roadmap.prioritization == "First in, first out"        ~ 1,
          clean_responses$roadmap.prioritization == "Opinions determine priority"        ~ 2,
          clean_responses$roadmap.prioritization == "Prioritization is based on the capability to deliver (e.g., low hanging fruits)"        ~ 3,
          clean_responses$roadmap.prioritization == "Prioritization is based on short term benefit (e.g., shareholder value)."        ~ 3,
          clean_responses$roadmap.prioritization == "Prioritization is done with an established process and focuses on delivering value to customers and the business."        ~ 7
        ) + case_when(
          clean_responses$roadmap.alignment == "No alignment. No one or only one stakeholder such as high level management has a product roadmap that is not communicated to others."        ~ 1,
          clean_responses$roadmap.alignment == "Several loosely connected product roadmaps for internal stakeholders exist."        ~ 2,
          clean_responses$roadmap.alignment == "Several loosely connected product roadmaps for internal and external stakeholders exist."        ~ 2,
          clean_responses$roadmap.alignment == "One central product roadmap exists for different internal and external stakeholders."        ~ 3,
          clean_responses$roadmap.alignment == "One central product roadmap exists that allows to derive different representations for different stakeholders. A process for achieving alignment and buy-in is in place."        ~ 5
        ) + case_when(
          clean_responses$roadmap.responsibility == "Tools are used to decide if items are placed on the roadmap (e.g., decision matrix)."        ~ 1,
          clean_responses$roadmap.responsibility == "Management"        ~ 2,
          clean_responses$roadmap.responsibility == "Specific roles (e.g., portfolio manager)"        ~ 2,
          clean_responses$roadmap.responsibility == "Product Management"        ~ 3,
          clean_responses$roadmap.responsibility == "Product  Management with cross-functional product teams in  liaison with key stakeholders."        ~ 4
        )  + case_when(
          clean_responses$roadmap.ownership == "No owner defined"        ~ 1,
          clean_responses$roadmap.ownership == "Managers"        ~ 2,
          clean_responses$roadmap.ownership == "Ownership is shared between multiple roles"        ~ 3,
          clean_responses$roadmap.ownership == "Strategy or portfolio planning"        ~ 3,
          clean_responses$roadmap.ownership == "Product management or product teams"        ~ 4
        )

#create maturity level based on DEEP score - levels 1&2 are suggested to restart roadmap process
clean_responses$roadmap.mat_level <- case_when(
      clean_responses$roadmap.DEEPScore <= 18 ~ 1,
      clean_responses$roadmap.DEEPScore <= 30 ~ 2,
      clean_responses$roadmap.DEEPScore <= 57 ~ 3,
      clean_responses$roadmap.DEEPScore <= 83 ~ 4,
      clean_responses$roadmap.DEEPScore <= 100 ~ 5)

#put in factors
clean_responses$Job.title = factor(clean_responses$Job.title)
clean_responses$org.industry = factor(clean_responses$org.industry)
clean_responses$org.employees = factor(clean_responses$org.employees, levels = c("< 10","10-49","50-249","250-4499",">= 4500"))
clean_responses$org.TTM = factor(clean_responses$org.TTM, levels = c("Less than 4.5 months","4.5 months to < 9 months","9 Months to < 18 months","More than 18 months","Don't know and cannot estimate"))
clean_responses$org.releases = factor(clean_responses$org.releases, levels =c("More than 12 releases a year","5-12 releases a year","3-4 releases a year","About 2 releases per year", "About 1 release per year", "Less than one release per year", "No release so far"))
clean_responses$org.prodteamsize = factor(clean_responses$org.prodteamsize, levels = c("< 4","4-9","10-19","20-49","50-249","> 250"))
clean_responses$org.location = factor(clean_responses$org.location)
clean_responses$info.sources = factor(clean_responses$info.sources)

clean_responses$roadmap.detailing = factor(clean_responses$roadmap.detailing, levels= c("Next steps are planned ad-hoc and there is no mid- to long term planning. Only short-term planning exists.",
                                                                                        "All tasks are planned and worked out in detail for short-,mid- and long term.",
                                                                                        "There is some correlation between time and level of detail but the detailing of the items is not done systematically.",
                                                                                        "There is a clear correlation between time and level of detail. The timelier items are more detailed.",
                                                                                        "Short-term items are detailed, prioritized, estimated and validated. Mid-term items are under validation or being discovered. The long-term timeframe contains themes."
)
)

clean_responses$roadmap.items = factor(clean_responses$roadmap.items, levels= c("Mainly products",
                                                                                "Mainly products, features",
                                                                                "Mainly business goals, products,  features",
                                                                                "Mainly customer and business goals, products, features and for the long-term timeframe topics (e.g., smart home)",
                                                                                "Mainly product vision, customer and business goals, products, features and for the long-term timeframe themes (i.e., high-level customer needs)"
)
)
clean_responses$roadmap.reliability = factor(clean_responses$roadmap.reliability, levels= c("Permanent ad-hoc adjustments.",
                                                                                            "Frequent ad-hoc adjustments." ,
                                                                                            "Mainly in regular review cycles (e.g., every 3 months)",
                                                                                            "Adjustments are mainly done reactively on demand.",
                                                                                            "Adjustments are mainly done proactively."   
)
)
clean_responses$roadmap.confidence = factor(clean_responses$roadmap.confidence, levels= c( "The impacts are not considered.",
                                                                                           "The impacts are mainly estimated by experts",
                                                                                           "The impacts are mainly determined based on data from the past (e.g., statistics).",
                                                                                           "The impacts are partly validated",
                                                                                           "The impacts are systematically validated." 
)
)
clean_responses$roadmap.discovery = factor(clean_responses$roadmap.discovery, levels= c( "No discovery activities. Typically, a manager is defining the roadmap items.",
                                                                                         "Product roadmap items are mainly defined based on expert knowledge.",
                                                                                         "Product roadmap items are mainly defined based on customer requests.",
                                                                                         "Several discovery activities are conducted (e.g., user research) but they are not or only loosely integrated with  delivery  activities.",
                                                                                         "Close integration of discovery and delivery activities"  
)
)
clean_responses$roadmap.prioritization = factor(clean_responses$roadmap.prioritization, levels= c( "First in, first out",
                                                                                                   "Opinions determine priority",
                                                                                                   "Prioritization is based on the capability to deliver (e.g., low hanging fruits)",
                                                                                                   "Prioritization is based on short term benefit (e.g., shareholder value).",
                                                                                                   "Prioritization is done with an established process and focuses on delivering value to customers and the business." 
)
)
clean_responses$roadmap.alignment = factor(clean_responses$roadmap.alignment, levels= c( "No alignment. No one or only one stakeholder such as high level management has a product roadmap that is not communicated to others.",
                                                                                         "Several loosely connected product roadmaps for internal stakeholders exist.",
                                                                                         "Several loosely connected product roadmaps for internal and external stakeholders exist.",
                                                                                         "One central product roadmap exists for different internal and external stakeholders.",
                                                                                         "One central product roadmap exists that allows to derive different representations for different stakeholders. A process for achieving alignment and buy-in is in place."
)
)
clean_responses$roadmap.responsibility = factor(clean_responses$roadmap.responsibility, levels= c( "Tools are used to decide if items are placed on the roadmap (e.g., decision matrix).",
                                                                                                   "Management",
                                                                                                   "Specific roles (e.g., portfolio manager)",
                                                                                                   "Product Management",
                                                                                                   "Product  Management with cross-functional product teams in  liaison with key stakeholders." 
)
)
clean_responses$roadmap.ownership = factor(clean_responses$roadmap.ownership, levels= c( "No owner defined",
                                                                                         "Managers",
                                                                                         "Ownership is shared between multiple roles",
                                                                                         "Strategy or portfolio planning",
                                                                                         "Product management or product teams"
)
)

practices <- clean_responses %>%
  select(X, Job.title, role.happiness, roadmap.happiness , roadmap.DEEPScore, roadmap.mat_level, 10:16)  

i = grep("Corporate Strategy", practices[,7])
practices[,"sm.corpstrat"] <-  FALSE
practices[i,"sm.corpstrat"] <-  TRUE

i = grep("Portfolio Management", practices[,7])
practices[,"sm.portfoliomgmt"] <-  FALSE
practices[i,"sm.portfoliomgmt"] <-  TRUE

i = grep("Innovation Management", practices[,7])
practices[,"sm.innovationmgmt"] <-  FALSE
practices[i,"sm.innovationmgmt"] <-  TRUE

i = grep("Resource Management", practices[,7])
practices[,"sm.resmgmt"] <-  FALSE
practices[i,"sm.resmgmt"] <-  TRUE

i = grep("Market Analysis", practices[,7])
practices[,"sm.marketanalysis"] <-  FALSE
practices[i,"sm.marketanalysis"] <-  TRUE

i = grep("Product Analysis", practices[,7])
practices[,"sm.prodanalysis"] <-  FALSE
practices[i,"sm.prodanalysis"] <-  TRUE

practices <- practices[-7]

i = grep("Positioning and product definition", practices[,7])
practices[,"ps.position"] <-  FALSE
practices[i,"ps.position"] <-  TRUE

i = grep("Delivery model & Service strategy", practices[,7])
practices[,"ps.deliverymodel"] <-  FALSE
practices[i,"ps.deliverymodel"] <-  TRUE

i = grep("Sourcing", practices[,7])
practices[,"ps.sourcing"] <-  FALSE
practices[i,"ps.sourcing"] <-  TRUE

i = grep("Business case and costing", practices[,7])
practices[,"ps.bizcase"] <-  FALSE
practices[i,"ps.bizcase"] <-  TRUE

i = grep("Pricing", practices[,7])
practices[,"ps.pricing"] <-  FALSE
practices[i,"ps.pricing"] <-  TRUE

i = grep("Ecosystem management", practices[,7])
practices[,"ps.ecosystem"] <-  FALSE
practices[i,"ps.ecosystem"] <-  TRUE

i = grep("Legal & PR management", practices[,7])
practices[,"ps.legalandpr"] <-  FALSE
practices[i,"ps.legalandpr"] <-  TRUE

i = grep("Performance & Risk management", practices[,7])
practices[,"ps.perfandrisk"] <-  FALSE
practices[i,"ps.perfandrisk"] <-  TRUE

practices <- practices[-7]

i = grep("Product life-cycle management", practices[,7])
practices[,"pp.lifecycle"] <-  FALSE
practices[i,"pp.lifecycle"] <-  TRUE

i = grep("Roadmapping", practices[,7])
practices[,"pp.roadmapping"] <-  FALSE
practices[i,"pp.roadmapping"] <-  TRUE

i = grep("Release planning", practices[,7])
practices[,"pp.releaseplanning"] <-  FALSE
practices[i,"pp.releaseplanning"] <-  TRUE

i = grep("Product requirements engineering", practices[,7])
practices[,"pp.prodRE"] <-  FALSE
practices[i,"pp.prodRE"] <-  TRUE

practices <- practices[-7]

i = grep("Engineering Management", practices[,7])
practices[,"dev.engmgmt"] <-  FALSE
practices[i,"dev.engmgmt"] <-  TRUE

i = grep("Project Management", practices[,7])
practices[,"dev.projmgmt"] <-  FALSE
practices[i,"dev.projmgmt"] <-  TRUE

i = grep("Project requirements engineering", practices[,7])
practices[,"dev.projRE"] <-  FALSE
practices[i,"dev.projRE"] <-  TRUE

i = grep("User experience design", practices[,7])
practices[,"dev.ux"] <-  FALSE
practices[i,"dev.ux"] <-  TRUE

i = grep("Quality Management", practices[,7])
practices[,"dev.qual"] <-  FALSE
practices[i,"dev.qual"] <-  TRUE

practices <- practices[-7]

i = grep("Marketing planning", practices[,7])
practices[,"mar.plan"] <-  FALSE
practices[i,"mar.plan"] <-  TRUE

i = grep("Customer analysis", practices[,7])
practices[,"mar.cust"] <-  FALSE
practices[i,"mar.cust"] <-  TRUE

i = grep("Opportunity Management", practices[,7])
practices[,"mar.oppomgmt"] <-  FALSE
practices[i,"mar.oppomgmt"] <-  TRUE

i = grep("Marketing mix optimisation", practices[,7])
practices[,"mar.mix"] <-  FALSE
practices[i,"mar.mix"] <-  TRUE

i = grep("Product launches (GTM)", practices[,7])
practices[,"mar.gtm"] <-  FALSE
practices[i,"mar.gtm"] <-  TRUE

i = grep("Operational marketing", practices[,7])
practices[,"mar.ops"] <-  FALSE
practices[i,"mar.ops"] <-  TRUE


practices <- practices[-7]