## survey-explore.R
##    first run survey-pre-process.R to create the clean data set and then 
##    survey-modelling.R to load and build the data model.
##
##    This is a script to run various explorations of the data to then feed into the model and visualisations.
##    The code below represents various hypothesis of factors that could be correlations in the data
##
## Acknowledgements
##  
##
library(rpivotTable) 
library(ggplot2)
library(dplyr)

## Begin exploring data
  lmDEEPScore = lm(roadmap.DEEPScore ~ org.employees + org.TTM + org.releases + org.prodteamsize + org.location, data = clean_responses) #Create a linear regression with two variables
  summary(lmDEEPScore)
  anova(lmDEEPScore)
  
  lmDEEPScore2 = lm(roadmap.DEEPScore ~ org.releases + org.TTM + product.b2b + product.b2c, data = clean_responses) #Create a linear regression with two variables
  summary(lmDEEPScore2)
  anova(lmDEEPScore2)  

  lmHappyRoadmap = lm(roadmap.happiness ~ org.releases + org.TTM + roadmap.DEEPScore + product.b2b + product.b2c , data = clean_responses) #Create a linear regression with two variables
  summary(lmHappyRoadmap)
  anova(lmHappyRoadmap)

  # Given early results, role responsibility happiness seems to be an indicator of roadmap maturity
  lmHappyRole = lm(role.happiness ~ roadmap.DEEPScore , data = prod_responses) #Create a linear regression with two variables
  summary(lmHappyRole)
  anova(lmHappyRole)
  
  png("../outputs/DEEPscore_vs_roleHappiness.png")
  myplot <- ggplot(prod_responses,aes(x = roadmap.DEEPScore, y = role.happiness)) +
    geom_point(aes(x = roadmap.DEEPScore, y = role.happiness, colour = org.industry, shape = org.employees)) +
    geom_abline(slope = coef(lmHappyRole)[[2]], intercept = coef(lmHappyRole)[[1]], colour="#CC0000") +
    facet_wrap(~Job.title)
  print(myplot)
  dev.off()
  
  plot(lmHappyRole$residuals, pch = 16, col = "red")
  
  ggplot(prod_responses,aes(x = roadmap.DEEPScore, y = role.happiness)) +
    geom_point(aes(x = roadmap.DEEPScore, y = role.happiness, colour = Job.title))  +
    geom_smooth(method=lm) +
    facet_wrap(~org.employees)
  
  ggplot(prod_responses,aes(x = roadmap.DEEPScore, y = role.happiness)) +
    geom_point(aes(x = roadmap.DEEPScore, y = role.happiness, colour = org.industry, shape = org.employees)) +
    geom_smooth(method=lm)
  
  ggplot(prod_responses,aes(x = roadmap.DEEPScore, y = role.happiness)) +
    geom_point(aes(x = roadmap.DEEPScore, y = role.happiness, colour = org.industry, shape = org.employees)) +
    geom_abline(slope = coef(lmHappyRole)[[2]], intercept = coef(lmHappyRole)[[1]], colour="#CC0000") +
    facet_wrap(~org.location)
  
  # given early results, roadmap happiness does seem to be an indicator of roadmap maturity with product roles
  lmHappyRoadmap = lm(roadmap.happiness ~ roadmap.DEEPScore , data = prod_responses) #Create a linear regression with two variables
  summary(lmHappyRoadmap)
  
  # given early results, roadmap happiness does not seem to be an indicator of roadmap maturity with non-prod roles
  lmHappyRoadmap = lm(roadmap.happiness ~ roadmap.DEEPScore , data = nonprod_responses) #Create a linear regression with two variables
  summary(lmHappyRoadmap)
  
  clean_responses %>%
    rpivotTable(
      rows = "Job.title", 
      cols = "roadmap.DEEPScore",
      aggregatorName = "Sum", 
      vals = "roadmap.mat_level", 
      rendererName = "Col Heatmap")   
  
#average happiness with roadmap by job title
clean_responses %>%
  select(Job.title, roadmap.happiness, role.happiness , roadmap.DEEPScore, roadmap.mat_level) %>% 
  group_by(Job.title) %>%
  summarise(n = n(),
            roadmap.happiness = mean(roadmap.happiness),
            role.happiness = mean(role.happiness),
            roadmap.DEEPScore = mean(roadmap.DEEPScore),
            roadmap.mat_level = mean(roadmap.mat_level)) 

#average happiness with roadmap by location
clean_responses %>%
  select(org.location, roadmap.happiness , roadmap.DEEPScore, roadmap.mat_level) %>% 
  group_by(org.location) %>%
  summarise(n = n(),
            roadmap.happiness = mean(roadmap.happiness),
            roadmap.DEEPScore = mean(roadmap.DEEPScore),
            roadmap.mat_level = mean(roadmap.mat_level)) 


#average happiness with roadmap by time to market
clean_responses %>%
  select(org.TTM, roadmap.happiness , roadmap.DEEPScore, roadmap.mat_level) %>% 
  group_by(org.TTM) %>%
  summarise(n = n(),
            roadmap.happiness = mean(roadmap.happiness),
            roadmap.DEEPScore = mean(roadmap.DEEPScore),
            roadmap.mat_level = mean(roadmap.mat_level)) 

#average DEEPScore with roadmap by roadmap hapiness
clean_responses %>%
  select( roadmap.happiness , roadmap.DEEPScore, roadmap.mat_level) %>% 
  group_by(roadmap.happiness) %>%
  summarise(n = n(),
            roadmap.DEEPScore = mean(roadmap.DEEPScore),
            roadmap.mat_level = mean(roadmap.mat_level)) 




clean_responses %>%
  select(Job.title, info.events, info.blogs, info.books, info.communities, info.profbody, info.profcert, info.training, info.vendor, info.google) %>%
  mutate_if(is.character,as.factor) %>%
  group_by(Job.title) %>%
  summarise("Events and conferences" = round(sum(info.events)/n()*100),
            "Blogs" = sum(info.blogs)/n()*100,
            "Books" = sum(info.books)/n()*100,
            "Online Communities" = sum(info.communities)/n()*100,
            "Prof Bodies" = sum(info.profbody)/n()*100,
            "Prof Certification" = sum(info.profcert)/n()*100,
            "Prof Training" = sum(info.training)/n()*100,
            "Tool Vendors" = sum(info.vendor)/n()*100,
            "Google" = sum(info.google)/n()*100)

prod_responses %>%
  select(Job.title, profbody.acm, profbody.aipmm, profbody.ami, profbody.apm, profbody.bcs, profbody.iaoip, profbody.ispma, profbody.pdma, profbody.MTP, profbody.WiP, profbody.none) %>%
  group_by(Job.title) %>%
  summarise("ACM" = round(sum(profbody.acm)/n()*100),
            "AIPMM" = sum(profbody.aipmm)/n()*100,
            "AMI" = sum(profbody.ami)/n()*100,
            "APM" = sum(profbody.apm)/n()*100,
            "BCS" = sum(profbody.bcs)/n()*100,
            "IAOIP" = sum(profbody.iaoip)/n()*100,
            "ISPMA" = sum(profbody.ispma)/n()*100,
            "PDMA" = sum(profbody.pdma)/n()*100,
            "MTP" = sum(profbody.MTP)/n()*100,
            "WiP" = sum(profbody.WiP)/n()*100,
            "None" = sum(profbody.none)/n()*100)

nonprod_responses %>%
  select(Job.title, profbody.acm, profbody.aipmm, profbody.ami, profbody.apm, profbody.bcs, profbody.iaoip, profbody.ispma, profbody.pdma, profbody.MTP, profbody.WiP, profbody.none) %>%
  group_by(Job.title) %>%
  summarise("ACM" = round(sum(profbody.acm)/n()*100),
            "AIPMM" = sum(profbody.aipmm)/n()*100,
            "AMI" = sum(profbody.ami)/n()*100,
            "APM" = sum(profbody.apm)/n()*100,
            "BCS" = sum(profbody.bcs)/n()*100,
            "IAOIP" = sum(profbody.iaoip)/n()*100,
            "ISPMA" = sum(profbody.ispma)/n()*100,
            "PDMA" = sum(profbody.pdma)/n()*100,
            "MTP" = sum(profbody.MTP)/n()*100,
            "WiP" = sum(profbody.WiP)/n()*100,
            "None" = sum(profbody.none)/n()*100)

ggplot(clean_responses) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1), aes(x = org.employees, y = roadmap.DEEPScore, colour = roadmap.happiness, shape = org.releases)) 

ggplot(clean_responses) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1), aes(x = org.industry, y = roadmap.DEEPScore, colour = roadmap.happiness, shape = org.releases)) 

ggplot(clean_responses) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1), aes(x = org.prodteamsize, y = roadmap.DEEPScore, colour = roadmap.happiness, shape = org.releases))



ggplot(clean_responses, aes(x=org.employees, y=roadmap.DEEPScore)) + 
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitter(0.2))


ggplot(clean_responses, aes(x=role.happiness, y=roadmap.DEEPScore)) + 
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitter(0.2)) 

summary(practices)

practices %>%
  select(Job.title, sm.corpstrat, sm.portfoliomgmt, sm.innovationmgmt, sm.resmgmt, sm.marketanalysis, sm.prodanalysis) %>%
  group_by(Job.title) %>%
  summarise("Corporate Strategy" = round(sum(sm.corpstrat)/n()*100),
            "Portfolio Management" = sum(sm.portfoliomgmt)/n()*100,
            "Innovation Management" = sum(sm.innovationmgmt)/n()*100,
            "Resource Management" = sum(sm.resmgmt)/n()*100,
            "Market Analysis" = sum(sm.marketanalysis)/n()*100,
            "Product Analysis" = sum(sm.prodanalysis)/n()*100)

# appears to be a relationship between having responsibility for market analysis and roadmap maturity 
lmDEEPPractices = lm(roadmap.DEEPScore ~ sm.marketanalysis , data = practices) #Create a linear regression with two variables
summary(lmDEEPPractices)

ggplot(practices) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1), aes(y = sm.corpstrat, x = roadmap.DEEPScore)) 
  ggplot(practices) +  geom_point(position = position_jitter(width = 0.1, height = 0.1), aes(y = sm.portfoliomgmt, x = roadmap.DEEPScore)) 
  ggplot(practices) +  geom_point(position = position_jitter(width = 0.1, height = 0.1), aes(y = sm.innovationmgmt, x = roadmap.DEEPScore)) 
  ggplot(practices) +  geom_point(position = position_jitter(width = 0.1, height = 0.1), aes(y = sm.resmgmt, x = roadmap.DEEPScore)) 
  ggplot(practices) +  geom_point(position = position_jitter(width = 0.1, height = 0.1), aes(y = sm.marketanalysis, x = roadmap.DEEPScore)) 
  ggplot(practices) +  geom_point(position = position_jitter(width = 0.1, height = 0.1), aes(y = sm.prodanalysis, x = roadmap.DEEPScore))


  ggplot(practices, aes(x=sm.marketanalysis, y=roadmap.DEEPScore)) + 
    geom_boxplot() + 
    geom_jitter(shape=16, position=position_jitter(0.2)) 


practices %>%
  rpivotTable(
    rows = "roadmap.DEEPScore", 
    cols = "sm.marketanalysis",
    aggregatorName = "Count", 
    vals = "roadmap.mat_level", 
    rendererName = "Col Heatmap")  


practices %>%
  select(roadmap.mat_level, ps.position, ps.deliverymodel, ps.sourcing, ps.bizcase, ps.pricing, ps.ecosystem, ps.legalandpr, ps.perfandrisk) %>%
  group_by(roadmap.mat_level) %>%
  summarise("Positioning and product definition" = sum(ps.position)/n()*100,
            "Delivery model & Service strategy" = sum(ps.deliverymodel)/n()*100,
            "Sourcing" = sum(ps.sourcing)/n()*100,
            "Business case and costing" = sum(ps.bizcase)/n()*100,
            "Pricing" = sum(ps.pricing)/n()*100,
            "Ecosystem management" = sum(ps.ecosystem)/n()*100,
            "Legal & PR management" = sum(ps.legalandpr)/n()*100,
            "Performance & Risk management" = sum(ps.perfandrisk)/n()*100)

# appears to be a relationship between having responsibility for "Business case and costing" and roadmap maturity 
lmDEEPProdPractices = lm(roadmap.DEEPScore ~ ps.bizcase , data = practices) #Create a linear regression with two variables
summary(lmDEEPProdPractices)
plot(lmDEEPProdPractices$residuals, pch = 16, col = "red")


practices %>%
  select(roadmap.mat_level, pp.lifecycle, pp.roadmapping,pp.releaseplanning, pp.prodRE) %>%
  group_by(roadmap.mat_level) %>%
  summarise("Product life-cycle management" = sum(pp.lifecycle)/n()*100,
            "Roadmapping" = sum(pp.roadmapping)/n()*100,
            "Release planning" = sum(pp.releaseplanning)/n()*100,
            "Product requirements engineering" = sum(pp.prodRE)/n()*100)

# appears to be a relationship between having responsibility for "Release planning" and roadmap maturity 
lmDEEPPlanPractices = lm(roadmap.DEEPScore ~ pp.releaseplanning , data = practices) #Create a linear regression with two variables
summary(lmDEEPPlanPractices)

plot(lmDEEPPlanPractices$residuals, pch = 16, col = "red")

ggplot(practices, aes(x=pp.releaseplanning, y=roadmap.DEEPScore)) + 
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitter(0.2)) 

          practices %>%
            select(roadmap.mat_level, dev.engmgmt,dev.projmgmt, dev.projRE, dev.ux, dev.qual ) %>%
            group_by(roadmap.mat_level) %>%
            summarise("Engineering Management" = sum(dev.engmgmt)/n()*100,
                      "Project Management" = sum(dev.projmgmt)/n()*100,
                      "Project requirements engineering" = sum(dev.projRE)/n()*100,
                      "User experience design" = sum(dev.ux)/n()*100,
                      "Quality Management" = sum(dev.qual)/n()*100)
        
            # appears to be a negative relationship between having responsibility for "Engineering Management"  and roadmap maturity 
          lmDEEPDevPractices = lm(roadmap.DEEPScore ~ dev.engmgmt , data = practices) #Create a linear regression with two variables
          summary(lmDEEPDevPractices)
          
          #remove non_PM jobs to see impact
          pm.practices <-      practices %>%
            select(roadmap.DEEPScore, Job.title, roadmap.mat_level, dev.engmgmt,dev.projmgmt, dev.projRE, dev.ux, dev.qual ) %>%
            filter(grepl('Product', Job.title)) 
          
          # still appears to be a negative relationship between having responsibility for "Engineering Management"  and roadmap maturity 
          lmDEEPDevPractices = lm(roadmap.DEEPScore ~ dev.engmgmt , data = pm.practices) #Create a linear regression with two variables
          summary(lmDEEPDevPractices)
          
          plot(lmDEEPDevPractices$residuals, pch = 16, col = "red")
          
          ggplot(practices, aes(x=dev.engmgmt, y=roadmap.DEEPScore)) + 
            geom_boxplot() + 
            geom_jitter(shape=16, position=position_jitter(0.2)) 


          
          practices %>%
            select(roadmap.mat_level, mar.plan,mar.cust, mar.oppomgmt, mar.mix, mar.gtm,mar.ops ) %>%
            group_by(roadmap.mat_level) %>%
            summarise("Marketing planning" = sum(mar.plan)/n()*100,
                      "Customer analysis" = sum(mar.cust)/n()*100,
                      "Opportunity Management" = sum(mar.oppomgmt)/n()*100,
                      "Marketing mix optimisation" = sum(mar.mix)/n()*100,
                      "Product launches (GTM)" = sum(mar.gtm)/n()*100,
                      "Operational marketing" = sum(mar.ops)/n()*100)
          
          # appears to be no relationships with marketing responsibilities and roadmap maturity 
          lmDEEPMarPractices = lm(roadmap.DEEPScore ~ mar.plan * mar.mix  , data = practices) #Create a linear regression with two variables
          summary(lmDEEPMarPractices)
          
          #remove non_PM jobs to see impact
          pm.practices <-      practices %>%
            select(roadmap.DEEPScore, Job.title, roadmap.mat_level, mar.plan,mar.cust, mar.oppomgmt, mar.mix, mar.gtm,mar.ops ) %>%
            filter(grepl('Product', Job.title)) 
          
          # appears to be no relationships with marketing responsibilities and roadmap maturity 
          lmDEEPMarPractices = lm(roadmap.DEEPScore ~ mar.plan * mar.mix , data = pm.practices) #Create a linear regression with two variables
          summary(lmDEEPMarPractices)
          
          plot(lmDEEPMarPractices$residuals, pch = 16, col = "red")
          
          ggplot(practices, aes(x=mar.mix, y=roadmap.DEEPScore)) + 
            geom_boxplot() + 
            geom_jitter(shape=16, position=position_jitter(0.2))           
          
          
    #take a look at the typical makeup of org and prod team size in the respondents      
          
          ggplot(clean_responses, aes(x=org.employees, y=org.prodteamsize)) + 
            geom_jitter(shape=16, position=position_jitter(0.2)) +
            facet_wrap(~org.location)          
          
## TOOLS
          
          lmDEEPTools = lm(roadmap.DEEPScore ~ custom * office * product * project , data = tools) #Create a linear regression with two variables
          summary(lmDEEPTools)
          
          #plot(lmDEEPTools$residuals, pch = 16, col = "red")
          
          
          #apears to be a negative relationship with just office/project tools but positive with both
          pm.tools <-      tools %>%
             filter(!grepl('Product', Job.title)) 
          
          lmDEEPProdTools = lm(roadmap.DEEPScore ~ office * project , data = pm.tools) #Create a linear regression with two variables
          summary(lmDEEPProdTools)
          