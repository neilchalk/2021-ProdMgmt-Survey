## survey-explore.R
##    first run survey-pre-process.R to create the clean data set and then 
##    survey-modelling.R to load and build the data model.
##
##    This is a script to run various explorations of the data to then feed into the model and visualisations.
##    The code below represents hypothesises of factors that could be correlations in the data
##
## Acknowledgements
##  
##
library(rpivotTable) 
library(ggplot2)
library(dplyr)

## Begin exploring data
  lmDEEPScore = lm(roadmap.DEEPScore ~ clean_responses$org.employees + clean_responses$org.TTM + clean_responses$org.releases + clean_responses$org.prodteamsize + clean_responses$org.location, data = clean_responses) #Create a linear regression with two variables
  summary(lmDEEPScore)
  anova(lmDEEPScore)
  
  lmDEEPScore2 = lm(roadmap.DEEPScore ~ org.releases + org.TTM + product.b2b + product.b2c, data = clean_responses) #Create a linear regression with two variables
  summary(lmDEEPScore2)
  anova(lmDEEPScore2)  

  lmHappyRoadmap = lm(roadmap.happiness ~ org.releases + org.TTM + roadmap.DEEPScore + product.b2b + product.b2c , data = clean_responses) #Create a linear regression with two variables
  summary(lmHappyRoadmap)
  anova(lmHappyRoadmap)

  # Given early results, role responsibility happiness seems to be an indicator of roadmap maturity
  lmHappyRole = lm(role.happiness ~ roadmap.DEEPScore , data = clean_responses) #Create a linear regression with two variables
  summary(lmHappyRole)
  anova(lmHappyRole)
  
  png("../outputs/DEEPscore_vs_roleHappiness.png")
  myplot <- ggplot(clean_responses,aes(x = roadmap.DEEPScore, y = role.happiness)) +
    geom_point(aes(x = roadmap.DEEPScore, y = role.happiness, colour = org.industry, shape = org.employees)) +
    geom_abline(slope = coef(lmHappyRole)[[2]], intercept = coef(lmHappyRole)[[1]], colour="#CC0000") +
    facet_wrap(~Job.title)
  print(myplot)
  dev.off()
  
  plot(lmHappyRole$residuals, pch = 16, col = "red")
  
  
  ggplot(clean_responses,aes(x = roadmap.DEEPScore, y = role.happiness)) +
    geom_point(aes(x = roadmap.DEEPScore, y = role.happiness, colour = org.industry, shape = org.employees)) +
    geom_abline(slope = coef(lmHappyRole)[[2]], intercept = coef(lmHappyRole)[[1]], colour="#CC0000") +
    facet_wrap(~org.location)
  
  # however, given early results, roadmap happiness does not seem to be an indicator of roadmap maturity
  lmHappyRoadmap = lm(roadmap.happiness ~ roadmap.DEEPScore , data = clean_responses) #Create a linear regression with two variables
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
            "Professional Bodies" = sum(info.profbody)/n()*100,
            "Professional Certification" = sum(info.profcert)/n()*100,
            "Professional Training" = sum(info.training)/n()*100,
            "Tool Vendor material" = sum(info.vendor)/n()*100,
            "Google" = sum(info.google)/n()*100)

clean_responses %>%
  select(Job.title, profbody.acm, profbody.aipmm, profbody.ami, profbody.apm, profbody.bcs, profbody.iaoip, profbody.ispma, profbody.pdma, profbody.none) %>%
  group_by(Job.title) %>%
  summarise("ACM" = round(sum(profbody.acm)/n()*100),
            "AIPMM" = sum(profbody.aipmm)/n()*100,
            "AMI" = sum(profbody.ami)/n()*100,
            "APM" = sum(profbody.apm)/n()*100,
            "BCS" = sum(profbody.bcs)/n()*100,
            "IAOIP" = sum(profbody.iaoip)/n()*100,
            "ISPMA" = sum(profbody.ispma)/n()*100,
            "PDMA" = sum(profbody.pdma)/n()*100,
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