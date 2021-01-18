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


## Begin exploring data
  lmDEEPScore = lm(clean_responses$roadmap.DEEPScore ~ clean_responses$org.employees + clean_responses$org.TTM + clean_responses$org.releases + clean_responses$org.prodteamsize + clean_responses$org.location, data = clean_responses) #Create a linear regression with two variables
  summary(lmDEEPScore)
  anova(lmDEEPScore)



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




ggplot(clean_responses) +
  geom_point(aes(x = roadmap.happiness, y = roadmap.DEEPScore, colour = roadmap.mat_level, shape = org.releases)) 


ggplot(clean_responses) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1), aes(x = org.employees, y = roadmap.DEEPScore, colour = roadmap.happiness, shape = org.releases)) 

ggplot(clean_responses) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1), aes(x = org.industry, y = roadmap.DEEPScore, colour = roadmap.happiness, shape = org.releases)) 

ggplot(clean_responses) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1), aes(x = org.prodteamsize, y = roadmap.DEEPScore, colour = roadmap.happiness, shape = org.releases))