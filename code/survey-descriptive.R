## survey-descriptive.R
##    first run survey-pre-process.R to create the clean data set and then 
##    survey-modelling.R to load and build the data model.
##
## Acknowledgements
##  * Map code based off the example at https://www.r-bloggers.com/2019/03/all-around-the-world-maps-and-flags-in-r/ by quantixed
##
if (!require("rworldmap")) {
  install.packages("rworldmap")
  library(rworldmap)
}
# ggplot2, dplyr are needed for the bar charts and rmarkdown to produce the word doc
library(ggplot2)
library(dplyr)
library(rmarkdown)



countries_responded <- clean_responses %>%
  select(country = org.location) %>%
  group_by(country) %>%
  summarise(value = n())
  
matched <- joinCountryData2Map(countries_responded, joinCode="NAME", nameJoinColumn="country", verbose = TRUE) 



# make png of the map
png(file = "../outputs/ResponsesMap.png",
    width = 1024, height = 768)
par(mai=c(0,0,0.2,0))
mapCountryData(matched,
               nameColumnToPlot="value",
               mapTitle= "Location of respondents",
               catMethod = "logFixedWidth",
               colourPalette = "heat",
               oceanCol="lightblue",
               missingCountryCol="white",
               addLegend = FALSE,
               lwd = 1)

dev.off()



summary(clean_responses[3:8])
##### create report
render("survey-visulisation.Rmd", "all", output_dir = "../outputs")
render("survey-visulisation.Rmd", "html_document", output_dir = "../docs", output_file = "index")

summary(clean_responses[18:26])

matched <- NULL
countries_responded <- NULL
myplot <- NULL

prod_responses <- clean_responses %>%
  filter(grepl('Product', Job.title)) 

nonprod_responses <- clean_responses %>%
  filter(!grepl('Product', Job.title)) 
