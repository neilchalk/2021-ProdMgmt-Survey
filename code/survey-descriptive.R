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
# ggplot2, ggFlags, dplyr are needed for the bar charts
library(ggplot2)
library(dplyr)
if (!require("ggflags")) {
  devtools::install_github("rensa/ggflags")
  library(ggflags)
}
library(rmarkdown)

countries_responded <- clean_responses %>%
  select(country = org.location) %>%
  group_by(country) %>%
  summarise(value = n())
  
matched <- joinCountryData2Map(countries_responded, joinCode="NAME", nameJoinColumn="country")

# make png of the map
png(file = "../outputs/ResponsesMap.png",
    width = 1024, height = 768)
par(mai=c(0,0,0.2,0))
mapCountryData(matched,
               nameColumnToPlot="value",
               mapTitle= "",
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

summary(clean_responses[18:26])



