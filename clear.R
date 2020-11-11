library(tidyverse)
library(magrittr)
library(lubridate)
femaRaw <- read.csv("PublicAssistanceFundedProjectsDetails.csv")

hurricane <- femaRaw %<>% dplyr::select(disasterNumber, declarationDate, incidentType, pwNumber, damageCategory, projectSize, county, countyCode, state, stateCode, stateNumberCode, projectAmount, federalShareObligated, totalObligated)  


hurricane <- femaRaw %>% dplyr::filter(incidentType == "Hurricane")  
hurricane %<>% separate(declarationDate, c('date', 'time'), sep = "T")

hurricane$date <- as.Date(hurricane$date, format = "%Y-%m-%d")

hurricane %<>% dplyr::filter(year(hurricane$date) > 2008)
hurricane %<>% select(-c(time, incidentType))

write.csv(hurricane, "hurricane.csv")
