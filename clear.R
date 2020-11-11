library(tidyverse)
library(magrittr)
femaRaw <- read.csv("PublicAssistanceFundedProjectsDetails.csv", col.names = TRUE)

hurricane <- femaRaw %>% dplyr::select(disasterNumber, declarationDate, incidentType, pwNumber, damageCategory, projectSize, county, countyCode, stateCode, stateNumberCode, projectAmount, federalShareObligated, totalObligated)  


hurricane <- femaRaw %>% dplyr::filter(incidentType == "Hurricane")  
hurricane %<>% separate(declarationDate, c('date', 'time'), sep = "T")
hurricane %<>% mutate(year = date[0:4])
hurricane %<>% separate(date, c('yyyy', 'mm', 'dd'), sep = '-')
hurricane$yyyy <- as.numeric(hurricane$yyyy)
hurricane %<>% filter(yyyy > 2008)
hurricane %<>% select(-c(mm, dd, time, incidentType))

write.csv(hurricane, "hurricane.csv")
