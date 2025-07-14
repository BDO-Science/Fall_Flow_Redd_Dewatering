library(dplyr)
library(tidyverse)

# query parameters for SacPAS
# calendar year
queryYear=2023
# query dates can be empty string "" or explicit "mm/dd" (value will be NA for future dates or missing dates)
# empty string for end date will result in data through most recent date in database for given year
querySD="9/1"
queryED=""
outputFormat="csvSingle"

# KWK river flow data from SacPAS
queryKWKflow=paste("https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?mgconfig=river&loc[]=KWK&data[]=Flow&outputFormat=",outputFormat,"&year[]=",queryYear,"&startdate=",querySD,"&enddate=",queryED, sep="")
kwkFlowData = read.csv(queryKWKflow)
kwkFlowData=dateRowsOnly.fn(kwkFlowData,outputFormat) # trim query results to date data rows only

# KES reservoir flow data from SacPAS
queryKESflow=paste("https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?mgconfig=river&loc[]=KES&data[]=ReservoirOutflow&outputFormat=",outputFormat,"&year[]=",queryYear,"&startdate=",querySD,"&enddate=",queryED, sep="")
kesFlowData = read.csv(queryKESflow)
kesFlowData=dateRowsOnly.fn(kesFlowData,outputFormat) # trim query results to date data rows only

allflows <- rbind(kwkFlowData, kesFlowData)
allflows <- allflows %>% 
  mutate(Date = as.Date(paste0(mm.dd,'-',year), format = '%m-%d-%Y')) %>% 
  rename('Flow' = 'value') %>% select(Date, Gage = location, Flow)
  












