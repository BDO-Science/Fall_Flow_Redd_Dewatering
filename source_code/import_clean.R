library(tidyverse)
library(janitor)
library(here)
library(readxl)

project <- here::here() #pointing to working directory

#user defined values
wy <- 2025
exp_fac <- 2
yr <- year(Sys.Date())

##############################
#Reading in shallow redd files
##############################

#######Read in redd data from shallow winter redd monitoring
redd_files <- list.files(here::here(project, 'input_data/shallow_redds/'), pattern = "\\.xlsx$", full.names = TRUE) #list all excel files
MaxReddFile <- max(redd_files) #reads the latest redd excel file
reddsheet <- tibble(sheet = excel_sheets(MaxReddFile)) %>%
  filter(grepl('shallow',sheet,ignore.case = TRUE)) %>%
  filter(!grepl('sacpas',sheet,ignore.case = TRUE)) %>%
  pull()


redds <- read_excel(MaxReddFile,  sheet = reddsheet, #read in shallow redd file and clean up.
                    range = cell_cols(c('A:I'))) %>% 
  na.omit() %>% 
  select(date_established = 3,emergence_date = 4, status = 6, dewater_flow = 9) %>%
  mutate_at(1:2, as.Date)#minor cleaning

redds2 <- redds #putting in a spare dataframe or later use

#######################
#reading in scenarios
#######################
scen_files <- list.files(here::here(project, 'input_data/flow_scen/'), full.names = TRUE, pattern = "xlsx$")
MaxScenFile <- max(scen_files)
flowsheets <- excel_sheets(MaxScenFile)
flowsheet <- grep("Alternatives", flowsheets, ignore.case = TRUE, value = TRUE) #for pulling in flow alternatives
scensheet <- grep("Desired", ignore.case = TRUE, flowsheets, value = TRUE) #for pulling in desired scenarios sheet
scendesc <- grep("Description", ignore.case = TRUE, flowsheets, value = TRUE)

#pull in desired scenarios for filtering
scen_filter <- read_excel(MaxScenFile, 
                   sheet = scensheet, col_names = TRUE) %>%
  filter(Use == 'Y') %>% 
  pull(Scenario)

#pull in flow alternatives
scen_flow_import <- read_excel(MaxScenFile, 
                          sheet = flowsheet, skip = 1, col_names = TRUE) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
  pivot_longer(names_to = 'scenarios', values_to = 'flow', -1) %>%
  filter(scenarios %in% scen_filter,
         !is.na(Date))

#pull in scenario description
scen_descriptions <- read_excel(MaxScenFile,
                                sheet = scendesc, skip = 6) %>%
  separate(1, into = c('Scenario', 'Description'), sep = ':') %>%
  filter(Scenario %in% scen_filter)

###########################################
#read in Redd Count data and date from 
#most recent file with that info
#different folder than standard redd files
###########################################
count_pattern <- 'To date, unexpanded redd count' #set pattern for count cell to look for
date_pattern <- 'Through' #set pattern for date cell to look for
CountFiles <- list.files(here::here(project,'input_data/shallow_redds/ReddCount/'), 
                         pattern = "xlsx$", full.names = TRUE) #list files with reporting tab in them
MaxCountFile <- max(CountFiles) #single out the most recent file
sheetCount <- tibble(sheet = excel_sheets(MaxCountFile)) %>% #read in most recent excel WITH count data
  filter(grepl('REPORTING', sheet, ignore.case = TRUE)) %>%
  pull()

#for Count data
Count <- read_excel(MaxCountFile,  
                    sheet = sheetCount) #pull in sheet with count data
reddCount <- round(as.numeric(Count[[(which(Count[, 2] == 'To date, unexpanded redd count') + 1), 2]]),0) #isolate count
countDate <- format(as.Date(as.numeric(Count[[(which(Count[, 1] == 'Through') + 1), 1]]), 
                            origin = "1899-12-30"), "%B %d, %Y")

updatedReddInfoDate <- read_excel(MaxReddFile, 
                                  sheet = grep('sacpas', excel_sheets(MaxReddFile),
                                               ignore.case = TRUE)) %>%
  summarize(max(measurement_date)) %>% pull()

#########################################
#pull in flow data from kwk and kes
#merge real-time flow data with scenarios
#########################################
#kwk <- cdec_query('KWK', '41', 'D', paste0(wy-1,'-08-01'), Sys.Date())
#kes <- cdec_query('KES', '23', 'D', paste0(wy-1,'-08-01'), Sys.Date())

kwk_url <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csv&hafilter=All&year%5B%5D="
                  ,wy,"&loc%5B%5D=KWK&data%5B%5D=Flow&tempUnit=F&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large")

kes_url <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csv&hafilter=All&year%5B%5D="
                  ,wy,"&loc%5B%5D=KES&data%5B%5D=ReservoirOutflow&tempUnit=F&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large")

kwk <- read_csv(kwk_url) %>% clean_names() %>%
  mutate(location = 'KWK') %>%
  select(date = 1, 3,flow = 2,)
kes <- read_csv(kes_url) %>% clean_names() %>%
  mutate(location = 'KES') %>%
  select(date = 1, 3,flow = 2,)

rt_flows <- bind_rows(kwk,kes) %>%
  mutate(date = mdy(paste0(date,'/',wy))) %>%
  filter(!is.na(date)) %>%
  filter(date <= Sys.Date()) %>%
  filter(date >= as.Date('2025-08-01'))

kes_flow_bind <- rt_flows %>%
  filter(location == 'KES') %>%
  select(-2) %>%
  crossing(scenarios = scen_filter) %>%
  select(Date = 1,3,2)

scens_with_rt_flows <- filter(scen_flow_import, Date > max(kes_flow_bind$Date)) %>%
  bind_rows(kes_flow_bind) %>%
  pivot_wider(names_from = 'scenarios', values_from = 'flow')


