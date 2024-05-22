library("readxl")
library("stringr")
library("dplyr")

spreadsheetName="C:/Users/cehlo/OneDrive - DOI/Desktop/Fall Flows.xlsx"

# note: in order to get this code to work, we had to provide the "range" of column A in the spreadsheet, set it LARGE at 1000
# read worksheet "Scenario Description" in Fall Flows spreadsheet into dataframe
worksheetSD <- read_xlsx(spreadsheetName, sheet = "Scenario Description",range="A1:A1000",col_names=F,col_types="text")
# remove extraneous lines, keeping lines with "=" in it
keep=apply(worksheetSD,2,function(x.in) str_detect(x.in,"="))
worksheetSD=worksheetSD[keep,]
worksheetSD=worksheetSD[!is.na(worksheetSD[,1]),]
# split the Alts from the descriptions
num.in = dim(worksheetSD)[1]
scenarioDesc=data.frame(matrix(nrow=num.in,ncol=2))
colnames(scenarioDesc)=c("Scenario","Descripion")
# loop through dataframe, split original dataframe column 1 on literal "=" characters and trim leading and trailing space, create new dataframe with 2 columns "Scenario" and "Description"
# populates scenarioDesc dataframe
for(i in 1:num.in){
	scenarioDesc[i,]=str_trim(str_split(worksheetSD[i,1],"=")[[1]])
}

#use scenarios defined in first chunk of code to filter the scenarioDesc dataframe
scenarios <- c("Alt.1c", "Alt.2e", "Alt.3l", "Alt.3m", "Alt.3p", "Alt.3q") #example group of scenarios
scenarios <- str_replace(scenarios, 'Alt.', '') #parse down to include just the scenario name

#filter based on scenario name
scenarioDesc <- scenarioDesc %>% 
  filter(grepl(paste(toupper(scenarios), collapse ='|'), toupper(Scenario))) #convert to uppercase within filter to cover inconsistencies in upper and lower cases.
