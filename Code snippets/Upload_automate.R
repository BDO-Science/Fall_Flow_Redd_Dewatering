library(readxl)
library(dplyr)

source_folder <- 'Winter Run data files/' #select source folder
destination_folder <- 'Winter Run data files/Processed' #select destination folder
file_names <- list.files(path = source_folder, pattern = "\\.xlsx$", full.names = TRUE) #list all excel files in source_folder

#list and filter sheets with 'Shallow' last night
sheets <- data.frame(excel_sheets(path = file_names))
sheet <- sheets %>% rename('name' = 1) %>% filter(grepl('SHALLOW', name, ignore.case = TRUE))

#read shallow redd sheet as a dataframe into R
redds <- read_excel(file_names,  sheet = sheet[1,1], 
                    range = cell_cols(c('A:H')))
redds <- redds %>% na.omit() %>% select(4,6,8) #minor cleaning to pull out desirable columns

#move file from source folder into sub-folder
new_file_path <- file.path(destination_folder, basename(file_names))
file.rename(file_names, new_file_path)
