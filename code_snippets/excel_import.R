library(readxl)
library(tidyverse)
library(dplyr)
Files <- list.files('Flow_alts/', pattern = "xlsx$")
MaxFile <- max(Files)

data <- read_excel(paste0('Flow_alts/',MaxFile), sheet = 'KES Flow all', skip = 1)

data <- data %>% filter(nchar(Date) == 5) %>% select(-2, -Timeline)

names <- colnames(data)
names
# Define a pattern to match the common part you want to keep
pattern <- "Alt\\s*(\\d+[a-z]?)(?:\\s*\\([^)]+\\))?"

# Extract and format the desired column names
new_column_names <- gsub(pattern, "Alt_\\1", names)
new_column_names <- gsub("-rice comp$", "", new_column_names)

colnames(data) <- new_column_names
data$Date <- as.Date(as.numeric(data$Date))
data <- data %>% mutate_if(is.character, as.numeric)
