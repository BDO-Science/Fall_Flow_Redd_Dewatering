library(digitize)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggpmisc)
data <- digitize('carryover_tdm_53.png')
data <- data[,2]
data <- round(data, 0)
data2 <- expand.grid(Carryover = c(800, 1000, 1200, 1300, 1500, 1600, 1800, 1900, 
                                   2000, 2100, 2200, 2300, 2400, 2600, 2800, 3000, 3200, 3400),
                     Value = c('Median', '75_perc', '90_perc'))

data2 <- cbind(data2, data)
data2$data <- if_else(data2$data < 5, 0, data2$data)
temp_53 <- data2 %>% spread(key = Value, value = data) %>% mutate(Temp_F = 53)

all_data <- rbind(temp_53, temp_54, temp_55)

write.csv(all_data, 'TDM_data.csv')


