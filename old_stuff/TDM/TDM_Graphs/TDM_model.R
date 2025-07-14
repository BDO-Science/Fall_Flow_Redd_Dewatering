library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(betareg)
library(ggtext)

################################
#Developing model
data <- read_excel('Raw_data/tdm_summary.xlsx')
q <- .75 #set q as 75th percentile
data <- data %>% rename('Year' = 1) %>% gather(key = 'Carryover', value = 'TDM', 2:19) %>% #gather into 2 columns
  group_by(Carryover) %>% summarize(median = median(TDM), #summarize by carryover for median
                                    quant75 = quantile(TDM, probs = q)) %>% #and 75th percentile
  mutate(Carryover = as.numeric(Carryover)) %>% #set carryover as numeric
  gather(key = 'measure', value = 'TDM', 2:3) #gather again for easier modeling

#fit a beta regression model for TDM based on Carryover and either median or 75th percentile
model <- betareg(TDM ~ Carryover * measure,
             data = data)
summary(model) #summary of model

#create a dataframe to predict and graph model
predictions <- expand.grid(Carryover = seq(800, 4000, 1), measure = c('median', 'quant75'))
predictions$pred <- predict(model, newdata = predictions, type = 'response')

#graph the dataframe to visually represent the model
ggplot(predictions, aes(x = Carryover, color = measure)) + geom_line(aes(y = pred), linewidth = 1.5) +
  labs(x = 'Carryover (TAF)', y = 'TDM') + 
  theme(plot.margin = margin(0.5,0.5,.75,.75, unit = 'cm'), legend.position = 'none',
        axis.title.y = element_text(vjust = +5),
        axis.title.x = element_text(vjust = -5))+
  annotate(geom = 'richtext', x = c(1590, 1700), y = c(0.10, 0.45), 
           label = c('Median', '75th Percentile'))

#######################################################
#fit model to flow scenarios
flows <- read.csv('Raw_data/kesFlow.csv')

flows <- flows %>% 
  mutate(Date = mdy(Date)) %>% #mutate Date column
  gather(key = 'Alts', value = 'Flow', -1) %>%  #gather all alts into simpler dataframe
  mutate(Volume = (Flow *1.98)/1000) %>% mutate(Alts = str_replace_all(Alts, 'Alt.', '')) #caculate TAF and simplify Alt names

#filter for September flows, sum total volume in TAF, and calculate carryover for each Alt by subtracting by 3485
summary <- flows %>% filter(Date >= '2023-09-01' & Date <= '2023-09-30') %>%
  group_by(Alts) %>% summarize(Volume = sum(Volume)) %>% 
  mutate(Carryover = 3485-Volume, 'median' = 'Median', 'quant75' = 'quant75') %>% #add median and quant75 for predict
  gather(key = 'measure', value = 'value', median, quant75) %>% dplyr::select(-5) #gather median and quant75 under 'measure' for predict

#run predict function
summary$pred2 <- predict(model, newdata = summary, type = 'response')

#create final_table and format to show TDM for every alt
final_table <- summary %>% mutate_if(is.numeric, round, digits = 3) %>%
  spread(key = 'measure', value = 'pred2') %>% rename('TDM (Median)' = 4, 'TDM (75th percentile)' = 5)

#export final table
write.csv(final_table, 'TDM_alts_raw.csv', row.names = FALSE)
