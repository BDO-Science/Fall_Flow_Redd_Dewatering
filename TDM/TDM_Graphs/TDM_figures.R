library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)

###########################
#Figure 1 code
tdm_53 <- read_excel('Raw_data/tdm_summary_53.xlsx')
tdm_54 <- read_excel('Raw_data/tdm_summary_54.xlsx')
tdm_55 <- read_excel('Raw_data/tdm_summary_55.xlsx')

tdm_53 <- tdm_53 %>% mutate(Temp = 53)
tdm_54 <- tdm_54 %>% mutate(Temp = 54)
tdm_55 <- tdm_55 %>% mutate(Temp = 55)

data <- rbind(tdm_53, tdm_54, tdm_55)
data <- data %>% gather(key = 'Carryover', value = 'TDM', 2:19)
write.csv(data, 'TDM_all.csv', row.names = FALSE)

q <- c(.10, .25, .5, .75, .9)

summary <- data %>%
  group_by(Temp, Carryover) %>%
  summarize(quant10 = quantile(TDM, probs = q[1]), 
            quant25 = quantile(TDM, probs = q[2]),
            quant50 = quantile(TDM, probs = q[3]),
            quant75 = quantile(TDM, probs = q[4]),
            quant90 = quantile(TDM, probs = q[5]),
            median = median(TDM)
            )

summary <- summary %>% mutate(Med_per = (median/sum(median)*100))
summary[,3:7] <- round(summary[3:7], 2)
write.csv(summary, 'TDM_summary.csv', row.names = FALSE)

data_54 <- data %>% filter(Temp == 54)
data_54$Carryover <- as.numeric(data_54$Carryover)
TDM_54 <- ggplot(data_54, aes(x = factor(Carryover), y = TDM)) + 
  geom_boxplot(fill = 'steelblue3', color = 'black', alpha = 0.75) + labs(x = 'Carryover (TAF)', y = 'TDM (Martin)') +
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.text.x = element_blank()) +
  geom_vline(xintercept = factor(1600), linewidth = 1.25, color = 'darkgreen', linetype = 'dashed', alpha = 0.5) +
  geom_vline(xintercept = factor(2200), linewidth = 1.25, color = 'purple', linetype = 'dashed', alpha = 0.5)
TDM_54

summary_54 <- summary %>% filter(Temp == 54)
Median_54 <- ggplot(summary_54, aes(x = Carryover, y = Med_per, group = 1)) + geom_line(color = 'red', linewidth = 1) +
  geom_vline(xintercept = factor(1600), linewidth = 1.25, color = 'darkgreen', linetype = 'dashed', alpha = 0.5) +
  geom_vline(xintercept = factor(2200), linewidth = 1.25, color = 'purple', linetype = 'dashed', alpha = 0.5) +
  labs(x = 'Carryover (TAF)', y = 'Median (%)') + 
  theme(legend.position = 'none',
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 16)),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.40),
                           "cm"))
Median_54

final_graph_54 <- ggarrange(TDM_54, Median_54, 
                         ncol = 1, nrow = 2)
final_graph_54

ggsave(plot = final_graph_54, file = 'Figure_1.png')

#################################
#Figure 2 code
all_data <- read_excel('Raw_data/tdm_summary.xlsx')

all_data <- all_data %>% gather(key = 'Carryover', value = 'TDM', 2:19) %>% rename('Year' = 1)


summary <- all_data %>%
  group_by(Carryover) %>%
  summarize(median = median(TDM)) %>% mutate(Med_per = (median/sum(median)*100))

TDM <- ggplot(all_data, aes(x = factor(Carryover), y = TDM)) + 
  geom_boxplot(fill = 'steelblue3', color = 'black', alpha = 0.75) + labs(x = 'Carryover (TAF)', y = 'TDM (Martin)') +
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.text.x = element_blank()) +
  geom_vline(xintercept = factor(2200), linewidth = 1.25, color = 'darkgreen', linetype = 'dashed', alpha = 0.5)
TDM

Median <- ggplot(summary, aes(x = Carryover, y = Med_per, group = 1)) + geom_line(color = 'red', linewidth = 1) +
  geom_vline(xintercept = factor(2200), linewidth = 1.25, color = 'darkgreen', linetype = 'dashed', alpha = 0.5) +
  labs(x = 'Carryover (TAF)', y = 'Median (%)') + 
  theme(legend.position = 'none',
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 16)),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.40),
                           "cm"))
Median

final_graph <- ggarrange(TDM, Median, 
                         ncol = 1, nrow = 2)
final_graph

ggsave(plot = final_graph, file = 'Figure_2.png')


