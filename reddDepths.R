library(tidyverse)
library(ggplot2)
library(readxl)

files <- list.files(path = 'External_data/ShallowRedds', pattern = 'xlsx')

redds <- read_excel(paste0('External_data/ShallowRedds/',max(files)), sheet = 'SACPAS Shallow Redds')

redds2 <- redds %>%
  group_by(Redd_ID) %>%
  filter(all(status == "OK")) %>%
  summarise(
    min_depth = min(measurement_depth, na.rm = TRUE),
    has_trample = any(str_detect(comments, regex('trample', ignore_case = TRUE)))
  ) %>%
  mutate(Risk = case_when(
    min_depth <= 5 & has_trample ~ 3,
    min_depth <= 5 ~ 1,
    has_trample ~ 2,
    TRUE ~ 0
  )) %>%
  right_join(redds, by = "Redd_ID") %>%
  ungroup() %>%
  arrange(Redd_ID, measurement_date) %>%
  mutate(measurement_date = as.Date(measurement_date)) %>%
  group_by(Redd_ID) %>%
  filter(all(status == "OK")) %>%
  ungroup()

depth <- ggplot(redds2, aes(x = measurement_date, y = measurement_depth, group = Redd_ID)) + 
  geom_rect(aes(fill=factor(Risk)), xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha=0.07) +
  geom_point(fill = '#999999') +
  geom_line(linetype = 'dashed', color = '#666666') +
  #geom_smooth(aes(color = as.factor(status)), method = "loess", span = 0.6, se = FALSE) +
  facet_wrap(~ Redd_ID, nrow = 4) +
  labs(x = 'Date', y = 'Water Depth (in)', fill = 'Risk') +
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y.right=element_text(margin = margin(l = 10)),
        plot.margin = margin(.1, .1, .001, .1, "cm")) +
  scale_x_date(date_breaks = '2 months', date_labels = '%b') +
 # scale_linetype_manual(name = '', values = c('dotted', 'longdash'), labels = c('Dewatering Flow (CFS)', 'KWK Flows (CFS)')) +
  scale_fill_manual(values = c('0' = 'NA', '1' = '#990000', '2' = '#FF9900', '3' = '#CC33CC'), 
                    labels = c('0' = 'Minimal', '1' = 'Dewater Risk', '2' = 'Trampled', '3' = 'Dewater Risk/Trampled')) +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))
depth

