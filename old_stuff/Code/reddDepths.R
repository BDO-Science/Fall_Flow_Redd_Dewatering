library(tidyverse)
library(ggplot2)
library(readxl)
library(plotly)
library(htmlwidgets)

files <- list.files(path = 'External_data/ShallowRedds', pattern = 'xlsx')
maxFile <- max(files)
redds <- read_excel(paste0('External_data/ShallowRedds/',maxFile), sheet = 'SACPAS Shallow Redds') %>%
  mutate(Emerge = if_else(Estimated_Emergence <= Sys.Date(), 1, 0)) %>%
  filter(Emerge == 0)

reddDate <- redds %>%
  summarize(format(max(measurement_date), '%B %d, %Y')) %>% pull()
title <- paste0('Shallow water redd monitoring as of ',reddDate)

redds2 <- redds %>%
  group_by(Redd_ID) %>%
  summarise(
    min_depth = min(measurement_depth, na.rm = TRUE),
    has_trample = any(str_detect(comments, regex('trample', ignore_case = TRUE)))
  ) %>%
  mutate(Risk = case_when(
    min_depth <= 10 & has_trample ~ 3,
    min_depth <= 10 ~ 1,
    has_trample ~ 2,
    TRUE ~ 0
  )) %>%
  right_join(redds, by = "Redd_ID") %>%
  ungroup() %>%
  arrange(Redd_ID, measurement_date) %>%
  mutate(measurement_date = as.Date(measurement_date)) %>%
  group_by(Redd_ID) %>%
  filter(all(status == "OK")) %>%
  ungroup() %>%
  mutate(days_to_emerg = difftime(Estimated_Emergence, measurement_date, unit = 'days')) %>%
  mutate(Run = case_when(grepl('W', Redd_ID) ~ 'Winter',
                         grepl('F', Redd_ID) ~ 'Fall',
                         grepl('S', Redd_ID) ~ 'Spring'))

filter <- redds2 %>%
  group_by(Redd_ID) %>%
  summarize(n = n())

redds3 <- redds2 %>%
  left_join(filter, by = 'Redd_ID') %>%
  filter(n > 1 & Run %in% c('Winter', 'Fall')) %>%
  filter(Estimated_Emergence > Sys.Date())

risk <- redds3 %>%
  group_by(Redd_ID) %>%
  summarize(Risk = max(Risk))
depth <- ggplot() + 
  geom_rect(risk, mapping = aes(fill=factor(Risk)), xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha=0.25, color = 'black') +
  geom_point(redds3, 
             mapping = aes(x = measurement_date, y = measurement_depth, group = Redd_ID), 
             fill = '#999999') +
  geom_line(redds3, 
            mapping = aes(x = measurement_date, y = measurement_depth, group = Redd_ID),
            linetype = 'dashed', color = '#666666') +
  #geom_smooth(aes(color = as.factor(status)), method = "loess", span = 0.6, se = FALSE) +
  facet_wrap(~ Redd_ID, nrow = 4) +
  labs(x = 'Date', y = 'Water Depth (in)', fill = 'Risk', title = title) +
  theme_bw() +
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y.right=element_text(margin = margin(l = 10)),
        plot.margin = margin(.1, .1, .001, .1, "cm")) +
  scale_x_date(date_breaks = '1 months', date_labels = '%b') +
 # scale_linetype_manual(name = '', values = c('dotted', 'longdash'), labels = c('Dewatering Flow (CFS)', 'KWK Flows (CFS)')) +
  scale_fill_manual(values = c('0' = 'NA', '1' = '#990000', '2' = '#FF9900', '3' = '#CC33CC'), 
                    labels = c('0' = 'Minimal', '1' = 'Dewater Risk (<= 10 in)', '2' = 'Trampled', '3' = 'Dewater Risk/Trampled')) +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))
depth
ggsave(plot = depth, filename = 'shallow_redd_depths.png', width = 9, height = 6.5, units = 'in')


interactive_plot <- ggplot(redds2, aes(x = measurement_date, y = measurement_depth, group = Redd_ID)) + 
  geom_point(aes(color = measurement_depth, 
                 text = paste("Run:", Run,
                   "<br>Depth (in):", measurement_depth,
                              "<br>Days to Emergence:", days_to_emerg,
                              "<br>Measurement Date:", format(measurement_date, "%B %d, %Y"),
                              "<br>Emergence Date: ", format(Estimated_Emergence, "%B %d, %Y"),
                              "<br>Location:", location)), size = 2)+
  geom_line(linetype = 'dashed', color = '#666666', alpha = 0.7) +
  facet_wrap(~ Redd_ID, nrow = 4) +
  labs(x = 'Date', y = 'Water Depth (in)', color = 'Depth (in)', title = title) +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)),
        plot.margin = margin(.25, .25, .25, .25, "cm"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  scale_x_date(date_breaks = '1.5 months', date_labels = '%b') +
  scale_colour_gradient(low = 'red', high = 'green')
  #scale_color_manual(values = c('0' = 'NA', '1' = '#990000', '2' = '#FF9900', '3' = '#CC33CC'), 
                     #labels = c('0' = 'Minimal', '1' = 'Dewater Risk', '2' = 'Trampled', '3' = 'Dewater Risk/Trampled')) +
interactive_plot
# Convert ggplot to plotly
interactive <- ggplotly(interactive_plot, tooltip = c("text")) %>%
  layout(
    margin = list(l = 60, r = 40, b = 60, t = 55),  # Adjust left, right, bottom, top margins
    autosize = TRUE  # Allow the plot to automatically resize
  )
interactive
saveWidget(interactive, file = "interactive_plot.html")

