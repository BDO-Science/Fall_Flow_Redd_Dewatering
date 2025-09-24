library(tidyverse)
library(here)
library(patchwork)
library(ggrepel)

project <- here::here()

source(here::here(project, 'source_code/import_clean.R'))

###############################
#fall flow dewatering estimates
###############################

#import fall-run spawn info
spawn <- read.csv(here::here(project, 'input_data/model_inputs/spawnTimingAll.csv')) %>% 
  mutate(Date = as.Date(doy, origin = as.Date(paste0(yr,'-01-01')))) %>% #converts Julian Day to date for current year
  rename('EmergDays' = 'EmergDate') %>% #renaming for later use
  filter(run == 'fall') #filtering just for fall-run

#import gard look up table
fall_lookup <- read.csv(here::here(project,'input_data/model_inputs/model_dewater_flows.csv')) %>%
  rename('GardDewater' = 'Dewater', 'GardSpawn' = 'Spawn_Flows') %>% 
  filter(Run == 'Fall')

#function for converting flows to nearest number in Gard lookup
round_to_nearest <- function(x, basis_vector) {
  nearest_value <- basis_vector[which.min(abs(basis_vector - x))]
  return(nearest_value)
}

#transform kesFlowReal dataframe for model
redd_model <- scens_with_rt_flows %>% 
  pivot_longer(names_to = 'Scenarios', values_to = 'Spawn_Flows', -1) %>%
  mutate(Boards = if_else(Date < as.Date(paste0(yr,'-11-01')), 'in', 'out')) %>% 
  mutate(Spawn_Flows = as.numeric(Spawn_Flows)) %>% 
  left_join(spawn, by = c('Date')) %>% 
  filter(!is.na(run)) %>%
  mutate(Spawn_Flows = zoo::na.approx(Spawn_Flows), EmergDate = Date + incubDays) %>%
  mutate(Dewater = sapply(EmergDate, function(edate) {
    if (!is.na(edate)) {
      min(Spawn_Flows[Date <= edate & Date >= Date], na.rm = TRUE)
    } else {
      NA
    }
  }))

redd_model_final <- redd_model %>%
  mutate(GardSpawn = sapply(redd_model$Spawn_Flows, round_to_nearest, fall_lookup$GardSpawn)) %>%
  mutate(GardDewater = sapply(redd_model$Dewater, round_to_nearest, fall_lookup$GardDewater)) %>%
  left_join(fall_lookup, by = c('GardDewater', 'GardSpawn', 'Boards')) %>%
  mutate(Prop_dewater = if_else(is.na(Prop_dewater), 0, Prop_dewater)) %>%
  mutate(TotalDewater = prop * Prop_dewater)

fall_dewater_summary <- redd_model_final %>%
  group_by(Scenarios, year) %>% #summarize total dewatering by Scenario
  summarize(Redds_lost = round((sum(TotalDewater)*100), 1)) %>%
  pivot_wider(names_from = 'Scenarios', values_from = 'Redds_lost') %>%
  filter(year > 2013)

fall_dewater_mean <- fall_dewater_summary %>%
  summarize(across(-1, ~mean(.x, na.rm = TRUE))) %>%
  mutate(year = "Mean", .before = 1)

all_dewater_table <- fall_dewater_summary %>%
  datawizard::data_transpose(colnames = TRUE) %>%
  rownames_to_column(var = 'Scenario')

fall_dewater_for_table <- fall_dewater_mean %>%
  select(-1) %>%
  mutate(across(everything(), ~ round(.x, 1)))
  
####various optional graphs
# min_year <- min(as.numeric(fall_dewater_summary$year), na.rm = TRUE)
# 
# spawn_graph_2 <- ggplot() +
#   geom_boxplot(spawn, mapping = aes(x = factor(year), y = Date, fill = factor(year)),
#                color = 'black') +
#   scale_fill_viridis_d() +
#   labs(x = 'Spawn Year') +
#   coord_flip() +
#   theme_bw() +
#   theme(legend.position = 'none')
# spawn_graph_2
# 
# fall_dewater_summary_test <- fall_dewater_summary %>%
#   mutate(C1 = B1, D1 = B1, E1 = B1, F1 = B1, G1 = B1)
# dewater_graph <- fall_dewater_summary_test %>%
#   pivot_longer(names_to = 'Scenarios', values_to = 'dewater', -1) %>%
#   ggplot(aes(x = Scenarios, y = dewater, fill = Scenarios)) +
#   scale_fill_viridis_d(option = 'turbo') +
#   #geom_tile(width = 0.9, height = 0.9, color = 'black') +
#   #geom_label(aes(label = dewater), fill = 'white', size = 3) +
#   geom_col(color = 'black') +
#   facet_wrap(~year, ncol = 1) +
#   scale_y_continuous(position = 'right') +
#   labs(y = 'Redds Dewatered (%)') +
#   #coord_flip() +
#   theme_bw() +
#   theme(legend.position = 'none',
#         strip.background = element_blank(),
#         strip.text = element_blank())
# dewater_graph
# 
# final_graph <- (spawn_graph_2|dewater_graph) + plot_layout(widths = c(4.5,1.5))
# final_graph
# #ggsave(plot = final_graph, file = 'test_graph.png', height = 9, width = 10)
# spawning_cum_half <- spawn %>%
#   mutate(cum_round = round(cumul_prop, 1)) %>%
#   filter(cum_round == 0.5) %>%
#   group_by(year) %>%
#   slice_head(n = 1)

# spawn_graph <-  ggplot() +
#   geom_line(filter(spawn, year >= min_year), 
#             mapping = aes(x = Date, y = cumul_prop),
#             color = 'steelblue3',
#             linewidth = 1) +
#   geom_point(filter(spawning_cum_half, year >= min_year), 
             #mapping = aes(x = Date, y = 0.50),
             #size = 2,
             #shape = 8,
             #stroke = 2) +
  #geom_text(filter(spawning_cum_half, year >= min_year), 
            #mapping = aes(x = Date-15, y = 0.50+0.12, label = format(Date, '%b %d'))) +
  #labs(y = 'Cumulative Spawning Proportion') +
  #facet_wrap(~year, ncol = 2) +
  #theme_bw()
#spawn_graph

##########################
#expansion factor
##########################

pop_tab <- data.frame(Expansion = seq(1,3.5,0.5)) %>%
  mutate(Redds = round(reddCount*Expansion, 0),
         "Dewatering Threshold (1%)" = round(Redds*0.01,0)) %>%
  rename("Expansion Factor" = Expansion,
         "Total Redds" = Redds)

######################
#winter-run dewatering
######################
wr_min_flow <- data.frame()

redds_ok <- redds %>%
  filter(status == 'OK')

redds_total <- nrow(redds)
redds_dewatered <- nrow(subset(redds, grepl("dewater", status, ignore.case = TRUE)))
redds_emerged <- nrow(subset(redds, grepl("emerged", status, ignore.case = TRUE)))

for(i in 1:nrow(redds_ok)) {
  temp <- filter(scens_with_rt_flows, Date <= redds$emergence_date[i] 
                 & Date >= redds$date_established[i]) %>%
    summarize(across(-1, min))
  wr_min_flow <- bind_rows(wr_min_flow, temp)
}

redds2 <- bind_cols(redds_ok, wr_min_flow) %>%
  mutate(dewater_flow_250_buffer = dewater_flow + 250) %>%
  pivot_longer(names_to = 'Scenarios', values_to = 'min_flow', 6:(ncol(.)-1)) %>%
  mutate(dewater = if_else(dewater_flow > min_flow, 1, 0),
         dewater_250_buffer = if_else(dewater_flow_250_buffer > min_flow, 1, 0))

wr_dewater <- redds2 %>% group_by(Scenarios) %>%
  summarize(dewater = sum(dewater) + redds_dewatered,
            dewater_250_buffer = sum(dewater_250_buffer) + redds_dewatered) %>%
  ungroup() %>%
  mutate(dewater_perc = round((dewater/reddCount)*100,1),
         dewater_perc_exp = round((dewater/(reddCount*exp_fac)*100),1),
         dewater_perc_250_buffer = round((dewater_250_buffer/(reddCount)*100),1)) %>%
  select(1,2,4,5,3,6) %>%
  datawizard::data_transpose(colnames = TRUE,
                             rownames = NULL)
rownames(wr_dewater) <- NULL
##########################
#volume and flow summaries
##########################
avg_flow_sep_oct <- scens_with_rt_flows %>%
  mutate(month = month(Date)) %>%
  filter(month %in% c(9,10)) %>%
  group_by(month) %>%
  summarize(across(-1,mean)) %>%
  select(-1) %>%
  mutate(across(everything(), ~ round(.x, 0)))

sep_feb_volume <- scens_with_rt_flows %>%
  filter(Date > paste0(yr,'-08-31')) %>%
  summarize(across(-1, ~ sum(.x) * 1.983 / 1000)) %>%
  mutate(across(everything(), ~ round(.x, 0)))

aug_sep_volume <- scens_with_rt_flows %>%
  filter(Date <= paste0(yr,'-09-30')) %>%
  summarize(across(-1, ~ sum(.x) * 1.983 / 1000)) %>%
  mutate(across(everything(), ~ round(.x, 0)))

##########################
#creating summary table
##########################
#create column of measures
Metric <- data.frame(Measure = c("Avg Sept Flow (cfs)", "Avg Oct Flow (cfs)", 
            "Sept-Feb Total Volume (TAF)", "Aug-Sept Total Volume (TAF)", "Winter-run Redds Dewatered", 
            "Winter-run % Lost (expansion factor = 1)", paste0("Winter-run % Lost (expansion factor = ",exp_fac,")"), 
            "Winter-run Redds Dewatered (250 cfs buffer)", "Winter-run % Lost (250 cfs buffer)",
            "Fall-run % Redds Dewatered"))

summary_table <- bind_rows(avg_flow_sep_oct, sep_feb_volume, aug_sep_volume,
                           wr_dewater, fall_dewater_for_table) %>%
  bind_cols(Metric) %>%
  select(ncol(.), 1:ncol(.))

summary_table_flow <- slice(summary_table, 1:4) %>%
  rename('Volume Measures' = 'Measure')

summary_table_counts <- slice(summary_table, 5,8) %>%
  rename('Dewatering Count Measures' = 'Measure')

summary_table_percent <- slice(summary_table, 6,7,9,10) %>%
  rename('Dewatering % Measures' = 'Measure')

##########################
#dewatering graph
##########################
ymin <- plyr::round_any(min(redds$dewater_flow), 1000, f = floor)
ymax <- plyr::round_any(max(scens_with_rt_flows[2:3])+400, 500, f = ceiling)
todays_date <- Sys.Date()
mid <- as.Date(paste0(yr,'-08-01')) + floor((todays_date - as.Date(paste0(yr,'-08-01')))/2)

redds_graph <- redds %>% group_by(emergence_date, status, dewater_flow) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(status = case_when(grepl('dewater', status, ignore.case = TRUE) ~ 'DEWATER',
                            grepl('ok', status, ignore.case = TRUE) ~ 'OK',
                            grepl('emerge', status, ignore.case = TRUE) ~ 'EMERGED')) %>%
  mutate(status = factor(status, 
                         levels = c('OK', 'EMERGED', 'DEWATER'),
                         labels = c('Re', 'Em', 'De')))

flows <- scens_with_rt_flows %>% 
  pivot_longer(names_to = 'Alts', values_to = 'Flow', -1) %>%
  filter(Date >= max(rt_flows$date))

redd_graph <- ggplot() + geom_line(flows, mapping = aes(x = Date, y = Flow, color = Alts), linewidth = 0.75) +
  geom_line(rt_flows, mapping = aes(x = date, y = flow, linetype = location), linewidth = 0.75) +
  geom_point(redds_graph, mapping = aes(x = emergence_date, y = dewater_flow, fill = status), 
             shape = 21, size = 6, color = 'black',
             ) +
  geom_text(redds_graph, mapping = aes(x = emergence_date, y = dewater_flow, label = count),
            color = 'black') +
  ylim(ymin, ymax+200) +
  xlim(min(as.Date(paste0(yr,'-08-01'))), (max(redds$emergence_date) + 5)) +
  labs(x = 'Date', y = 'Flow (cfs)', fill = 'Redd Status', linetype = '') +
  theme_bw() +
  theme(legend.position = 'bottom', 
        legend.text = element_text(size = 12), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.key.size = unit(0.3, 'cm')) +
  scale_fill_manual(values = c(Re = 'lightgrey', Em = 'steelblue3', De = 'darkorange')) +
  annotate(geom = 'rect', xmin = as.Date(paste0(yr,'-08-01')), xmax = max(rt_flows$date), 
           ymin = 3000, ymax = max(rt_flows$flow) + 100, fill = 'darkgrey', color = 'black', alpha = 0.2, linetype = 'dotted') +
  annotate(geom = 'text', x = (mid + 0.5), y = max(rt_flows$flow) + 350, size = 3.5, 
           fontface = 'italic', label = 'Actual Flows')
redd_graph

##########################
#EOS analysis
##########################
projected_90_cost <- (sum(forecast_90_flows$flow) * 1.983) / 1000

eos_scen <- aug_sep_volume %>%
  pivot_longer(names_to = 'Scenario', values_to = 'Vol', 1:ncol(aug_sep_volume)) %>%
  mutate(eos = eosStorage - (Vol-projected_90_cost))
eos_scen_filter <- eos_scen %>%
  filter(eos <= 2200) %>%
  pull(Scenario)
eos_scen_paste <- paste(eos_scen_filter, collapse = ", ")
roundeosStorage <- plyr::round_any(eosStorage, 100)/1000
carryover_safe <- paste('All proposed scenarios are anticipated to have EOS storage greater than the 2,200 TAF threshold and therefore would not be',
                        'expected to contribute to TDM impacts to winter-run chinook salmon in the subsequent year.',
                        'Scenarios focused on avoiding dewatering of winter-run redds have higher releases',
                        'through early November which is not factored into this performance indicator.')
carryover_bad <- paste('scenarios',eos_scen_paste, 'are anticipated to have EOS storage less than the 2,200 TAF threshold and therefore may',
                       'contribute to TDM impacts to winter-run chinook salmon in the subsequent year.',
                       'Scenarios focused on avoiding dewatering of winter-run redds have higher releases',
                       'through early November which is not factored into this performance indicator.')

carryover_text <- if(length(eos_scen_filter) == 0) {
  print(carryover_safe)
} else {
  print(carryover_bad)
}
