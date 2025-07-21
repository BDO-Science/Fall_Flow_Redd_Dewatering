library(leaflet)
library(leafpop)
library(readxl)
library(tidyverse)
library(gridExtra)
library(patchwork)
library(leaflet.extras2)
library(leaflet.extras)
library(janitor)

project <- here::here() #pointing to working directory
wy <- 2025
files <- list.files(here::here(project, 'input_data/shallow_redds/'), pattern = "\\.xlsx$", full.names = TRUE)
maxFile <- max(files)
redds <- read_excel(maxFile, sheet = 'SacPAS Shallow Redds') %>%
  mutate(Run = case_when(grepl('W', Redd_ID) ~ 'Winter',
                         grepl('F', Redd_ID) ~ 'Fall',
                         grepl('S', Redd_ID) ~ 'Spring')) %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude),
         measurement_date = ymd(measurement_date))
maxdepth <- max(redds$measurement_depth)
redd_names <- unique(redds$Redd_ID)
redd_list <- list()
min_depth <- if_else(min(redds$measurement_depth) < 0, min(redds$measurement_depth), 0)

for(i in redd_names){
  temp <- filter(redds, Redd_ID == i)
  redd_list[[i]] <- temp
}

kwk_url <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csv&hafilter=All&year%5B%5D="
                  ,wy,"&loc%5B%5D=KWK&data%5B%5D=Flow&tempUnit=F&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large")

kes_url <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csv&hafilter=All&year%5B%5D="
                  ,wy,"&loc%5B%5D=KES&data%5B%5D=ReservoirOutflow&tempUnit=F&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large")

kwk <- read_csv(kwk_url) %>% clean_names() %>%
  mutate(gage = 'KWK') %>%
  select(3, date = 1, flow = 2) %>% 
  mutate(date = mdy(paste0(date,'/',wy))) %>%
  filter(!is.na(date)) %>%
  filter(date <= Sys.Date() &
           date >= as.Date('2025-04-01'))

kes <- read_csv(kes_url) %>% clean_names() %>%
  mutate(gage = 'KES') %>%
  select(3, date = 1, flow = 2) %>%
  mutate(date = mdy(paste0(date,'/',wy))) %>%
  filter(!is.na(date)) %>%
  filter(date <= Sys.Date() &
           date >= as.Date('2025-04-01'))


plots <- lapply(redd_list, function(df){
  last <- df %>% slice(n())
  mid <- median(range(df$measurement_date))
  plot <- ggplot(df, aes(x = measurement_date, y = measurement_depth, 
                         fill = measurement_depth)) +
    #geom_line(linewidth = 1.5, alpha = 0.5) +
    geom_point(size = 5, shape = 21) +
    scale_fill_viridis_c(
      limits = c(min_depth, max(redds$measurement_depth, na.rm = TRUE)),
      direction = -1
    ) +
    #annotate(geom = 'text', x = mid, y = 5, label = text, fontface = 'bold', size = 4) +
    scale_y_reverse(limits = c(maxdepth, 0)) +
    geom_area(fill = 'steelblue2', alpha = 0.3) +
    labs(y = 'Redd Depth (in)', x = 'Date') +
    theme_bw() +
    theme(legend.position = 'none',
          axis.title.y = element_text(margin = margin(r = 8)),
          axis.title.x = element_text(margin = margin(t = 8)),
          plot.margin = margin(.25, .25, .25, .25, "cm"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14))
  table <- df %>%
    slice(n()) %>%
    select(3,'Most Recent Depth (in)' = 12, 'Date Measured' = 13, 'Emergence Date (estimate)' = 5,18,7)
  t_grob <- tableGrob(table,  rows = NULL)
  t_plot <- ggplot() +
    annotation_custom(t_grob) +
    theme_void()
  final_plot <- t_plot/plot + plot_layout(heights = c(0.5,1.5))
})


kwk_plot <- ggplot() +
  geom_line(data = kes, aes(x = date, y = flow, color = "KES"), linetype = 'dashed', linewidth = 1) +
  geom_line(data = kwk, aes(x = date, y = flow, color = "KWK"), alpha = 0.8, linewidth = 1) +
  #geom_line(data = kes, aes(x = date, y = flow, color = "KES"), linetype = 'dashed', linewidth = 1) +
  scale_color_manual(name = "Site", values = c("KWK" = "steelblue3", "KES" = "grey")) +
  #scale_linetype_manual(name = "Site", values = c("KWK" = "solid", "KES" = "dashed")) +
  labs(x = 'Date', y = 'Flow (cfs)') +
  scale_y_continuous(limits = c(0, max(kes$flow)+1000)) +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.title.x = element_text(margin = margin(t = 8)),
    plot.margin = margin(.25, .25, .25, .25, "cm"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )
kwk_plot

kes_plot <- ggplot() +
  geom_line(data = kwk, aes(x = date, y = flow, color = "KWK"), linetype = 'dashed', linewidth = 1) +
  geom_line(data = kes, aes(x = date, y = flow, color = "KES"), alpha = 0.8, linewidth = 1) +
  #geom_line(data = kwk, aes(x = date, y = flow, color = "KWK"), linetype = 'dashed', linewidth = 1) +
  scale_color_manual(name = "Site", values = c("KES" = "steelblue3", "KWK" = "grey")) +
  #scale_linetype_manual(name = "Site", values = c("KWK" = "solid", "KES" = "dashed")) +
  labs(x = 'Date', y = 'Flow (cfs)') +
  scale_y_continuous(limits = c(0, max(kes$flow)+1000)) +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.title.x = element_text(margin = margin(t = 8)),
    plot.margin = margin(.25, .25, .25, .25, "cm"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )
kes_plot

plot_flows <- list(kes_plot, kwk_plot)
for_map <- redds %>% group_by(Redd_ID, lat = Latitude, long = Longitude) %>%
  slice(n())
plots_ordered <- plots[as.character(for_map$Redd_ID)]

for_map_flows <- data.frame(gauge = c('KES Gage (Keswick Reservoir Outlfow)', 'KWK Gage(Sacramento River below Keswick)'),
                            Latitude = c(40.612104, 40.600983),
                            Longitude = c(-122.445699,-122.444458))


pal <- colorNumeric(
  palette = "viridis",
  domain = c(min_depth, max(for_map$measurement_depth, na.rm = TRUE)),
  reverse = TRUE
)
leaf_icons <- awesomeIcons(
  icon = 'tint',
  iconColor = 'white',
  library = 'fa',
  markerColor = 'blue'
)
map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB (grey canvas)") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = 'ESRI (satellite)') %>%
  setView(lng = -122.3836, lat = 40.5754, zoom = 11) %>%
  addCircleMarkers(data = for_map, 
                   lng = ~Longitude, 
                   lat = ~Latitude, 
                   label = ~paste("Latest Depth (in):", measurement_depth),
                   fillColor = ~pal(measurement_depth),
                   group = "CartoDB (grey canvas)",
                   color = 'black',
                   weight = 2,# Optional: border color
                   fillOpacity = 1,
                   radius = 5,
                   popup = popupGraph(plots_ordered, width = 625, height = 425),
                   popupOptions = popupOptions(
                     autoPan = TRUE
                   )) %>%
  addCircleMarkers(data = for_map, 
                   lng = ~Longitude, 
                   lat = ~Latitude, 
                   label = ~paste("Latest Depth (in):", measurement_depth),
                   fillColor = ~pal(measurement_depth),
                   group = "ESRI (satellite)",
                   color = 'white',
                   fillOpacity = 2,
                   weight = 3,# Optional: border color
                   radius = 6,
                   popup = popupGraph(plots_ordered, width = 625, height = 425),
                   popupOptions = popupOptions(
                     autoPan = TRUE
                   )) %>%
  addAwesomeMarkers(data = for_map_flows,
             icon = leaf_icons,
             lng = ~Longitude,
             lat = ~Latitude,
             label = ~gauge,
             popup = popupGraph(plot_flows, width = 450, height = 350)) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = c(min_depth, max(for_map$measurement_depth, na.rm = TRUE)),
    title = "Water Depth (in)",
    opacity = 1
  ) %>%
  addMiniMap(
    tiles = providers$CartoDB.Positron,
    toggleDisplay = TRUE,
    position = "bottomleft",
    width = 150,
    height = 150,
    zoomLevelOffset = -4
  ) %>%
  addLayersControl(
    baseGroups = c(
      "CartoDB (grey canvas)",
      "ESRI (satellite)"
    )) %>%
  addResetMapButton()
map

htmlwidgets::saveWidget(map, file = 'redd_depth_map.html')



