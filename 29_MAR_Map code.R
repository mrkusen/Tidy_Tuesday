
library(tidyverse)
library(tidytuesdayR)
library(skimr)
library(leaflet)
library(tigris)

tuesdata <- tidytuesdayR::tt_load(2022, week = 13)
sports <- tuesdata$sports

skim(sports)



skim(sports$year)
by_state = sports %>% group_by(state_cd) %>% 
    summarize(total_profit_state = sum(total_profit)) %>% 
    arrange(desc(total_profit_state)) %>% 
    rename(STUSPS = "state_cd") %>% 
    drop_na()
by_state
names(by_state)

#add US map data from tigris
us <- states()

#check missing states / overseas territorites
STUSPS_os = setdiff(us$STUSPS,by_state$STUSPS)

#remove missing territories from the map & add data of interest
states =  us %>% filter(!STUSPS %in% STUSPS_os)
states = left_join(states,by_state,by="STUSPS")
names(states)

#leaflet
pal <- colorNumeric(
    palette = "YlGnBu",
    domain = states$total_profit_state
)

leaflet(states) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(fillColor = ~pal(total_profit_state),
                color = "black",
                weight = 0.5) %>%
    setView(-98.5795, 39.8282, zoom=3) %>% 
    addLegend(pal = pal, 
              values = states$total_profit_state, 
              position = "bottomright", 
              title = "Revenue for Collegiate Sports by State") 