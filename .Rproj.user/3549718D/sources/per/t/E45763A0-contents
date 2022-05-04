library(tidytuesdayR)
library(tidyverse);library(janitor);library(skimr);library(lubridate)
library(hrbrthemes)

tuesdata <- tidytuesdayR::tt_load(2022, week = 18)
capacity <- tuesdata$capacity
wind = tuesdata$wind
solar = tuesdata$solar
avg_cost = tuesdata$average_cost

avg_cost = avg_cost %>% 
    filter(year < 2021) %>% 
    pivot_longer(!year,names_to = "type",
                 values_to = "avg_cost_per_mwh")
skim(avg_cost)

#Graph 1 - avg cost per MWh by type of power
a = ggplot(avg_cost) +
 aes(x = year, y = avg_cost_per_mwh, fill = type) +
 geom_area(size = 1.5) +
    scale_fill_manual(values = c("#005f6a","#FDB813","#D1F1F9"),
                      labels = c("Gas","Solar","Wind")) +
    labs(y="Average Cost per MWh (USD)",x="Year",
         title="Average Cost of Power per MegaWatt Hour (MWh) 
         in the United States",
         subtitle="The cost of solar power is getting cheaper to produce",
         caption="Source: Berkeley Lab’s 'Utility-Scale Solar, 2021 Edition' \n
         #TidyTuesday
         Visualization by Matthew Kusen - @mkusen ",
         fill="Type of Power")+
    theme_ipsum()+
    theme(legend.position = "top",
          plot.subtitle = element_text(size=10,
                                       face = "italic",
                                       color = "black",
                                       family = "",
                                       hjust=.83))
a
ggsave(a, filename = "3_may.jpg",height = 6,width = 6)
