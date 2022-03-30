library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(skimr)
#library(ISOcodes)
library(MetBrewer)
library(ggthemes)
library(hrbrthemes)
#library(ggmap)
library(patchwork)
#library(gghighlight)
library(skim)
library(scales)


tuesdata <- tidytuesdayR::tt_load(2022, week = 12)
#lifetables <- tuesdata$lifetables %>% clean_names()
#births <- tuesdata$births %>% clean_names()
babynames <- tuesdata$babynames %>% clean_names()


#filter data
Matthew = babynames %>% 
    filter(name == "Matthew",year >= 1850) %>% 
    group_by(name,year) %>% 
    summarize(count = sum(n))
Mary = babynames %>% filter(name == "Mary",
                               year >= 1850) %>% 
    group_by(name,year) %>% 
    summarize(count = sum(n))
Adam = babynames %>% filter(name == "Adam",
                               year >= 1850)%>% 
    group_by(name,year) %>% 
    summarize(count = sum(n))
#visualize EDA
ggplot(Matthew,aes(x = year, y = count))+ geom_area()
ggplot(Adam,aes(x = year, y = count)) + geom_area()
ggplot(Mary,aes(x = year, y = count)) + geom_area()

#Filter data into one Data frame
Kusen <- babynames %>% 
    filter(name == "Matthew" | name == "Adam" | name == "Mary",
                                        year >= 1850) %>% 
    group_by(name,year) %>% 
    summarize(count = sum(n))


skim(Kusen)  
Kusen$name = as_factor(Kusen$name) %>% fct_relevel(c("Mary","Adam","Matthew"))
  
tt_graph <- Matthew %>% ggplot(aes(x = year,y=count,fill=name)) +
    geom_area(alpha = .7)+
    geom_area(alpha = .7,data=Mary)+
    geom_area(alpha = .7,data=Adam)+
    scale_fill_manual(values = c("#AED6F1","orchid3","#82E0AA")) +
    labs(x = "Year", y = "Count",
         fill="Name",
         title = "Baby Names in the U.S.",
         subtitle = "My siblings: The Kusens",
         caption = " \n #TidyTuesday - @mkusen \n  data: R {babynames}") +
    theme_classic()+
    theme(text = element_text(color = "white"),
          axis.text = element_text(color="white"),
          legend.background = element_rect(fill="black"),
          plot.title = element_text(hjust = 0.5, 
                                    vjust = 0.1,
                                    size = 16, 
                                    face = "bold"),
          plot.subtitle = element_text(hjust = 0.5,
                                       vjust = 0.1,
                                       size = 12),
          plot.caption = element_text(size = 7,
                                      hjust = 1),
          panel.background = element_rect(fill="black"),
          plot.background = element_rect(fill="black"))+
    scale_y_continuous(label=comma)+
    geom_text(label = "Mary \n born (1985)",aes(x=1958,
              y=20000),color="white",size=3.3)+
    geom_segment(x=1965,xend=1985,y=16000,yend=9311,size = .2, color="white")+
    geom_text(label = "Adam \n born (1988)",x=2010,
              y=40000,color="white", size=3.3)+
    geom_segment(x=2010,xend=1988,y=35000,yend=16542,size = .2, color="white")+
    geom_text(label = "Matthew \n born (1990)",
              x=2005,y=70000,color="white",size=3.3)+
    geom_segment(x=2005,xend=1990,y=65000,yend=44925,size = .2, color="white")
tt_graph

Kusen %>% filter(year == "1985" & name == "Mary"|
                 year == "1988" & name == "Adam"|
                 year == "1990" & name == "Matthew")

library(here)
ggsave(tt_graph,file = "baby_names_kusen_siblings.jpg",width = 7.5)
