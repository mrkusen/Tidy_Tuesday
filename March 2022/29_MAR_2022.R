library(tidyverse)
library(tidytuesdayR)
library(skimr)
library(here)
library(janitor)
library(scales)
library(patchwork)
library(hrbrthemes)

tuesdata <- tidytuesdayR::tt_load(2022, week = 13)
sports <- tuesdata$sports
skim(sports)

#Add additional data on salaries
salaries = read_csv("March 2022/data_NCAA/head_coach_salary_2019.csv") %>% clean_names()
salary_subset = salaries %>% select(institution_name,state_cd,
                                    mens_team_average_annual_institutional_salary_per_head_coach) %>% 
    rename(avg_salary_men_headcoach = "mens_team_average_annual_institutional_salary_per_head_coach")
skim(salary_subset)

#add profit variable to tt
sports = sports %>% mutate(total_profit = total_rev_menwomen - total_exp_menwomen) %>% drop_na(total_profit)

#Salary of top 20 revenue
top_20 = sports %>% filter(year == 2019) %>% slice_max(total_profit,n=20) %>% select(institution_name,total_profit)
top_20 
top_20_salary = salary_subset %>% filter(institution_name %in% top_20$institution_name) %>% arrange(desc(avg_salary_men_headcoach))

#join the two data sets
top_20_salary = left_join(top_20_salary,top_20,by="institution_name")
top_20_salary
#edit shorten U of Lousiana
top_20_salary$institution_name = recode(top_20_salary$institution_name,"Louisiana State University and Agricultural & Mechanical College" = "Lousiana State University")


a = top_20_salary %>% 
    arrange(total_profit) %>% 
    mutate(institution_name = factor(institution_name, levels=institution_name)) %>% 
    ggplot()+aes(x = institution_name, weight = avg_salary_men_headcoach) +
 geom_bar(fill = "darkolivegreen4")+
    scale_y_continuous(label=comma)+
    coord_flip()+
    labs(x="Name of University",
         y="Average Salary of Male's Team Head Coach",
         title = "Average Annual Salary for Head Coach in 2019",
         subtitle = "Top 20 Universities by Highest Annual College Football Revenue",
         caption= "#TidyTuesday -  By Matthew Kusen - @mkusen \n
         data source: Equity In Athletic Data Analysis - US DOE")+
    theme(plot.title = element_text(hjust = 0.5, 
                                    vjust = 0.1,
                                    size = 14, 
                                    face = "bold"))+
    theme_ipsum()+
    geom_text(label = "Highest Revenue ($104,923,029)",
              y = 1300000,x="The University of Texas at Austin",
              color="black",size=3)+
    geom_text(label = "2nd Lowest Revenue ($37,426,290)",
              y=1700000,x="Texas A & M University-College Station",
              color="black", size=3)

#add lowest and highest salary 
min(top_20$total_profit)
max(top_20$total_profit)
    
a

ggsave(a,filename="March 2022/college_football_salaries.jpg",width=9,height=6)
g

# -----


#Some random EDA
revenue_by_sport = sports %>% 
    group_by(sports,year) %>% 
    summarize(total_revenue = sum(total_profit)) %>% 
    arrange(desc(total_revenue)) %>% 
    filter(total_revenue > 0,
           sports != "Football") %>% 
    filter(sports != "Basketball")
revenue_by_sport$year = as_factor(revenue_by_sport$year)
revenue_by_sport
