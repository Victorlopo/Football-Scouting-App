library(plotly)
library(tidyverse)

data <- read.csv("data/fifa_players.csv", sep="|", encoding = 'UTF-8')

data <- data %>% select(short_name, club_name,
                        league_name, nationality_name, player_position,
                        age, value_eur, wage_eur, preferred_foot,
                        pace, shooting, passing, dribbling, defending, physic)

data <- data %>% rename(Name = short_name, Club = club_name ,League = league_name, 
                        Nation = nationality_name, Position = player_position,
                        Age = age, Value = value_eur, Salary = wage_eur, Foot = preferred_foot,
                        Pace = pace, Shooting = shooting, 
                        Passing = passing, Dribbling = dribbling, Defending = defending, 
                        Physic = physic)

stats_names = c('Pace','Shooting','Passing', 'Dribbling', 'Defending', 'Physic')
data_stats = data %>% select ("Name", stats_names)

fig <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
)  %>%
  add_trace(
    r = as.numeric(as.data.frame(data_stats %>% filter(Name == "L. Messi")
                                 %>% select(stats_names))[1,]),
    theta = stats_names,
    name = as.data.frame(data_stats %>% filter(Name == "L. Messi"))[1,"Name"]
  )  %>%
  add_trace(
    r = as.numeric(as.data.frame(data_stats %>% filter(Name == "R. Lewandowski")
                                 %>% select(stats_names))[1,]),
    theta = stats_names,
    name = as.data.frame(data_stats %>% filter(Name == "R. Lewandowski"))[1,"Name"]
  )  %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,100)
      )
    )
  )

fig
