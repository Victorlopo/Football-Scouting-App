library(tidyverse)
library(plotly)
library(factoextra)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

## DATA #######################################################################
# GK missing
data <- read.csv("data/fifa_players.csv", sep="|", encoding = 'UTF-8')

# Create global position variable (Defender, Midfielder or Forward)
data$global_position <- ifelse(data$player_position %in% c("RB","RWB","LB","LWB","CB") ,"Defender",
                               ifelse(data$player_position %in% c("CDM","CM","CAM","LM","RM"), "Midfielder",
                                      "Forward"))

# https://dplyr.tidyverse.org/reference/ (library for select, filter, rename...)
data <- data %>% select(id, short_name, club_name,
                        league_name, nationality_name, player_position, global_position,
                        age, value_eur, wage_eur, preferred_foot,
                        pace, shooting, passing, dribbling, defending, physic, player_face_url,
                        search_name)
# search_name has to be the last one

# Rename columns (new name = older name)
data <- data %>% rename(Name = short_name, Club = club_name, League = league_name, 
                        Nation = nationality_name, Position = player_position, 
                        Global.Position = global_position,
                        Age = age, Value = value_eur, Salary = wage_eur, Foot = preferred_foot,
                        Pace = pace, Shooting = shooting, 
                        Passing = passing, Dribbling = dribbling, Defending = defending, 
                        Physic = physic)
# These are the last names of the columns


options(repr.plot.width = 12, repr.plot.height = 8)

world_map <- map_data("world")
world_map <- world_map %>% mutate(region = as.character(region))
data <- data %>% mutate(Nation = if_else(Nation == "England", "UK", 
                                         if_else(Nation == "United States", "USA", Nation))) %>% 
           filter(League == "Spain Primera Division") %>%
           count(Nation, name = "Number of Player") %>%
           rename(region = Nation) %>%
           mutate(region = as.character(region))

numofplayers <- world_map %>% left_join(data, by = "region")

ggplot(numofplayers, aes(long, lat, group = group))+
  geom_polygon(aes(fill = `Number of Player` ), color = "white", show.legend = FALSE)+
  scale_fill_viridis_c(option = "C")+
  theme_void()+
  labs(fill = "Number of Player",
       title = "Number of Players")

nations <- as.data.frame(unique(world_map["region"]))
nations <- lapply(nations,sort,decreasing=FALSE)

