library(tidyverse)
library(shiny)
library(shinyWidgets)
library(plotly)
library(factoextra)

data <- read.csv("data/fifa_players.csv", sep="|", encoding = 'UTF-8')

# Create global position variable (Defender, Midfielder or Forward)
data$global_position <- ifelse(data$player_position %in% c("RB","RWB","LB","LWB","CB") ,"Defender",
                               ifelse(data$player_position %in% c("CDM","CM","CAM","LM","RM"), "Midfielder",
                                      "Forward"))

# https://dplyr.tidyverse.org/reference/ (library for select, filter, rename...)
data <- data %>% select(id, short_name, club_name,
                        league_name, nationality_name, player_position, global_position,
                        age, value_eur, wage_eur, preferred_foot,
                        pace, shooting, passing, dribbling, defending, physic,
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

stats_names <- c('Pace','Shooting','Dribbling', 'Passing', 'Defending', 'Physic')

# Clustering
data_cluster <- data %>% filter(Global.Position == "Defender") 
row.names(data_cluster) <- data_cluster$id 
data_cluster_in <- data_cluster %>% select(stats_names) 
data_cluster_in <- data_cluster_in[1:3,]

set.seed(1)
k <- kmeans(data_cluster_in, center = 20, nstart = 25)   

p <- fviz_cluster(k, geom = "point", data = data_cluster_in, ggtheme = theme_minimal())+ggtitle("k = 3")
p

data_cluster$Cluster <- k$cluster


Mode <- function(x) {
  ux <- unique(x)
  if(!anyDuplicated(x)){
    NA_character_ } else { 
      tbl <-   tabulate(match(x, ux))
      toString(ux[tbl==max(tbl)])
    }
}
z <- data_cluster %>% group_by(Cluster) %>% summarise("Value (M)" = mean(Value),
                                                      Salary = mean(Salary),
                                                      Age = mean(Age),
                                                      Pace = mean(Pace),
                                                      Shooting = mean(Shooting),
                                                      Passing = mean(Passing),
                                                      Dribbling = mean(Dribbling),
                                                      Defending = mean(Defending),
                                                      Physic = mean(Physic),
                                                      Position = Mode(Position))
z <- round(z,2)
z$`Value (M)`

z <- round(z,0)
z <- as.data.frame(sapply(z,as.integer))
