library(tidyverse)
library(plotly)
library(factoextra)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(maps)

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
                        pace, shooting, passing, dribbling, defending, physic, overall,
                        player_face_url, search_name)
# search_name has to be the last one

# Rename columns (new name = older name)
data <- data %>% rename(Name = short_name, Club = club_name, League = league_name, 
                        Nation = nationality_name, Position = player_position, 
                        Global.Position = global_position,
                        Age = age, Value = value_eur, Salary = wage_eur, Foot = preferred_foot,
                        Pace = pace, Shooting = shooting, 
                        Passing = passing, Dribbling = dribbling, Defending = defending, 
                        Physic = physic, Rating = overall)
# These are the last names of the columns

# Create inputs for filters (database) ordering by name
nations <- as.list(unique(data["Nation"]))
nations <- lapply(nations,sort,decreasing=FALSE)
leagues <- as.list(unique(data["League"]))
leagues <- lapply(leagues,sort,decreasing=FALSE)

# Create radar chart data 
stats_names <- c('Pace','Shooting','Dribbling', 'Passing', 'Defending', 'Physic')
data_stats <- data %>% select ("Name", "Club", "player_face_url", "Age", "Nation",
                               "Value", "Position", stats_names)
data_stats$Name_Club <- paste(data_stats$Name, "-", data_stats$Club)
# Create inputs for filters (radar chart) ordering by name
players <- as.list(unique(data_stats["Name_Club"]))
players <- lapply(players,sort,decreasing=FALSE)

# Function for getting the mode
Mode <- function(x) {
  ux <- unique(x)
  if(!anyDuplicated(x)){
    NA_character_ } else { 
      tbl <-   tabulate(match(x, ux))
      toString(ux[tbl==max(tbl)])
    }
}

## UI ##########################################################################

ui <- navbarPage("Scouting App",
                 
                 ## TAB Compare players #######################################################################
                 tabPanel("Compare Players", fluid = TRUE,
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              h3('Select Players'),
                            
                              selectInput("player_1",
                                          label = "Player 1:", 
                                          choices = players,
                                          selected="L. Messi - Paris Saint-Germain"),
                              
                              selectInput("player_2",
                                          label = "Player 2:", 
                                          choices = players,
                                          selected = "N. Kanté - Chelsea"),
                              
                              helpText("After clicking on the selector you 
                                       should write the name of the player."),
                              
                              helpText("To visualize / hide the stats of a player
                                       click his name in the legend.")
                              
                            ), # Close sidebar
                            
                            mainPanel(
                              fluidRow(
                                
                                ## Player 1 target ##
                                column(6, box(title = textOutput("name_player_1"),
                                              width = 12,
                                              status = "primary", solidHeader = TRUE,
                                              collapsible = F,
                                              fluidRow(column(width = 2, uiOutput("img_player_1")),
                                                       column(width = 5, offset = 2, 
                                                              fluidRow(textOutput("age_player_1")), 
                                                              fluidRow(textOutput("value_player_1")), 
                                                              fluidRow(textOutput("position_player_1")),
                                                              fluidRow(textOutput("nation_player_1")))
                                              )
                                ),style = "background-color:#f0f0f0 ; padding-bottom: 1.3em; border-right: 2px solid grey;"),
                                
                                ## Player 2 target ##
                                column(6, box(title = textOutput("name_player_2"),
                                              status = "primary",
                                              solidHeader = T,
                                              collapsible = F,
                                              width = 12,
                                              fluidRow(column(width = 2,  uiOutput("img_player_2")),
                                                       column(width = 8, offset=2, 
                                                              fluidRow(textOutput("age_player_2")), 
                                                              fluidRow(textOutput("value_player_2")), 
                                                              fluidRow(textOutput("position_player_2")),
                                                              fluidRow(textOutput("nation_player_2")))
                                              )
                                )
                                ),style = "background-color:#f0f0f0; border: 2px solid grey; margin-right: 5px; border-radius: 7px; "),
                              
                              ## Display radar chart
                              fluidRow(
                                column(11, align="center",
                                       plotlyOutput("radar")
                                ))
                            ) # Close main panel
                            
                          ) # Close the sidebar layout
                          
                 ),# Close tab panel            
                 
                 
                 ## TAB Similar players #######################################################################
                 tabPanel("Similar Players", fluid = TRUE,
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              h3('Clustering'),
                              
                              selectInput("position_cluster",
                                          label = "Select Position:", 
                                          choices = c("Defender", "Midfielder", "Forward"),
                                          selected="Defender"),
                              
                              pickerInput("leagues_cluster",
                                          label = "Select Leagues:", 
                                          choices = leagues, 
                                          selected = unlist(leagues, use.names = FALSE),
                                          options = list(`actions-box` = TRUE),
                                          multiple = T),
                              
                              selectInput("n_clusters",
                                          label = "Number of Clusters:", 
                                          choices = c(2,3,4,5),
                                          selected = 3),
                              
                              helpText("The visualisation helps to choose the appropriate or desired number of clusters.")
                              
                            ), # Close sidebar
                            
                            mainPanel(
                              plotOutput('cluster_plot'),
                              tabsetPanel(type = "tabs",
                                          tabPanel("Summary", tableOutput('cluster_summary'),),
                                          tabPanel("Players", dataTableOutput('cluster_players'))
                              )
                              
                            ) # Close main panel
                            
                          ) # Close the sidebar layout
                          
                 ),# Close tab panel                     
                 
                 ## TAB Stats Correlation #######################################################################
                 tabPanel("Stats Correlation", fluid = TRUE,
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              h3('Select a League, Position and Main characteristics'),
                              
                              selectInput("corr_league",
                                          label = "League:", 
                                          choices = leagues,
                                          selected="English Premier League"),
                              
                              selectInput("corr_position",
                                          label = "Position:", 
                                          choices = list("RB" = "RB",
                                                         "RWB" = "RWB",
                                                         "LB" = "LB",
                                                         "LWB" = "LWB",
                                                         "CB" = "CB", 
                                                         "CDM" = "CDM",
                                                         "CM" = "CM",
                                                         "CAM" = "CAM",
                                                         "LM" = "LM",
                                                         "LW" = "LW",
                                                         "RM" = "RM",
                                                         "RW" = "RW",
                                                         "CF" = "CF",
                                                         "ST" = "ST"
                                          ),
                                          selected = "CAM"),
                              
                              selectInput("Characteristic_1",
                                          label = "Characteristic 1:", 
                                          choices = stats_names,
                                          selected = "Dribbling"),
                              
                              selectInput("Characteristic_2",
                                          label = "Characteristic 2:", 
                                          choices = stats_names,
                                          selected = "Shooting"),                                          
                              
                              helpText("You can see similar players based on their main attributes.")
                              
                              
                              
                            ), # Close sidebar
                            
                            mainPanel(plotlyOutput("correlation")
                            ) # Close main panel
                            
                          ) # Close the sidebar layout       
                 ),# Close tab panel     
                 
                 ## TAB Nations #######################################################################
                 tabPanel("Nations", fluid = TRUE,
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              h3('Map of the world'),
                              
                              selectInput("leagues_map",
                                          label = "Select League:", 
                                          choices = leagues, 
                                          selected = "English Premier League",
                                          multiple = F),
                              
                              checkboxGroupInput("positions_map", 
                                                 label = "Positions:", 
                                                 choices = c("Defender", "Midfielder", "Forward"),
                                                 inline = TRUE, c("Defender", "Midfielder", "Forward")),
                              
                              helpText("The visualisation helps to find the players' countries 
                                       of origin in each league and the distribution of the most relevant countries (top 8).")
                              
                            ), # Close sidebar
                            
                            mainPanel(plotOutput("map"),
                                      plotOutput("pie_map"),  
                            ) # Close main panel
                            
                          ) # Close the sidebar layout
                          
                 ),# Close tab panel
                 
                 ## TAB Offer Indicator #######################################################################
                 tabPanel("Offer Indicator", fluid = TRUE,
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              h3('Select Value or Salary, and Rating or Age, and and position you are looking for.'),
                              
                              selectInput("offer_position",
                                          label = "Position:", 
                                          choices = list("RB" = "RB",
                                                         "RWB" = "RWB",
                                                         "LB" = "LB",
                                                         "LWB" = "LWB",
                                                         "CB" = "CB", 
                                                         "CDM" = "CDM",
                                                         "CM" = "CM",
                                                         "CAM" = "CAM",
                                                         "LM" = "LM",
                                                         "LW" = "LW",
                                                         "RM" = "RM",
                                                         "RW" = "RW",
                                                         "CF" = "CF",
                                                         "ST" = "ST"
                                          ),
                                          selected = "CB"
                              ),
                              
                              selectInput("offer_value",
                                          label = "Value(€M)/Salary(€k):",
                                          choices = list("Salary" = "Salary",
                                                         "Value"  = "Value"
                                          ),
                                          selected = "Value"
                              ),
                              
                              selectInput("offer_player",
                                          label = "Age/Rating:",
                                          choices = list("Age" = "Age",
                                                         "Rating"  = "Rating"
                                          ),
                                          selected = "Age"
                              ),
                              
                              
                              
                            ), # Close sidebar
                            
                            mainPanel(plotlyOutput("offer")
                            ) # Close main panel
                            
                          ) # Close the sidebar layout
                          
                 ),# Close tab panel 
                 
                 ## TAB Database ####################################################################### 
                 
                 tabPanel("Database", fluid = TRUE,
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              h3('Apply Filters'),
                              
                              pickerInput("nations",
                                          label = "Nations:", 
                                          choices = nations, 
                                          selected = unlist(nations, use.names = FALSE),
                                          options = list(`actions-box` = TRUE),
                                          multiple = T),
                              
                              pickerInput("leagues",
                                          label = "Leagues:", 
                                          choices = leagues, 
                                          selected = unlist(leagues, use.names = FALSE),
                                          options = list(`actions-box` = TRUE),
                                          multiple = T),
                              
                              checkboxGroupInput("positions", 
                                                 label = "Positions:", 
                                                 choices = list("RB" = "RB",
                                                                "RWB" = "RWB",
                                                                "LB" = "LB",
                                                                "LWB" = "LWB",
                                                                "CB" = "CB", 
                                                                "CDM" = "CDM",
                                                                "CM" = "CM",
                                                                "CAM" = "CAM",
                                                                "LM" = "LM",
                                                                "LW" = "LW",
                                                                "RM" = "RM",
                                                                "RW" = "RW",
                                                                "CF" = "CF",
                                                                "ST" = "ST"
                                                 ), inline = TRUE, selected = "ST"),
                              
                              checkboxGroupInput("foot", 
                                                 label = "Foot:", 
                                                 choices = list("Left" = "Left",
                                                                "Right" = "Right"
                                                 ), inline = TRUE, selected = c("Left", "Right")),
                              
                              fluidRow(
                                
                                column(6, sliderInput("pace_range", 
                                                      label = "Pace:",
                                                      min = 1, max = 99, 
                                                      value = c(1, 99))),
                                column(6, sliderInput("shooting_range", 
                                                      label = "Shooting:",
                                                      min = 1, max = 99, 
                                                      value = c(1, 99)))
                                
                              ),
                              
                              fluidRow(
                                
                                column(6, sliderInput("passing_range", 
                                                      label = "Passing:",
                                                      min = 1, max = 99, 
                                                      value = c(1, 99))),
                                column(6, sliderInput("dribbling_range", 
                                                      label = "Dribbling:",
                                                      min = 1, max = 99, 
                                                      value = c(1, 99)))
                                
                              ),
                              
                              fluidRow(
                                
                                column(6, sliderInput("defending_range", 
                                                      label = "Defending:",
                                                      min = 1, max = 99, 
                                                      value = c(1, 99))),
                                column(6, sliderInput("physic_range", 
                                                      label = "Physic:",
                                                      min = 1, max = 99, 
                                                      value = c(1, 99)))
                                
                              ),
                              
                              helpText("You can use the filters and selectors to find the desired players in the dataset.")
                              
                            ), # Close sidebar
                            mainPanel(dataTableOutput('table')
                            ) # Close main panel
                            
                          ) # Close the sidebar layout
                          
                 ), # Close tab panel
                 
                 
) # Close navbar


## SERVER #######################################################################

server <- function(input, output) {
  
  ## Create Database Table #######################   
  output$table <- renderDataTable({
    data <- data %>% filter(Nation %in% input$nations &
                      League %in% input$leagues & 
                      Position %in% input$positions & 
                      Foot %in% input$foot & 
                      between(Pace, input$pace_range[1], input$pace_range[2]) &
                      between(Shooting, input$shooting_range[1], input$shooting_range[2]) &
                      between(Passing, input$passing_range[1], input$passing_range[2]) &
                      between(Dribbling, input$dribbling_range[1], input$dribbling_range[2]) &
                      between(Defending, input$defending_range[1], input$defending_range[2]) &
                      between(Physic, input$physic_range[1], input$physic_range[2])
    ) %>% select(-c(Global.Position, id, player_face_url))
    
    data$Value <- round(data$Value/1000000,2)
    data$Salary <- round(data$Salary/1000,2)
    data %>% rename("Value (€M)" = Value, "Salary(€k)" = Salary)
    },
    options = list(pageLength = 10, scrollX = T,
                   columnDefs = list(list(visible=FALSE, targets=c(-1))))
  )

  ## Create radar TAB info ####################### 
  
  # Create radar plot
  output$radar <- renderPlotly({
    plot_ly(
      type = 'scatterpolar',
      mode = "closest",
      fill = 'toself',
      width = 700, height=475
    )  %>%
      add_trace(
        r = as.numeric(as.data.frame(data_stats %>% filter(Name_Club == input$player_1)
                                     %>% select(stats_names))[1,]),
        theta = stats_names,
        name = as.data.frame(data_stats %>% filter(Name_Club == input$player_1))[1,"Name_Club"],
        mode = "markers"
      )  %>%
      add_trace(
        r = as.numeric(as.data.frame(data_stats %>% filter(Name_Club == input$player_2)
                                     %>% select(stats_names))[1,]),
        theta = stats_names,
        name = as.data.frame(data_stats %>% filter(Name_Club == input$player_2))[1,"Name_Club"],
        mode = "markers"
      )  %>%
      layout(
        legend = list(x = 0.3, y = -0.15),
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,100)
          )
        )
      )
  })
  
  ## Player 1 general info###
  output$img_player_1 <- renderUI({
    url <- as.data.frame(data_stats %>% filter(Name_Club == input$player_1))[1,"player_face_url"]
    tags$img(src = url, height = 100, width = 100)
  })
  
  output$name_player_1 <- renderText(
    name <- as.data.frame(data_stats %>% filter(Name_Club == input$player_1))[1,"Name"]
  )
  
  output$age_player_1 <- renderText({
    age <- as.data.frame(data_stats %>% filter(Name_Club == input$player_1))[1,"Age"]
    paste("AGE:", as.character(age))
  })
  
  output$value_player_1 <- renderText({
    value <- as.data.frame(data_stats %>% filter(Name_Club == input$player_1))[1,"Value"]
    value <- round(value/1000000,2)
    paste("VALUE:","€",as.character(value), "M")
  })
  
  output$position_player_1 <- renderText({
    position <- as.data.frame(data_stats %>% filter(Name_Club == input$player_1))[1,"Position"]
    paste("POSITION:",as.character(position))
  })
  
  output$nation_player_1 <- renderText({
    nation <- as.data.frame(data_stats %>% filter(Name_Club == input$player_1))[1,"Nation"]
    paste("NATION:",as.character(nation))
  })
  
  ## Player 2 general info ###
  output$img_player_2 <- renderUI({
    url <- as.data.frame(data_stats %>% filter(Name_Club == input$player_2))[1,"player_face_url"]
    tags$img(src = url, height = 100, width = 100)
  })
  
  output$name_player_2 <- renderText(
    name <- as.data.frame(data_stats %>% filter(Name_Club == input$player_2))[1,"Name"]
  )
  
  output$age_player_2 <- renderText({
    age <- as.data.frame(data_stats %>% filter(Name_Club == input$player_2))[1,"Age"]
    paste("AGE:", as.character(age))
  })
  
  output$value_player_2 <- renderText({
    value <- as.data.frame(data_stats %>% filter(Name_Club == input$player_2))[1,"Value"]
    value <- round(value/1000000,2)
    paste("VALUE:","€",as.character(value), "M")
  })
  
  output$position_player_2 <- renderText({
    position <- as.data.frame(data_stats %>% filter(Name_Club == input$player_2))[1,"Position"]
    paste("POSITION:",as.character(position))
  })
  
  output$nation_player_2 <- renderText({
    nation <- as.data.frame(data_stats %>% filter(Name_Club == input$player_2))[1,"Nation"]
    paste("NATION:",as.character(nation))
  })
  
  ## Clustering  #######################  
  # Filter the data by Global Position
  dataCluster <- reactive({
    validate(
      need(!is.null(input$leagues_cluster), "Please select at least one league")
    )
    data_cluster <- data %>% filter(Global.Position == input$position_cluster  & 
                                      League %in% input$leagues_cluster) 
    row.names(data_cluster) <- data_cluster$id
    data_cluster
  })
  
  # Create the clusters with Kmeans
  clusters <- reactive({
    data_cluster <- dataCluster() %>% select(stats_names)
    set.seed(1)
    kmeans(data_cluster, centers = as.numeric(input$n_clusters), nstart = 20)   
  })
  
  # Create the plot in 2D
  output$cluster_plot <- renderPlot({
    data_cluster <- dataCluster() %>% select(stats_names)
    k <- clusters()
    p <- fviz_cluster(k, geom = "point", data = data_cluster, 
                      ggtheme = theme_minimal())+ggtitle(paste0("Kmeans (",input$n_clusters, " clusters)"))
    options(repr.plot.width = 15, repr.plot.height = 8)
    p
  }, width = 600, height = 400)
  
  # Data table output with clusters summary
  output$cluster_summary <- renderTable({
    data <- dataCluster()
    k <- clusters()
    data$Cluster <- k$cluster
    summary <- data %>% group_by(Cluster) %>% summarise("Number of Players" = n(),
                                                        "Value (€M)" = round(mean(Value)/1000000,2),
                                                        "Salary (€k)" = round(mean(Salary)/1000,2),
                                                        Age = as.integer(mean(Age)),
                                                        Pace = as.integer(mean(Pace)),
                                                        Shooting = as.integer(mean(Shooting)),
                                                        Passing = as.integer(mean(Passing)),
                                                        Dribbling = as.integer(mean(Dribbling)),
                                                        Defending = as.integer(mean(Defending)),
                                                        Physic = as.integer(mean(Physic)),
                                                        "Main Position" = Mode(Position)
    )
    summary
    })
  
  # Data table output with the players in each cluster
  output$cluster_players <- renderDataTable({
    data <- dataCluster()
    k <- clusters()
    data$Cluster <- k$cluster
    data <- data %>% select(Cluster, Name, Club, League, Value, Position) %>% arrange(Cluster)
    data$Value <- round(data$Value/1000000,2)
    data %>% rename("Value (€M)" = Value)
  },
  options = list(pageLength = 5, scrollX = T, lengthMenu = c(5, 10, 20))
  )
  
  ## Create Map  #######################  
  output$map <- renderPlot({
    world_map <- map_data("world")
    world_map <- world_map %>% mutate(region = as.character(region))
    data <- data %>% mutate(Nation = if_else(Nation == "England", "UK", 
                                             if_else(Nation == "United States", "USA", Nation))) %>% 
      filter(League == input$leagues_map & Global.Position %in% input$positions_map) %>%
      count(Nation, name = "Number of Players") %>%
      rename(region = Nation) %>%
      mutate(region = as.character(region))
    
    numofplayers <- world_map %>% left_join(data, by = "region")
    
    ggplot(numofplayers, aes(long, lat, group = group))+
      geom_polygon(aes(fill = `Number of Players` ), color = "white", show.legend = TRUE)+
      scale_fill_viridis_c(option = "C")+
      theme_void()+
      labs(fill = "Number of Players",
           title = paste("Nations of", 
                         input$leagues_map, "(", 
                         paste(input$positions_map, collapse = ', '),")"))
  }, width = 850, height = 400)
  
  output$pie_map <- renderPlot({
    world_map <- map_data("world")
    world_map <- world_map %>% mutate(region = as.character(region))
    data <- data %>% mutate(Nation = if_else(Nation == "England", "UK", 
                                             if_else(Nation == "United States", "USA", Nation))) %>% 
      filter(League == input$leagues_map & Global.Position %in% input$positions_map) %>%
      count(Nation, name = "Number of Players") %>%
      rename(region = Nation) %>%
      mutate(region = as.character(region))
    
    numofplayers <- world_map %>% left_join(data, by = "region") %>% select(region, "Number of Players")
    numofplayers <- unique(numofplayers) %>% rename(Number.of.Players = "Number of Players")
    numofplayers <- na.omit(numofplayers)
    numofplayers <- numofplayers[order(-numofplayers$Number.of.Players),]
    numofplayers <- head(numofplayers,8)
    
    # Compute the prop
    numofplayers <- numofplayers %>% 
      arrange(desc(Number.of.Players)) %>%
      mutate(prop = Number.of.Players/sum(numofplayers$Number.of.Players) *100)
    
    # Basic piechart
    p<- ggplot(numofplayers, aes(x="", y=prop, fill=reorder(region,prop))) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y") +
      theme_void() + 
      geom_text(aes(label = paste0(round(prop), "%")), 
                position = position_stack(vjust = 0.5)) +
      scale_fill_brewer(palette="Set1")
    
    p + labs(fill = "Top 8 Nations") + ggtitle("Nationality Ratio")
  }, width = 350, height = 350)
  
  ## Create correlation plot #######################
  output$correlation <- renderPlotly({
    
    caracteristic_1 = input$Characteristic_1
    caracteristic_2 = input$Characteristic_2
    
    data <- (data %>% filter(League == input$corr_league, Position == input$corr_position) %>% 
               select(Name, Foot, input$Characteristic_1, input$Characteristic_2))
    
    hypo = cor.test(data[caracteristic_1][, 1], data[caracteristic_2][, 1], method = "spearman")
    
    ggplot(data, aes(x=input$Characteristic_1, y=input$Characteristic_2, label = Name, color = Foot)) +
      aes_string(x = input$Characteristic_1, y = input$Characteristic_2) +
      geom_point() +
      labs(title = paste("Spearman Correlation Coefficient:", round(hypo$estimate, digits = 2))) +
      geom_text(check_overlap = T, nudge_x = 0.25, nudge_y = 0.9) +
      theme(legend.position = "bottom")
    
  })
  
  ## Create BarChart Offer Indicator  ####################### 
  output$offer <- renderPlotly({
    
    input_value    = input$offer_value
    input_player   = input$offer_player
    
    data$Value <- round(data$Value/1000000,2)
    data$Salary <- round(data$Salary/1000,2)
    
    data_bar <- (data %>% filter(Position == input$offer_position) %>%
                   select(input$offer_value, input$offer_player))
    
    ggplot(data_bar, aes_string(x=input$offer_player,y=input$offer_value))+
      geom_bar(stat='summary', fill="skyblue", alpha=0.7, fun= 'mean')+
      xlab(input_player)+
      ylab(input_value)
  })
}

## APP ##########################################################################
shinyApp(ui = ui, server = server)