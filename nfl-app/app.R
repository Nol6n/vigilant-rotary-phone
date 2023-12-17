
library(shiny)
library(tidyverse)

Whole_data <- load_pbp()


Whole_data <- Whole_data |>
  select(game_date, home_team,	away_team, defteam, yrdln, play_type, td_team,	td_player_name,
         receiver_player_name, rusher_player_name, fixed_drive_result)

write_csv(x = Whole_data, file = 'data/whole_data.csv')

Whole_data = read_csv(file = "data/whole_data.csv")


ui = navbarPage(
  title = "NFL",
  tabPanel(
    title = "Input/Visualization",
    titlePanel(title = "NFL Scoring Data for 2023 Season"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "home_team", 
                    label = "Home Team:", 
                    choices = sort(unique(Whole_data$home_team)),
                    selected = "CHI"),
        
        selectInput(inputId = "away_team", 
                    label = "Away Team:", 
                    choices = sort(unique(Whole_data$away_team)),
                    selected = "GB"),
        selectInput(inputId = "date", 
                    label = "Game Date:", 
                    choices = sort(unique(Whole_data$game_date))
                    ),
        selectInput(inputId = "td_team", 
                    label = "Touchdown  Distribution:", 
                    choices = sort(unique(c(Whole_data$home_team, Whole_data$away_team))),
                    selected = "CHI"),
        checkboxInput(inputId = "td_yes", label = "Filter Table for Touchdown Plays", value = FALSE)
      ), 
      mainPanel(plotOutput("distPlot"))
    )
    
    
    
  ),
  tabPanel(title = 'Table', dataTableOutput("table")),
  tabPanel(title = "About", includeMarkdown("about.Rmd"))
  
  
  )

server = function(input, output) {
  
  whole_data_home = reactive({
    Whole_data |>
      filter(home_team == input$home_team)
  })
  
  
  observeEvent(eventExpr = input$home_team,
               handlerExpr = {
                 updateSelectInput(inputId = "away_team", choices = unique(whole_data_home()$away_team))
               })
  

  whole_data_td = reactive({
    Whole_data |>
      filter(td_team == input$td_team)
  })
  
  observeEvent(eventExpr = {input$home_team 
    input$away_team},
               handlerExpr = {
                 td_team_choices = sort(unique(c(input$home_team, input$away_team)))
                 updateSelectInput(inputId = "td_team", choices = td_team_choices)
               }) 
  
  
  
  observeEvent(
    c(input$home_team, input$away_team),
    {
      filtered_dates = Whole_data |>
        filter(home_team == input$home_team, away_team == input$away_team) |>
        pull(game_date)
      
      updateSelectInput(inputId = "date", choices = unique(filtered_dates))
    }
  ) 
  
  
    output$distPlot = renderPlot({
      
      Whole_data |>
        filter((home_team == input$home_team & away_team == input$away_team) 
               & as.Date(game_date) == as.Date(input$date)
                 & td_team == input$td_team) |>
        group_by(player_name = coalesce(receiver_player_name, rusher_player_name)) |>
        summarize(total_touchdowns = sum(!is.na(td_player_name))) |>
        arrange(desc(total_touchdowns)) |>
        ggplot() +
        aes(x= reorder(player_name, -total_touchdowns), y = total_touchdowns, fill = player_name) |>
        geom_bar(stat = 'identity') +
        theme_bw() + 
        labs(title = "Touchdowns by Player",
             x = "Player Name",
             y = "Total Touchdowns",
             fill = 'Player Name')
      
      
    })
    
    output$table = renderDataTable({
     tab = whole_data_td() |>
        filter_and_count_touchdowns()
      
      if (input$td_yes) {
        tab = tab |>
          filter(touchdown_occurred == 'Yes') |>
        filter(home_team == input$home_team,
               away_team == input$away_team,
               td_team == input$td_team,
               as.Date(game_date) == as.Date(input$date))
      }
     
     tab
    })
    
    
}

shinyApp(ui = ui, server = server)
