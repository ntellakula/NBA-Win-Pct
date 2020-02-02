############################# Necessary Libraries ##############################
library(DT)
library(zoo)
library(rvest)
library(RCurl)
library(ggrepel)
library(ggimage)
library(tidyverse)
library(lubridate)
library(teamcolors)
library(shiny)
library(shinydashboard)




############################### Data Wrangling ################################
#source site on bb reference
url_source <- "https://www.basketball-reference.com/teams/"
webpage <- read_html(url_source)

#list of the divisions
divisions <- webpage %>% 
  html_nodes(".division") %>% 
  html_nodes("strong") %>% 
  html_text()

#list of city names
team_cities <- webpage %>% 
  html_nodes(".division") %>% 
  html_nodes("a") %>% 
  html_text()

#3 letter team ids
team_ids <- webpage %>% 
  html_nodes(".division") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  str_sub(8, 10)

#initialize an empty list into which we will store all teams and their win pcts
empty_list <- vector(mode = "list", length = 30)

#loop and store all the game info
for (i in 1:30) {
  #game by game info for a team
  year <- "2020"
  team <- team_ids[i]
  url <- paste0("https://www.basketball-reference.com/teams/",
                team, "/", year, "_games.html")
  webpage <- read_html(url)
  
  #the list of columns in the data table on the site
  col_names <- webpage %>% 
    html_nodes("table#games > thead > tr > th") %>% 
    html_attr("data-stat")
  
  #raw data set, then converting it to tibble
  data <- webpage %>% 
    html_nodes("table#games > tbody > tr > td") %>% 
    html_text() %>% 
    matrix(ncol = length(col_names) - 1, byrow = TRUE)
  source <- as_tibble(data)
  colnames(source) <- col_names[2:length(col_names)]
  
  #adding win percentage, taking just the win percentage and the team name
  source_pct <- source %>%
    filter(game_result != "") %>% 
    mutate(game = as.numeric(wins) + as.numeric(losses),
           pct = as.numeric(wins) / (as.numeric(wins) + as.numeric(losses)),
           percent = format(round(pct, 3), nsmall = 3),
           team = team,
           game_date = as.Date(str_sub(date_game, 6),
                               format = "%b %d, %Y")) %>% 
    select(team, percent, wins, losses, game, game_date)
  
  #every team starts at 0 winning percentage, so this row will be added up top
  team_final <- add_row(source_pct, team = team,
                        percent = "0.000", wins = 0, .before = 1) %>% 
    mutate(pct = as.numeric(percent))
  
  #storing the information into the list
  empty_list[[i]] <- team_final
}

#consolidating the list
all_teams <- bind_rows(empty_list)

#what is the earliest date?
min_date <- all_teams %>% 
  filter(!is.na(game_date)) %>% 
  pull(game_date) %>% 
  min()

#what is the latest date?
max_date <- all_teams %>% 
  filter(!is.na(game_date)) %>% 
  pull(game_date) %>% 
  max()

all_teams <- all_teams %>% 
  mutate(thee_date = as.Date.numeric(ifelse(is.na(game_date),
                                            min_date - 1,
                                            game_date)))

#combining division, team, and ID
desc_team <- as_tibble(cbind(rep(divisions, each = 5),
                             team_cities, team_ids)) %>% 
  rename("division" = V1)

#add desc_team to all_teams
full_team <- all_teams %>% 
  left_join(desc_team, by = c("team" = "team_ids"))

#colors
data(teamcolors)
nba_colors <- teamcolors %>% 
  filter(league == "nba") %>% 
  #concatenating Los Angeles Lakers/Clippers for easy merge
  mutate(the_city = ifelse(location == "Los Angeles",
                           paste(location, mascot),
                           location))

#logos: from GitHub
url_logo <- getURL("https://raw.githubusercontent.com/ntellakula/NBA-Win-Pct/master/nba_team_logos.csv")
logos <- read.csv(text = url_logo)

#add a label to the end of each group: will show up at right edge of graphic
#final analytical data set to be used in the visualizations
analysis <- full_team %>% 
  #joining in the team colors
  left_join(nba_colors %>% 
              select(-c(division, league, sportslogos_name,
                        location, logo)),
            by = c("team_cities" = "the_city")) %>% 
  #join the logos
  left_join(logos %>% 
              mutate(svg_png = as.character(url1.svg.png)) %>% 
              select(-c(team, url2.svg, url1.svg.png)),
            by = c("team" = "team_code")) %>% 
  group_by(team) %>% 
  #add logo and label variable so they only show at end of graphic
  mutate(wins = as.numeric(wins),
         losses = as.numeric(losses),
         conference = ifelse(division %in% divisions[1:3],
                             "Eastern",
                             "Western")) %>% 
  #arrange by team and game
  arrange(team_cities, game) %>% 
  ungroup() %>% 
  select(team, team_cities, mascot, name, conference, division, primary,
         svg_png, thee_date, game, losses, wins, pct)




###################################### UI ######################################
ui <- dashboardPage(
  dashboardHeader(title = "NBA Wins and Win Percentage",
                  titleWidth = 450), #end dashboard header
  dashboardSidebar(
    
    #Division and Conference Selection
    selectInput(inputId = "div",
                label = strong("Division"),
                choices = c("All", divisions,
                            "Western Conference", "Eastern Conference"),
                selected = "All"),
    
    #Wins or Win Percentage?
    selectInput(inputId = "type",
                label = strong("Type of Graphic"),
                choices = c("Wins", "Win Percentage"),
                selected = "Wins"),
    
    #Label or Logo
    selectInput(inputId = "display",
                label = strong("Type of Visual"),
                choices = c("Label", "Logo"),
                selected = "Label"),
    h6("3-letter Team Label or Team Logo",
       align = "center"),
    
    #Size of the logo
    sliderInput(inputId = "logo_size",
                "Size of Logo/Label",
                1, 10, 5),
    
    #Date range
    dateRangeInput("date", strong("Date Range"),
                   start = min(analysis$thee_date), end = max_date,
                   min = min(analysis$thee_date), max = max_date),
    h6("Default dates are start of season and current day.",
       align = "center")
    
  ), #end dashboard Sidebar
  
  dashboardBody(
    
    #main graphic/plot
    box(plotOutput("plot", height = 800),
        width = 8,
        title = "Line Graph", status = "primary",
        solidHeader = T),
    
    box(DTOutput("view"),
        width = 4,
        status = "info",
        solidHeader = T,
        title = "Win/% Table")
    
  ) #end dashboard Body
)





#################################### Server ####################################
server <- function(input, output) {
  
  #Subset date by time
  time_select <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]),
                  "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2],
                  "Error: Start date should be earlier than end date."))
    analysis %>% 
      filter(thee_date > input$date[1] & thee_date < input$date[2]) %>% 
      group_by(team) %>% 
      mutate(game_session = game - min(game),
             wins_session = wins - min(wins),
             loss_session = losses - min(losses),
             pct2 = wins_session / (wins_session + loss_session),
             percent_sesh = format(round(pct2, 3), nsmall = 3),
             logo_max = ifelse(row_number() == n(),
                               svg_png,
                               NA_character_),
             label_max = ifelse(row_number() == n(),
                                team,
                                NA_character_))
  })
  
  #Subset data
  selected_data <- reactive({
    #conditional on if everything is selected
    if (input$div == "All") {
      time_select()
    } else if (input$div == "Western Conference") {
      #conditional to split on conference
      time_select() %>% 
        filter(conference == "Western")
    } else if (input$div == "Eastern Conference") {
      time_select() %>% 
        filter(conference == "Eastern")
    } else {
      #just the divisions
      time_select() %>% 
        filter(division == input$div)
    }
  })
  
  #Wins or Win Percentage? Subset accordingly
  type_of_graph <- reactive({
    switch(input$type,
           "Wins" = selected_data()$wins_session,
           "Win Percentage" = selected_data()$pct2)
  })
  
  #Win/Win Percentage for all the grey bars as background
  tog <- reactive({
    switch(input$type,
           "Wins" = time_select()$wins_session,
           "Win Percentage" = time_select()$pct2)
  })
  
  #To show either the label of the team or the logo
  disp <- reactive({
    switch(input$display,
           "Label" = geom_label_repel(data = selected_data(),
                                      aes(label = label_max,
                                          x = game_session,
                                          y = type_of_graph()),
                                      na.rm = T,
                                      size = input$logo_size),
           "Logo" = geom_image(data = selected_data(),
                               aes(image = logo_max,
                                   x = game_session,
                                   y = type_of_graph()),
                               size = input$logo_size / 100,
                               na.rm = T))
  })
  
  #main graphic/plot
  output$plot <- renderPlot({
    ggplot() +
      geom_line(aes(x = game_session,
                    y = tog(),
                    group = team),
                data = time_select(),
                colour = alpha("grey", 0.6)) +
      geom_line(aes(x = game_session,
                    y = type_of_graph(),
                    group = team),
                data = selected_data(),
                colour = selected_data() %>% 
                  pull(primary),
                size = 1.1) +
      labs(title = paste(as.character(input$type), "by Team"),
           x = "Games",
           y = as.character(input$type)) +
      scale_x_continuous(expand = c(0, 0),
                         limits = c(0, time_select()$game_session %>% max() * 1.02)) +
      scale_y_continuous(expand = c(0, 0),
                         limits = c(0, tog() %>% max() * 1.02)) +
      theme(panel.border = element_rect(colour = "black", fill = NA),
            plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white")) +
      disp()
  })
  
  #Subset the data for table output for the teams
  record <- reactive({
    selected_data() %>% 
      group_by(team) %>% 
      filter(row_number() == n()) %>% 
      mutate(record = paste(wins_session, loss_session, sep = "-")) %>% 
      arrange(desc(wins_session)) %>% 
      select(team, record, percent_sesh) %>% 
      rename("Team" = team,
             "Record" = record,
             "Win %" = percent_sesh)
  })
  
  #Data Table
  output$view <- renderDT(
    record(),
    options = list(pageLength = 15,
                   lengthMenu = list(c(5, 10, 15),
                                     c("5", "10", "15")))
  )
  
}

shinyApp(ui, server)