library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboard)

# Load data
data <- read.csv("Results_21Mar2022.csv")
data <- data %>% mutate(across(c(diet_group, age_group, my_age_group, grouping, mc_run_id, sex), as.factor))

# ui
ui <- dashboardPage(
  dashboardHeader(title = "COMP4037 CW2"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$div(class = "well well-sm",
             tags$h4("This visualisation dashboard is based on the Research Paper:"),
             tags$a(href = "https://www.nature.com/articles/s43016-023-00795-w", 
                    target = "_blank", 
                    "Vegans, Vegetarians, Fish-eaters & Meat-eaters in the UK show discrepant Environmental Impacts")
    ),
    fluidRow(column(width = 2),
             box(solidHeader = T, title = "Dietary Impact", 
                 plotlyOutput("diet.plot"),
                 h4('* The radar plot shows the adverse impact of different diet groups across various environmental factors.'),
                 h4('* The larger the polygon, the greater is the impact on the environment.'), 
                 status="primary",width=8),
             column(width = 2)),
    fluidRow(column(width = 2),
             box(solidHeader = T, title = "Gender Impact", 
                 plotlyOutput("gender.plot"),
                 h4('* The radar plot shows the adverse impact of different diet groups & how it varies with gender.'),
                 status="primary",width=8),
             column(width = 2)),
    fluidRow(column(width = 2),
             box(solidHeader = T, title = "Age Group Impact", 
                 plotlyOutput("age.plot"),
                 h4('* The radar plot shows the adverse impact of different diet groups & how it varies with age.'),
                 h4('* 20 to 29 = Young Aged, 30 to 59 years = Middle Aged, 60 to 79 = Old Aged'),
                 status="primary",width=8),
             column(width = 2))
  )
)

# server
server <- function(input, output) {
  
  output$diet.plot <- renderPlotly({
    # original units
    data_summary.og <- data %>% group_by(diet_group) %>%
      summarise(across(c(mean_ghgs, mean_land, mean_watscar, mean_watuse, mean_bio, mean_eut, mean_acid), mean, .names = "mean_{.col}"), Total_Participants = sum(n_participants, na.rm = TRUE), .groups = 'drop')
    new_labels <- c("GHG Emissions", "Land Use", "Water Scarcity", "Water Usage", "Biodiversity", "Eutrophication", "Acidification")
    names(data_summary.og)[2:8] <- new_labels
    data_long.og <- pivot_longer(data_summary.og, -c(diet_group, Total_Participants), names_to = "Variables", values_to = "Values")
    
    # data Scaling
    scaled_columns <- c('mean_ghgs', 'mean_land', 'mean_watscar', 'mean_eut', 'mean_ghgs_ch4', 'mean_ghgs_n2o', 'mean_bio', 'mean_watuse', 'mean_acid')
    data[scaled_columns] <- scale(data[scaled_columns])
    data_summary <- data %>% group_by(diet_group) %>%
      summarise(across(c(mean_ghgs, mean_land, mean_watscar, mean_watuse, mean_bio, mean_eut, mean_acid), mean, .names = "mean_{.col}"), Total_Participants = sum(n_participants, na.rm = TRUE), .groups = 'drop')
    names(data_summary)[2:8] <- new_labels
    data_long <- pivot_longer(data_summary, -c(diet_group, Total_Participants), names_to = "Variables", values_to = "Values")
    
    # final
    data_long <- cbind.data.frame(data_long, og = data_long.og$Values)
    
    # Hover text debugging: Make sure to format it correctly - to be fixed 
    text = ~paste("Diet Group: ",diet_group,"<br>Avg.",Variables, ": ", round(og, 2), "<br>Participants: ", Total_Participants)
    
    # Create a Radar Chart for aggregated data
    plot_ly(data_long, type = 'scatterpolar', r = ~Values, theta = ~Variables,  color = ~diet_group,  text = text, hoverinfo = 'text', fill = 'toself', mode = 'lines+markers') %>%
      layout(polar = list(radialaxis = list(visible = TRUE, range = range(data_long$Values)) ), title = "Environmental Impact by Diet Group", legend = list(title = list(text = 'Diet Group')), showlegend = TRUE)
  })
  
  output$gender.plot = renderPlotly({
    # original units
    data_summary.og <- data %>% group_by(diet_group, sex) %>% 
      summarise(across(c(mean_ghgs, mean_land, mean_watscar, mean_watuse, mean_bio, mean_eut, mean_acid), mean, .names = "mean_{.col}"), Total_Participants = sum(n_participants, na.rm = TRUE), .groups = 'drop')
    new_labels <- c("GHG Emissions", "Land Use", "Water Scarcity", "Water Usage", "Biodiversity", "Eutrophication", "Acidification")
    names(data_summary.og)[3:9] <- new_labels
    data_long.og <- pivot_longer(data_summary.og, -c(diet_group, Total_Participants, sex), names_to = "Variables", values_to = "Values")
    
    # Data Scaling
    scaled_columns <- c('mean_ghgs', 'mean_land', 'mean_watscar', 'mean_eut', 'mean_ghgs_ch4', 'mean_ghgs_n2o', 'mean_bio', 'mean_watuse', 'mean_acid')
    data[scaled_columns] <- scale(data[scaled_columns])
    data_summary <- data %>% group_by(diet_group, sex) %>%
      summarise(across(c(mean_ghgs, mean_land, mean_watscar, mean_watuse, mean_bio, mean_eut, mean_acid), mean, .names = "mean_{.col}"), Total_Participants = sum(n_participants, na.rm = TRUE), .groups = 'drop')
    names(data_summary)[3:9] <- new_labels
    data_long <- pivot_longer(data_summary, -c(diet_group, Total_Participants, sex), names_to = "Variables", values_to = "Values")
    
    data_long <- cbind.data.frame(data_long, og = data_long.og$Values)
    
    # Hover text debugging: Make sure to format it correctly - to be fixed 
    text = ~paste("Diet Group: ",diet_group,"<br>Gender:",sex,"<br>Avg.",Variables, ": ", round(og, 2), "<br>Participants: ", Total_Participants)
    
    # Ensure the Radar Chart includes correct hover information - to be fixed
    plot_ly(data_long, type = 'scatterpolar', r = ~Values, theta = ~Variables, color = ~diet_group, split = ~sex, text = text, hoverinfo = 'text', fill = 'toself', mode = 'lines+markers') %>%
      layout(polar = list(radialaxis = list(visible = TRUE, range = range(data_long$Values))), title = "Environmental Impact by Diet Group and Sex", legend = list(title = list(text = 'Diet Group')), showlegend = TRUE)
    
  })

  output$age.plot = renderPlotly({
    # original units
    data_summary.og <- data %>% group_by(diet_group, my_age_group) %>% 
      summarise(across(c(mean_ghgs, mean_land, mean_watscar, mean_watuse, mean_bio, mean_eut, mean_acid), mean, .names = "mean_{.col}"), Total_Participants = sum(n_participants, na.rm = TRUE), .groups = 'drop')
    new_labels <- c("GHG Emissions", "Land Use", "Water Scarcity", "Water Usage", "Biodiversity", "Eutrophication", "Acidification")
    names(data_summary.og)[3:9] <- new_labels
    data_long.og <- pivot_longer(data_summary.og, -c(diet_group, Total_Participants, my_age_group), names_to = "Variables", values_to = "Values")
    
    # Data Scaling
    scaled_columns <- c('mean_ghgs', 'mean_land', 'mean_watscar', 'mean_eut', 'mean_ghgs_ch4', 'mean_ghgs_n2o', 'mean_bio', 'mean_watuse', 'mean_acid')
    data[scaled_columns] <- scale(data[scaled_columns])
    data_summary <- data %>% group_by(diet_group, my_age_group) %>%
      summarise(across(c(mean_ghgs, mean_land, mean_watscar, mean_watuse, mean_bio, mean_eut, mean_acid), mean, .names = "mean_{.col}"), Total_Participants = sum(n_participants, na.rm = TRUE), .groups = 'drop')
    names(data_summary)[3:9] <- new_labels
    data_long <- pivot_longer(data_summary, -c(diet_group, Total_Participants, my_age_group), names_to = "Variables", values_to = "Values")
    
    data_long <- cbind.data.frame(data_long, og = data_long.og$Values)
    
    # Hover text debugging: Make sure to format it correctly - to be fixed 
    text = ~paste("Diet Group: ",diet_group,"<br>Age group:",my_age_group,"<br>Avg.",Variables, ": ", round(og, 2), "<br>Participants: ", Total_Participants)
    
    # Ensure the Radar Chart includes correct hover information - to be fixed
    plot_ly(data_long, type = 'scatterpolar', r = ~Values, theta = ~Variables, color = ~diet_group, split = ~my_age_group, text = text, hoverinfo = 'text', fill = 'toself', mode = 'lines+markers') %>%
      layout(polar = list(radialaxis = list(visible = TRUE, range = range(data_long$Values))), title = "Environmental Impact by Diet Group and Age Group", legend = list(title = list(text = 'Diet Group')), showlegend = TRUE)
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
