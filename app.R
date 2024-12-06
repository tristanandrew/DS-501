library(shiny)
library(plotly)
library(shinythemes)
library(DT)
library(fontawesome)
library(shinyjs)

ui <-fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"), #Applies a modern looking theme
  tags$style(HTML("h4 { color: #2c3e50; font-weight: bold; } .tab-content { background-color: f9f9f9; padding: 20px; border-radius: 5px; }")
  ),
  
  #Welcome screen
  actionButton("welcomeModal", "Show Welcome", style = "display: none;"),
  
  titlePanel(
    div(icon("futbol"), "Soccer Players with 50+ International Goals", style = "font-weight: bold; font-size: 24px; color: #0073C2;")
  ),
  sidebarLayout(
    sidebarPanel(
      h4("Filters", style = "color: #0073C2; "),
      textInput("filterInput", "Filter by Player Name", ""),
      selectInput("highScorer", "Filter High Scorers", choices = c("All", "Yes", "No"), selected = "All"),
      uiOutput("nationFilterUI"),
      uiOutput("confederdationFilterUI"),
      sliderInput("goalRange", "Goals Range",
                  min = 0, max = 200, value = c(0, 200), width = "100%"),
      sliderInput("goalsPerMatchRange", "Goals per Match Range",
                  min = 0, max = 2, value = c(0, 2), width = "100%"),
      tags$h5("Career Years"),
      sliderInput("yearRange", "Career Year Range",
                  min = 1900, max = as.numeric(format(Sys.Date(), "%Y")), value = c(1900, as.numeric(format(Sys.Date(), "%Y"))), width = "100%"),
      actionButton("resetFilters", "Reset All Filters", icon = icon("undo"), class = "btn-primary")
    ),
    mainPanel(
      textOutput("filterFeedback"),
      tabsetPanel(
        type = "tabs",
        tabPanel(div(icon("table"), "Player Table"),
                 DTOutput("playerStats")),
        tabPanel(div(icon("chart-line"), " Player Stats Plot"),
                 plotlyOutput("scatterPlot")),
        tabPanel(div(icon("chart-bar"), " Regression Analysis"), 
                 plotOutput("regressionPlot"),
                 tableOutput("regressionTable")),
        tabPanel(div(icon("globe"), " Nation and Confederation Analysis"),
                 plotOutput("nationBarChart"),
                 plotOutput("confederationPieChart")),
        tabPanel(div(icon("clock"), " Career Timeline"), 
                 plotOutput("careerTimeline", height = "850px", width = "100%")),
        tabPanel(div(icon("lightbulb"), " Fun Facts"), 
                 uiOutput("funFacts"))
      )
    )
  ),
  tags$footer("2024 Soccer Stats App")
)


#Library declarations
library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)
library(caret)
library(cluster)
library(dplyr)
library(plotly)

#Function to download dataset
download_kaggle_data <- function() {
  kaggle_credentials <- fromJSON("C:\Users\tandrew\OneDrive - Rapid Micro Biosystems\Documents\tjandrew_HW6\kaggle.json")
  kaggle_username <- kaggle_credentials$username
  kaggle_key <- kaggle_credentials$Key
  
  #Define the dataset and path
  dataset <- "whisperingkahuna/footballers-with-50-international-goals-men"
  download_path <- "data/"
  dataset_zip <- paste0(download_path, "dataset.zip")
  
  #Ensure the folder exists
  if (!dir.exists(download_path)) {
    dir.create(download_path)
  }
  
  #Kaggle API request URL
  url <- paste0("https://www.kaggle.com/api/vi/datasets/download/", dataset, ".zip")
  
  #Request to download
  response <- GET(url,
                  authenticate(kaggle_username, kaggle_key),
                  write_disk(dataset_zip, overwrite = TRUE))
  
  #Check and unzip if successful
  if (status_code(response) == 200) {
    print("Dataset downloaded successfully.")
    unzip(dataset_zip, exdir = download_path)
    return(TRUE)
  } else {
    print("Failed to download dataset.")
    print(content(response, as = "text"))
    return(FALSE)
  }
}


#Server function
server <- function(input, output, session) {
  
  observe({
    runjs("$('#welcomeModal').click();")
  })
  
  observeEvent(input$welcomeModal, {
    showModal(modalDialog(
      title = "Welcome to the Soccer Dashboard",
      "This app allows you to explore data on the players with 50+ international goals.
      Use the filters to narrow down your search, view statistics, and analyze trends.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  load_data <- function() {
    dataset_path <- "data/tjandrew_HW6_cleaned.csv"
    
    if (!file.exists(dataset_path)) {
      success <- download_kaggle_data()
      if(!success) {
        stop("Failed to download the Kaggle dataset.")
      }
    }
    
    my_data <- read.csv(dataset_path, stringsAsFactors = FALSE)
    
    #Adding derived columns
    my_data$HighScorer <- ifelse(my_data$Goals >= 100, "Yes", "No") #Classification label
    my_data$StartYear <- as.numeric(substr(my_data$Career.span, 1, 4))
    my_data$EndYear <- ifelse(grepl("-", my_data$Career.span),
                              as.numeric(substr(my_data$Career.span, 6, 9)),
                              2024) #Assumes players that do not have an end year are still active
    my_data$Goals.per.match <- my_data$Goals / my_data$Caps
    
    return(my_data)
  }
  
  #Load dataset
  my_data <- load_data()
  
  #Dynamic UI for Nation and Confederation
  output$nationFilterUI <- renderUI({
    selectInput("nationFilter", "Filter by Nation",
                choices = c("", unique(my_data$Nation)),
                selected = "")
  })
  
  output$confederdationFilterUI <- renderUI({
    selectInput("confederationFilter", "Filter by Confederation",
                choices = c("", unique(my_data$Confederation)),
                selected = "")
  })
  
  #Reactive to filter data based on user inputs
  filteredData <- reactive({
    df <- my_data
    
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    df$EndYear[is.na(df$EndYear)] <- current_year
    
    #Filter data based on input values
    if (!is.null(input$filterInput) && input$filterInput != "") {
      df <- df[grepl(input$filterInput, df$Player, ignore.case = TRUE), ]
    } 
    if (!is.null(input$highScorer) && input$highScorer != "All") {
      df <- df[df$HighScorer == input$highScorer, ]
    }
    if (!is.null(input$nationFilter) && input$nationFilter != ""){
      df <- df[df$Nation == input$nationFilter, ]
    }
    if (!is.null(input$confederationFilter) && input$confederationFilter != "") {
      df <- df[df$Confederation == input$confederationFilter, ]
    }
    if (!is.null(input$goalRange)) {
      df <- df[df$Goals >= input$goalRange[1] & df$Goals <= input$goalRange[2], ]
    }
    if (!is.null(input$goalsPerMatchRange)) {
      df <- df[df$Goals.per.match >= input$goalsPerMatchRange[1] & df$Goals.per.match <= input$goalsPerMatchRange[2], ]
    }
    if (!is.null(input$yearRange)) {
      df <- df[(df$StartYear >= input$yearRange[1] & df$EndYear <= input$yearRange[2]) |
                 (df$EndYear == current_year & current_year <= input$yearRange[2]), ]
    }
    return(df)
  })
  
  #Player statistics
  output$playerStats <- DT::renderDataTable({
    req(filteredData())
    datatable(filteredData()[, c("Rank", "Player", "Nation", "Goals", "Caps", "Goals.per.match")],
              options = list(pageLength = 10, scrollx = TRUE, autoWidth = TRUE),
              rownames = FALSE) %>%
      formatStyle(
        "Goals", backgroundColor = styleInterval(c(50, 100), c("white", "lightyellow", "lightgreen"))
      )
  })
  
  output$filterFeedback <- renderText ({
    paste("Showing", nrow(filteredData()), "players matching the selected filters.")
  })
  
  observeEvent(input$resetFilters, {
    updateTextInput(session, "filterInput", value = "")
    updateSelectInput(session, "highScorer", selected = "All")
    updateSliderInput(session, "goalRange", value = c(0, 200))
    updateSliderInput(session, "goalsPerMatchRange", value = c(0, 2))
    updateSliderInput(session, "yearRange", value = c(1900, as.numeric(format(Sys.Date(), "%Y"))))
  })
  
  output$scatterPlot <- renderPlotly({
    req(filteredData())
    plot <- ggplot(filteredData(), aes(x = Goals, y = Caps, 
                                       text = paste("Player: ", Player, "<br>Goals: ", Goals, "<br>Caps: ", Caps))) +
      geom_point(color = "#0073C2", size = 3) +
      labs(title = "Goals vs. Caps", x = "Goals", y = "Caps") +
      theme_minimal()
    
    ggplotly(plot, tooltip = "text")
  })
  
  #Regression analysis
  output$regressionPlot <- renderPlot({
    req(filteredData())
    
    #Preparing the data
    plot_data <- filteredData() %>%
      select(Goals, Caps, Goals.per.match) %>%
      na.omit()
    
    #Fit the regression model
    lm_model <- lm(Goals ~ Caps + Goals.per.match, data = plot_data)
    
    #Plot the scatter with the regression line
    ggplot(plot_data, aes(x = Caps, y = Goals)) +
      geom_point(color = "lightblue", size = 3) +
      geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +
      labs(title = "Multiple Regression: Goals based on Caps and Goals per Match", 
           x = "Caps", y = "Goals") +
      theme_minimal()
  })
  
  #Adding a table to visually demonstrate regression
  output$regressionTable <- renderTable({
    req(filteredData())
    
    #Preparing data
    plot_data <- filteredData() %>%
      select(Player, Goals, Caps, Goals.per.match) %>%
      na.omit()
    
    #Fit the regression model
    lm_model <- lm(Goals ~ Caps + Goals.per.match, data = plot_data)
    
    #Predict goals based on the regression model
    plot_data$PredictedGoals <- predict(lm_model, newdata = plot_data)
    
    #Sort by predicted goals in descending order and get the top 5 players
    top_players <- plot_data %>%
      arrange(desc(PredictedGoals)) %>%
      head(5)
    
    #Return the table
    top_players[, c("Player", "PredictedGoals", "Goals", "Caps", "Goals.per.match")]
  })
  
  # Nation & Confederation Analysis
  output$nationBarChart <- renderPlot({
    req(filteredData)
    nation_summary <- filteredData() %>%
      group_by(Nation) %>%
      summarize(TotalGoals = sum(Goals, na.rm = TRUE), .groups = "drop")
    
    ggplot(nation_summary, aes(x = reorder(Nation, -TotalGoals), y = TotalGoals)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      labs(title = "Total Goals by Nation", x = "Nation", y = "Total Goals") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$confederationPieChart <- renderPlot({
    req(filteredData())
    conf_summary <- filteredData() %>%
      group_by(Confederation) %>%
      summarize(TotalGoals = sum(Goals, na.rm = TRUE))
    
    ggplot(conf_summary, aes(x = "", y = TotalGoals, fill = Confederation)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Goals by Confederation") +
      theme_void() +
      theme(legend.position = "right")
  })
  
  # Historical Trends
  output$careerTimeline <- renderPlot({
    req(filteredData())
    ggplot(filteredData(), aes(x = StartYear, xend = EndYear, y = Player, yend = Player)) +
      geom_segment(size = 2, color = "lightblue") +
      labs(title = "Career Spans of Players", x = "Year", y = "Player") +
      theme_minimal() +
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 16, face = "bold"),
            axis.title.y = element_text(size = 16, face = "bold"),
            panel.grid.major.x = element_line(size = 0.5, color = "gray80"),
            panel.grid.major.y = element_blank())
  })
  
  # Fun Facts
  output$funFacts <- renderUI({
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    active_players <- my_data[is.na(my_data$EndYear) | my_data$EndYear >= current_year, "Player"]
    career_end_year <- as.numeric(sub(".*-", "", my_data$Career.span))
    retired_players <- my_data[!is.na(career_end_year) & career_end_year < current_year, "Player"]
    fastest_50 <- my_data[which.min(my_data$Caps[my_data$Goals >= 50]), "Player"]
    first_100 <- my_data[which.min(my_data$Caps[my_data$Goals >= 100]), "Player"]
    active_list <- head(active_players, 10)
    retired_list <- head(retired_players, 10)
    tagList(
      tags$style(HTML("
                      h4 { color: #2c3e50; font-weight: bold; margin-bottom: 10px; }
                      ul { list-style-type: disc; padding-left: 20px; }
                      li { margin-bottom: 5px }
                      p { font-size: 16px; line-height: 1.5; margin-top: 10px; }")),
      
      div(class = "fun-facts-section",
          tags$h4("Active Players"),
          if(length(active_players) > 0) {
            tags$ul(lapply(active_players, tags$li))
          } else {
            tags$p("No active players available.")
          }),
      
      div(class = "fun-facts-section",
          tags$h4("Retired Players"),
          if(length(retired_players) > 0) {
            tags$ul(lapply(retired_players, tags$li))
          } else {
            tags$p("No retired players available.")
          }),
      
      div(class = "fun-facts-section",
          tags$h4("Fastest to 50 Goals"),
          tags$p(
            tags$b(fastest_50),
            " achieved this incredible feat faster than any other player.")
      ),
      
      div(class = "fun-facts-section",
          tags$h4("First to 100 Goals"),
          tags$p(
            tags$b(first_100),
            " was the first player to reach 100 goals in internationl football, and one of three to ever do it.")
      )
    )
  })
}

shinyApp(ui = ui, server = server)
