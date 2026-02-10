#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Load libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyBS)  
library(shinycssloaders)  
library(lubridate)

# Load Data
data <- read.csv("dataset_garmin_activity.csv", stringsAsFactors = FALSE)

# Data Cleaning 
data$Distance <- as.numeric(data$Distance)
data$Calories <- as.numeric(data$Calories)
data$AvgHR <- as.numeric(data$Avg.HR)
data$MaxHR <- as.numeric(data$Max.HR)

data$Start_Formatted <- trimws(data$Start_Formatted)  # remove extra spaces
data$Start_Formatted <- as.POSIXct(data$Start_Formatted, format = "%d/%m/%Y %H:%M")
sum(is.na(data$Start_Formatted))



# Convert Time and Pace to consistent seconds format
convert_time <- function(x) {
  if (grepl(":", x)) {
    parts <- unlist(strsplit(x, ":"))
    if (length(parts) == 2) {
      as.numeric(parts[1]) * 60 + as.numeric(parts[2])
    } else {
      as.numeric(parts[1]) * 3600 + as.numeric(parts[2]) * 60 + as.numeric(parts[3])
    }
  } else {
    NA
  }
}
data$Time_seconds <- sapply(data$Time, convert_time)

# Define UI
ui <- fluidPage(
  titlePanel("FitnessPal - Fitness Tracker"),
  
  withSpinner(
    sidebarLayout(
      sidebarPanel(
        selectInput("user", "Select User:", choices = unique(data$User), multiple = TRUE),
        bsTooltip("user", "Select one or more users to filter the data", placement = "right"),
        
        selectInput("activity", "Select Activity:", choices = unique(data$Activity.Type), multiple = TRUE),
        bsTooltip("activity", "Choose one or multiple activity types (e.g., Running, Cycling)", placement = "right"),
        
        dateRangeInput("date_range", 
                       "Select Date Range:",
                       start = min(data$Start_Formatted, na.rm = TRUE),
                       end = as.Date("2016-04-30")),
        bsTooltip("date_range", "Select a range of dates to filter activities", placement = "right"),
        
        checkboxInput("missing", "Include Rows with Any Missing Data", TRUE),
        bsTooltip("missing", "Check to include records that have missing values in Distance, Calories, or Heart Rate", placement = "right"),
        
        downloadButton("download_data", "Download Filtered Data as CSV")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Overview", 
                   fluidRow(
                     column(4, h4("Total Distance"), textOutput("total_dist")),
                     column(4, h4("Total Calories"), textOutput("total_cal")),
                     column(4, h4("Average HR"), textOutput("avg_hr"))
                   ),
                   hr(),
                   h5("Filtered Rows:"),
                   textOutput("row_count"),
                   plotOutput("summary_plot")
          ),
          
          tabPanel("Charts",
                   plotOutput("time_series_plot"),
                   plotOutput("scatter_plot"),
                   plotOutput("bar_plot")
          ),
          
          tabPanel("Data Table",
                   DTOutput("data_table")
          ),
          
          tabPanel("Help", 
                   h3("How to Use the App"),
                   p("Use the filters in the sidebar to explore the fitness tracking data:"),
                   tags$ul(
                     tags$li("Select one or more users to display their activities."),
                     tags$li("Filter by activity type (e.g., Running, Cycling)."),
                     tags$li("Adjust the date range picker to explore activities within a specific time period."),
                     tags$li("Include Rows with Any Missing Data – Check this box to include activities that are missing one or more values like Distance, Calories, or Heart Rate. Unchecking it ensures only complete entries are used in summaries and plots.")
                   ),
                   h4("Combining Filters"),
                   p("You can combine multiple filters for in-depth exploration. For example:"),
                   tags$ul(
                     tags$li("Select 'User A', 'Running', and set the date range to April to view only their running activities in that month."),
                     tags$li("Leave all filters unselected and date range unchanged to display all available data.")
                   )
          )
        )
      )
    )
  )
)

#Server
server <- function(input, output) {
  
  # Reactive: Filtered data based on inputs
  filtered_data <- reactive({
    df <- data
    if (!input$missing) {
      df <- df %>% filter(!is.na(Distance) & !is.na(Calories))
    }
    if (length(input$user) > 0) {
      df <- df %>% filter(User %in% input$user)
    }
    if (length(input$activity) > 0) {
      df <- df %>% filter(Activity.Type %in% input$activity)
    }
    if (!is.null(input$date_range)) {
      df <- df %>% filter(Start_Formatted >= input$date_range[1] & Start_Formatted <= input$date_range[2])
    }
    df
  })
  
  output$row_count <- renderText({
    paste(nrow(filtered_data()), "rows")
  })
  
  # Download filtered data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_fitness_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Overview Stats
  output$total_dist <- renderText({
    sum(filtered_data()$Distance, na.rm = TRUE)
  })
  
  output$total_cal <- renderText({
    sum(filtered_data()$Calories, na.rm = TRUE)
  })
  
  output$avg_hr <- renderText({
    hr <- mean(filtered_data()$AvgHR, na.rm = TRUE)
    if (is.nan(hr)) {
      "Not Available"
    } else {
      round(hr, 1)
    }
  })
  
  # Summary Bar Plot (Average Calories by Activity)
  output$summary_plot <- renderPlot({
    df <- filtered_data() %>%
      filter(!is.na(Calories))  # only filter for this variable
    
    ggplot(df, aes(x = reorder(Activity.Type, Calories, FUN = mean), y = Calories, fill = Activity.Type)) +
      geom_bar(stat = "summary", fun = "mean") +
      geom_text(stat = "summary", fun = mean,
                aes(label = round(after_stat(y), 1)),
                vjust = -0.5) +
      theme_minimal() +
      labs(title = "Average Calories Burned by Activity",
           x = "Activity Type", y = "Average Calories") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  # Time Series Plot
  output$time_series_plot <- renderPlot({
    df <- filtered_data() %>%
      filter(!is.na(Distance), !is.na(Start_Formatted))  # clean inputs
    
    ggplot(df, aes(x = Start_Formatted, y = Distance, color = Activity.Type)) +
      geom_point() +
      theme_minimal() +
      labs(title = "Distance Over Time", x = "Date", y = "Distance (km)") +
      scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  # Scatter Plot (Distance vs Calories)
  output$scatter_plot <- renderPlot({
    df <- filtered_data() %>%
      filter(!is.na(Distance), !is.na(Calories))  # clean input data
    
    ggplot(df, aes(x = Distance, y = Calories, color = Activity.Type)) +
      geom_point() +
      theme_minimal() +
      labs(title = "Distance vs. Calories", x = "Distance (km)", y = "Calories")
  })
  
  
  # Bar Plot (Average Distance by Activity)
  output$bar_plot <- renderPlot({
    df <- filtered_data() %>%
      filter(!is.na(Distance))  # remove only Distance NAs for this chart
    
    ggplot(df, aes(x = reorder(Activity.Type, Distance, FUN = mean), y = Distance, fill = Activity.Type)) +
      geom_bar(stat = "summary", fun = "mean") +
      geom_text(stat = "summary", fun = mean,
                aes(label = round(after_stat(y), 1)),
                vjust = -0.5) +
      theme_minimal() +
      labs(title = "Average Distance by Activity",
           x = "Activity Type", y = "Average Distance (km)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Data Table
  output$data_table <- renderDT({
    datatable(filtered_data(), options = list(
      pageLength = 10,
      lengthMenu = c(5, 10, 20),
      searching = TRUE,
      ordering = TRUE,
      scrollX = TRUE
    ))
  })
}

# Run the app
shinyApp(ui = ui, server = server)