# Load libraries
library(shiny)
library(tidyverse)

# Read in data
library(readr)
adult <- read_csv("/Users/aria/Downloads/adult.csv")
View(adult)

# Convert column names to lowercase for convenience 
names(adult) <- tolower(names(adult))

# Define server logic
shinyServer(function(input, output) {
  df_country <- reactive({
      adult %>% filter(native_country == input$country)
  })
  
  # TASK 5: Create logic to plot histogram or boxplot
  output$continuous_plot <- renderPlot({
    if (input$graph_type == "histogram") {
      # Histogram
      ggplot(df_country(), aes_string(x = input$continuous_variable)) +
        geom_histogram(bins = 30) +  # histogram geom
        labs(y = "Count", 
             title = paste("Distribution of", input$continuous_variable)) +  # labels
        facet_wrap(~prediction)    # facet by prediction
    }
    else {
      # Boxplot
      ggplot(df_country(), aes_string(y = input$continuous_variable)) +
        geom_boxplot() +  # boxplot geom
        coord_flip() +  # flip coordinates
        labs(x = "Count", 
             title = paste("Distribution of", input$continuous_variable)) + # labels
        facet_wrap(~prediction) # facet by prediction
    }
  })
  
  # TASK 6: Create logic to plot faceted bar chart or stacked bar chart
  output$categorical_plot <- renderPlot({
    
    # Bar chart
    p <- ggplot(df_country(), aes_string(x = input$categorical_variable)) +
      labs(y = "Count", 
           title = paste("Distribution of", input$categorical_variable)) +  # labels
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")    # modify theme to change text angle and legend position
    
    if (input$is_stacked) {
      p + geom_bar(aes(fill = prediction), position = "stack")  # add bar geom and use prediction as fill
    }
    else{
      p + 
        geom_bar(aes(fill = input$categorical_variable)) + # add bar geom and use input$categorical_variables as fill 
        facet_wrap(~prediction)   # facet by prediction
    }
  })
  
})


