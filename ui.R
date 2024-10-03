# Load libraries
library(shiny)
library(tidyverse)

# Application Layout
shinyUI(
  fluidPage(
    br(),
    # TASK 1: Application title
    titlePanel("Income and Demographics Explorer"),
    p("Explore the difference between people who earn less than 50K and more than 50K. You can filter the data by country, then explore various demogrphic information."),
    
    # TASK 2: Add first fluidRow to select input for country
    fluidRow(
      column(12, 
             wellPanel(selectInput("country", "Select a country:", 
                                   choices = c("United-States", "Canada", "Mexico", "Germany", "Philippines"),
                                   selected = "United-States"))  # add select input 
             )
    ),
    
    # TASK 3: Add second fluidRow to control how to plot the continuous variables
    fluidRow(
      column(3, 
             wellPanel(
               p("Select a continuous variable and graph type (histogram or boxplot) to view on the right."),
               radioButtons("continuous_variable", "Continuous variable:",
                            choices = c("age", "hours_per_week"),
                            selected = "age"), # add radio buttons for continuous variables
               radioButtons("graph_type", "Select Graph Type:",
                            choices = c("histogram", "boxplot"),
                            selected = "histogram")    # add radio buttons for chart type
               )
             ),
      column(9, plotOutput("continuous_plot"))  # add plot output
    ),
    
    # TASK 4: Add third fluidRow to control how to plot the categorical variables
    fluidRow(
      column(3, 
             wellPanel(
               p("Select a categorical variable to view bar chart on the right. Use the check box to view a stacked bar chart to combine the income levels into one graph. "),
               radioButtons("categorical_variable", "Select Categorical Variable:",
                            choices = c("education", "workclass", "sex"),
                            selected = "education"),    # add radio buttons for categorical variables
               checkboxInput("is_stacked", "Stack bars", value = FALSE)     # add check box input for stacked bar chart option
               )
             ),
      column(9, plotOutput("categorical_plot"))  # add plot output
    )
  )
)
