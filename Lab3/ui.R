#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(prophet)



# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Stock Forecasting"),
    
    # Input: Select a company ----
    
    selectInput("Closing", "Select A Company for Closing Price Plot",
                choices = c('Select Company', 'Amgen','Apple', 'Comcast', 
                            'Gilead', 'Microsoft', 'Netflix'),
                selected = 'Select'),
    # Horizontal Line
    tags$hr(),
    
    # Input: Select a graph ----
    selectInput("Companies", "Select A Company for Forecast Plot:",
                choices = c('Select Company', 'Amgen','Apple', 'Comcast', 
                            'Gilead', 'Microsoft', 'Netflix'),
                selected = 'Select'),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("close_plot"),
            plotOutput("forecast_plot")
        )
    )
)
