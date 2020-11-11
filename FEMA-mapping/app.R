#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(conflicted)
filter <- dplyr::filter

# prepare data for plots

fema <- read.csv("hurricane.csv", header = TRUE)
options(scipen=999)
fema$state <- tolower(fema$state)
fema$county <- tolower(fema$county)
fema %<>% dplyr::select(disasterNumber, date, state, county, projectAmount, federalShareObligated)
names(fema) <- c("disaster", "date", "region", "subregion", "projectAmount", "federalShare")
# Separate county data, sum projectAmount, federalShareObligated by disaster and county
countyHurr <- fema %>% dplyr::filter(subregion!= "statewide")
countyFund <- countyHurr %>% dplyr::group_by(disaster, region, subregion, date) %>% dplyr::select(projectAmount, federalShare) %>% summarise_each(funs(sum)) 

# Separate statewide data, sum projectAmount, federalShareObligated by disaster and state
stateHurr <- fema %>% dplyr::filter(subregion=="statewide")
stateFund <- stateHurr %>% dplyr::group_by(disaster, region, date) %>% dplyr::select(projectAmount, federalShare) %>% summarise_each(funs(sum)) 


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
