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
library(lubridate)
library(hurricaneexposuredata)
library(drat)
library(conflicted)

filter <- dplyr::filter

# prepare data for plots

fema <- read.csv("hurricane.csv")
options(scipen=999)
fema$state <- tolower(fema$state)
fema$county <- tolower(fema$county)
fema %<>% dplyr::select(disasterNumber, date, state, county, projectAmount, federalShareObligated)
names(fema) <- c("disaster", "date", "region", "subregion", "projectAmount", "federalShare")
fema$year <- year(fema$date)
# Separate county data, sum projectAmount, federalShareObligated by disaster and county
countyHurr <- fema %>% dplyr::filter(subregion!= "statewide")
countyFund <- countyHurr %>% dplyr::group_by(disaster, region, subregion, date) %>% dplyr::select(projectAmount, federalShare) %>% summarise_each(funs(sum)) 

# Separate statewide data, sum projectAmount, federalShareObligated by disaster and state
stateHurr <- fema %>% dplyr::filter(subregion=="statewide")
stateFund <- stateHurr %>% dplyr::group_by(disaster, region, date) %>% dplyr::select(projectAmount, federalShare) %>% summarise_each(funs(sum)) 

# Prepare map data
StatesInt <- c("texas","oklahoma","kansas","louisiana","arkansas","missouri","iowa","wisconsin","michigan","illinois","indiana","ohio","kentucky","tennessee","alabama","mississippi","florida","georgia","south carolina","north carolina","virginia","west virginia","maryland","delaware","pennsylvania","new jersey","new york","connecticut","rhode island","massachusetts","vermont","new hampshire","maine")

MainStates <- map_data("county", StatesInt)
# Add longitudes and latitudes to county and state data
merge(MainStates,countyFund,by=c("region","subregion"))


# Define UI for application shows maps and plots
ui <- fluidPage(
    title = "FEMA",
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Year:",
                        min = 2009,
                        max = 2020,
                        value = 1)
        ),
        tabsetPanel(
            conditionalPanel(
                'input.dataset === "fema"'),
            
            conditionalPanel(
                'input.dataset === "fema"',
            )
        ),
        mainPanel(
            
            tabsetPanel(
                id = 'dataset',
                tabPanel("County Federal FEMA Funds",
                         # Create a new Row in the UI for selectInputs for mapping
                         fluidRow(
                             
                             column(4,
                                    selectInput("year",
                                                "Year:",
                                                c("All",
                                                  unique(fema$year)))
                             )
                             ,
                             column(4,
                                    selectInput("state",
                                                "State:",
                                                c("All",
                                                  unique(fema$state)))
                             ),
                         ),
                         # Create plots for fed spending prop. per year
                         plotOutput("countyMap"),
                         plotOutput("fedMap"),
                         tableOutput("fedTable"),
                         br(),
                         tableOutput("stateTable"),
                         br(),br()
                )
                ,
                tabPanel("State FEMA Funds Total",
                        # Create a new Row in the UI for selectInputs
                         fluidRow(
                             column(4,
                                    selectInput("yearFema",
                                                "Year:",
                                                c("All",
                                                  unique(fema$year))
                                    ),
                             ),
                             column(4,
                                    selectInput("stateFema",
                                                "State:",
                                                c("All", unique(fema$state))
                                                
                                    )
                             ),
                             # Plot rain of the year
                             plotOutput("statewidePlot"),
                             br(), br(),
                             br(),
                             # Create summary table for rain of the year
                             tableOutput("statewideTable"),
                             br(),
                             br()
                             
                         )
                )
                ,
                
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$countyTable <- renderTable({
        if(input$state =="All" & input$year == "All"){
            filtered <- stateFund
        } else if (input$year == "All"){
            filtered <- countyFund %>% filter(region == input$state)
        } else if (input$state == "All"){
            filtered <- countyFund %>% filter(year == input$year)
        }
        else {
            filtered <- countyFund %>% filter(region == input$state & Year ==input$year )
        }
        names(filtered) = c("Disaster Number", "State", "Disaster Declaration Date", "Project Amount Total", "Federal Obligated Fund")
         kable(filtered) %>% kableExtra::kable_styling(bootstrap_options = "material_dark")
    })
    
    output$countyMap <- renderPlot({
        filtered <- countyFund %>% filter(year == input$year)
        p <- ggplot()+
            geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),colour="black",fill="white")+
            geom_polygon(data=filtered, aes(x = long, y = lat, group = group, fill = projectAmount/federalShare))+
            labs(fill="Project Total Amount(USD)") + 
            ggtitle(paste("FEMA Funds for Year", input$year, "by County")) +
            theme(plot.title = element_text(hjust = 0.5))
        p
    })
    
    output$fedMap <- renderPlot({
        # if(input$stateY =="All" & input$yearY == "All"){
        #     filtered <- yield
        #     p <- ggplot(data = filtered)+
        #         geom_point(aes(x=Year, y= Value, color = factor(State)), alpha=0.6, size = 5)+
        #         ylim(c(0, 800))+
        #         xlim(c(2015, 2019))
        # } else if (input$yearY == "All"){
        #     filtered <- yield %>% filter(State == input$stateY)
        #     p <- ggplot(data = filtered)+
        #         geom_col(aes(x=Year, y= Value, fill = State),  width = 0.4)+
        #         ylim(c(0, 800))+
        #         xlim(c(2014, 2020))
        # } else if (input$stateY == "All"){
        #     filtered <- yield %>% filter(Year == input$yearY)
        #     p <- ggplot(data = filtered)+
        #         geom_point(aes(x=Year, y= Value, color = factor(State)), alpha=0.6, size = 5)+
        #         ylim(c(0, 800))+
        #         xlim(c(2014, 2020))
        # }
        # else {
        #     filtered <- yield %>% filter(State == input$stateY)
        #     p <- ggplot(data = filtered)+
        #         geom_col(aes(x=Year, y= Value, fill = State),  width = 0.4)+
        #         ylim(c(0, 800))+
        #         xlim(c(2014, 2020))
        # }
        # p
    })
    
    output$yieldSum <- renderTable({
        
        # if(input$stateY =="All" & input$yearY == "All"){
        #     filtered <- yield
        # } else if (input$yearY == "All"){
        #     filtered <- yield %>% filter(State == input$stateY)
        # } else if (input$stateY == "All"){
        #     filtered <- yield %>% filter(Year == input$yearY)
        # }
        # else {
        #     filtered <- yield %>% filter(State == input$stateY & Year ==input$yearY )
        # }
        # 
        # filtered <- filtered %>% select (Year, State, Value, unit)
        # names(filtered) = c("Year", "State", "Yield", "Unit")
        # filtered
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
