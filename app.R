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
library(maps)

# Prepare map data
StatesInt <- c("texas","oklahoma","kansas","louisiana","arkansas","missouri","iowa","wisconsin","michigan","illinois","indiana","ohio","kentucky","tennessee","alabama","mississippi","florida","georgia","south carolina","north carolina","virginia","west virginia","maryland","delaware","pennsylvania","new jersey","new york","connecticut","rhode island","massachusetts","vermont","new hampshire","maine")

MainStates <- map_data("county", StatesInt)
states <- map_data("state", StatesInt)
# prepare data for plots

fema <- read.csv("hurricane.csv")

options(scipen=999)
fema$state <- tolower(fema$state)
fema$county <- tolower(fema$county)
fema %<>% dplyr::select(disasterNumber, date, state, county, projectAmount, federalShareObligated)
names(fema) <- c("disaster", "date", "region", "subregion", "projectAmount", "federalShare")
fema$year <-as.numeric( year(fema$date))
# Separate county data, sum projectAmount, federalShareObligated by disaster and county
countyHurr <- fema %>% dplyr::filter(subregion!= "statewide")
countyFund <- countyHurr %>% dplyr::group_by(disaster, region, subregion, date) %>% dplyr::select(projectAmount, federalShare) %>% summarise_each(funs(sum)) 

# Separate statewide data, sum projectAmount, federalShareObligated by disaster and state
stateHurr <- fema %>% dplyr::filter(subregion=="statewide")
stateFund <- stateHurr %>% dplyr::group_by(disaster, region, date) %>% dplyr::select(projectAmount, federalShare) %>% summarise_each(funs(sum)) 

# Add longitudes and latitudes to county and state data
countyFund<- merge(MainStates,countyFund,by=c("region","subregion"))
countyFund$year <- as.numeric(year(countyFund$date))
stateFund$year <- as.numeric(year(stateFund$date))


# prepare state total plot data
stateTotal <- stateFund %>% dplyr::group_by(region) %>% dplyr::select(projectAmount, federalShare) %>% summarise_each(funs(sum))
stateTotal <- merge(stateTotal, MainStates, by = c("region"))


stateYear <- stateFund %>% dplyr::group_by(region, year) %>% dplyr::select(projectAmount, federalShare) %>% summarise_each(funs(sum))



# Define UI for application shows maps and plots
ui <- fluidPage(
    title = "FEMA",
    sidebarLayout(
        # sidebarPanel(
        #     sliderInput("year", label = h3("Hurricane Year Range"),
        #                 min = 2009,
        #                 max = 2020,
        #                 value = 2012)
        # ),
        tabsetPanel(
            conditionalPanel(
                'input.dataset === "fema"'),

            conditionalPanel(
                'input.dataset === "fema"',
            )
        ),
        mainPanel(
            
                
                "County Federal FEMA Funds",
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
                                                  unique(countyFund$region)))
                             )
                         ),
                         # Create plots for fed spending prop. per year
                         plotOutput("stateMap"),
                         plotOutput("countyMap"),
                         tableOutput("countyTable"),
                         tableOutput("stateTable"),
                         br(),br()
                )
                
            
        
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$countyTable <- renderTable({
        
        if(input$state =="All" & input$year == "All"){
            filtered <- stateFund
        } else if (input$year == "All"){
            filtered <- stateFund %>% dplyr::filter(region == input$state)
        } else if (input$state == "All"){
            filtered <- stateFund %>% dplyr::filter(year == input$year)
        }
        else {
            filtered <- stateFund %>% dplyr::filter(region == input$state & year ==input$year )
        }
        filtered %<>% select(disaster, region, date, projectAmount, federalShare)
        names(filtered) = c("Disaster Number", "State", "Disaster Declaration Date", "Project Amount Total", "Federal Obligated Fund")
        filtered
    })
    
    output$countyMap <- renderPlot({
        if(input$state =="All" & input$year == "All"){
            filtered <- countyFund
        } else if (input$year == "All"){
            filtered <- countyFund %>% dplyr::filter(region == input$state)
        } else if (input$state == "All"){
            filtered <- countyFund %>% dplyr::filter(year == input$year)
        }
        else {
            filtered <- countyFund %>% dplyr::filter(region == input$state & year ==input$year )
        }        
        p <- ggplot()+
            geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),colour="black",fill="white")+
            geom_polygon(data=filtered, aes(x = long, y = lat, group = group, 
                                            fill = federalShare/projectAmount))+
            labs(fill="Proportion of Federal Obligated Funds") +
            ggtitle(paste("Porportion Federal Obligated Funds for Year", input$year,  "by County")) +
            theme(plot.title = element_text(hjust = 0.5))
        p
    })
    
    output$stateMap <- renderPlot({
    if(input$state =="All" & input$year == "All")
        {
        p <- ggplot() + 
            geom_polygon(data=states, aes(x=long, y=lat, group=group),colour="black",fill="white")+
            geom_polygon(data=stateTotal, aes(x = long, y = lat, group = group, fill = projectAmount), color = "transparent")+
            labs(fill="Total Project Amount(USD)") + 
            ggtitle("Total Project Amount by State from 2009-2020") +
            theme(plot.title = element_text(hjust = 0.5))
        
    } 
        else if (input$year == "All"){
        filtered <- stateTotal %>% dplyr::filter(region == input$state)
        p <- ggplot() + 
            geom_polygon(data=states, aes(x=long, y=lat, group=group),colour="black",fill="white")+
            geom_polygon(data=filtered, aes(x = long, y = lat, group = group, fill = projectAmount), color = "transparent")+
            labs(fill="Total Project Amount(USD)") + 
            ggtitle(paste(input$state, "Total Project Amount by State from 2009-2020") ) +
            theme(plot.title = element_text(hjust = 0.5))
    } 
        else if (input$state == "All"){
        filtered <- stateYear %>% dplyr::filter(year == input$year)
        filtered <- merge(filtered, MainStates, by = c("region"))
        p <- ggplot() + 
            geom_polygon(data=states, aes(x=long, y=lat, group=group),colour="black",fill="white")+
            geom_polygon(data=filtered, aes(x = long, y = lat, group = group, fill = projectAmount), color = "transparent")+
            labs(fill="Total Project Amount(USD)") + 
            ggtitle(paste("FEMA Project Amount for", input$year, "by State") ) +
            theme(plot.title = element_text(hjust = 0.5))
    }
    else {
        filtered <- stateYear %>% dplyr::filter(region == input$state & year == input$year)
        filtered <- merge(filtered, MainStates, by = c("region"))
        p <- ggplot() + 
            geom_polygon(data=states, aes(x=long, y=lat, group=group),colour="black",fill="white")+
            geom_polygon(data=filtered, aes(x = long, y = lat, group = group, fill = projectAmount), color = "transparent")+
            labs(fill="Project Amount(USD)") + 
            ggtitle(paste(input$state, "FEMA Project Amount") ) +
            theme(plot.title = element_text(hjust = 0.5))
    }
    p
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
