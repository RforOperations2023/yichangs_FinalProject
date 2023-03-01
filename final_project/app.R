#
# World Shape Data Source: http://thematicmapping.org/downloads/world_borders.php
# Data Source: https://datacatalog.worldbank.org/dataset/data-statistical-capacity
#

library(shiny)
library(rgdal)
library(dplyr)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(scales)
library(rsconnect)


# Load Data
df <- read.csv('data.csv', header=TRUE, sep=",")

# Measures divided into columns 
df_table <- read.csv("data_table.csv", header=TRUE, sep=",")

# world shape file
WorldMap <- readOGR('TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp')

#remove unused datapoints for World Map
WorldMap <- subset(WorldMap, is.element(WorldMap$ISO3,df$Country_code))

#Align shapefile with datasets
NewData <- df[order(match(df$Country_code, WorldMap$ISO3)),]

NewData_Table <- df_table[order(match(df_table$Country.Code, WorldMap$ISO3)),]


#Remove row names and drop 'X' column
rownames(NewData) <- c()

rownames(NewData_Table) <- c()

NewData <- select(NewData,-c(X))

NewData_Table <- select(NewData_Table,-c(X))


#Sorting years
yearRange<-sort(unique(as.numeric(NewData$Year)), decreasing=TRUE)

#Sorting countries
countriesAplhabeticalOrder <- sort(unique(NewData$NAME), decreasing = FALSE)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "ShinyR Final Project",
                  titleWidth = 650
  ),
  dashboardSidebar(
    # sidebarmenu
    sidebarMenu(
      id = "sidebar",
      
      #first menuitem
      menuItem("Dataset", tabName = "data", icon=icon("chart-line")),
      menuItem(text = "Visualization", tabName = "viz", icon=icon("chart-line")),
      
      selectInput("dataYear", "Year", choices=yearRange, selected=yearRange[1]),
      selectInput('country','Country', choices = countriesAplhabeticalOrder, 
                  multiple = FALSE, 
                  selected = countriesAplhabeticalOrder[1])
      
    )
  ),
  dashboardBody(
    tabItems(
      #first tab item 
      tabItem(tabName = "data",
              #tab box
              tabBox(id="t1", width=12,
                     tabPanel("About", icon=icon("address-card"), fluidRow(
                       column(width = 4, tags$br(),
                              tags$p("Introduction"))
                     )),
                     tabPanel(title = "Data", icon=icon("address-card"), dataTableOutput("dataT"))
              )
      ),
      
      # second tab item or landing page
      tabItem(tabName = "viz",
              tabBox(id="t2", width = 12,
                     fluidRow(
                       column(width = 10,
                              #Leaflet Map
                              box(width = NULL, solidHeader = TRUE, leafletOutput("worldMap", height=400)))
                     )
              ),
      )
    )
  )
)


#server
server <- function(input, output, session) {
  
  #output map
  output$worldMap <- renderLeaflet({
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


