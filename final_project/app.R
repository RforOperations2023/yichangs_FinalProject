#
# World Shape Data Source: http://thematicmapping.org/downloads/world_borders.php
# Data Source: https://datacatalog.worldbank.org/dataset/data-statistical-capacity
#
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
WorldMap <- subset(WorldMap, is.element(WorldMap$ISO3,SCI$Country_code))

#Align shapefile with datasets
NewData <- SCI[order(match(SCI$Country_code, WorldMap$ISO3)),]

NewData_Table <- SCI_Table[order(match(SCI_Table$Country.Code, WorldMap$ISO3)),]


#Remove row names and drop 'X' column
rownames(NewData) <- c()

rownames(NewData_Table) <- c()

NewData <- select(NewData,-c(X))

NewData_Table <- select(NewData_Table,-c(X))


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)