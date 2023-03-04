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


#Bins for choropleth
bins <- c(0,10,20,30,40,50,60,70,80,90,100)
pal <- colorBin('RdYlBu', domain = c(0,100), bins = bins)


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
                  selected = countriesAplhabeticalOrder[1]),
      radioButtons("dataMeasure", "Measure", choices=c('Average Score' = 'Average',
                                                       'Periodicity Score' = 'Periodicity',
                                                       'Source Score' = 'Source',
                                                       'Methodology Score' = 'Methodology'))
      
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
                              box(width = NULL, solidHeader = TRUE, leafletOutput("worldMap", height=400)),
                              #Line plot
                              box(width = NULL,
                                  plotOutput("countryPlot")
                              ),
                              box(width = NULL,
                                  plotOutput("yearPlot")))
                     )
              )
      )
    )
  )
)


#server
server <- function(input, output, session) {
  #Data used for Choropleth Map
  data_input <- reactive({
    NewData %>%
      filter(Year == input$dataYear) %>%
      filter(Indicator_Name == input$dataMeasure)
    
  })
  
  #align data
  data_input_ordered <- reactive({
    data_input()[order(match(data_input()$Country_code, WorldMap$ISO3)),]
  })
  
  
  #output map
  output$worldMap <- renderLeaflet({
    leaflet() %>%
      setView(0, 32, 2) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = WorldMap, 
                  weight = 1, 
                  smoothFactor = 0.5, 
                  color='white', 
                  fillOpacity = 0.8,
                  fillColor = pal(data_input_ordered()$Percentage),
                  highlightOptions = highlightOptions(color = "black", weight = 3,
                                                      bringToFront = TRUE)) %>%
      addLegend(pal = pal,
                title = "SCI Score",
                values = data_input_ordered()$Percentage,
                opacity = 0.7,
                position = 'topright')
  })
  
  observeEvent(input$WorldMap_shape_click, {
    p <- input$WorldMap_shape_click
    print(p)
  })
  
  
  #Line Plot Output
  data_input_plot <- reactive({
    NewData %>%
      filter(Indicator_Name == input$dataMeasure) %>%
      filter(NAME == input$country)
  })
  
  output$countryPlot = renderPlot({
    ggplot(data_input_plot()) +
      geom_line(mapping = aes(x = unique(NewData$Year),
                              y = data_input_plot()$Percentage, 
                              colour = data_input_plot()$NAME),
                color='darkblue') + 
      labs(x = "Years", y = "Score", 
           title = paste("SCI Score for", unique(data_input_plot()$NAME))) +
      scale_x_continuous(breaks=pretty_breaks()) +
      scale_colour_discrete(name = "Country")
  })
  
  output$yearPlot = renderPlot({
    ggplot(data_input()) +
      geom_line(mapping = aes(x = unique(NewData$Year),
                              y = data_input_plot()$Percentage, 
                              colour = data_input_plot()$NAME),
                color='darkblue') + 
      labs(x = "Years", y = "Score", 
           title = paste("SCI Score for", unique(data_input_plot()$NAME))) +
      scale_x_continuous(breaks=pretty_breaks()) +
      scale_colour_discrete(name = "Country")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


