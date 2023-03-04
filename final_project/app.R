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
  dashboardHeader(title = "Final Project - SCI",
                  titleWidth = 650
  ),
  dashboardSidebar(
    # sidebarmenu
    sidebarMenu(
      id = "sidebar",
      
      #first menuitem
      menuItem("Dataset", tabName = "data", icon=icon("chart-line")),
      
      #second menuitem
      menuItem(text = "Visualization", tabName = "viz", icon=icon("chart-line")),
      
      #first input
      selectInput("dataYear", "Year", choices=yearRange, selected=yearRange[1]),
      
      #second input
      selectInput('country','Country', choices = countriesAplhabeticalOrder, 
                  multiple = FALSE, 
                  selected = countriesAplhabeticalOrder[1]),
      
      #third input
      radioButtons("dataMeasure", "Measure", choices=c('Average Score' = 'Average',
                                                       'Periodicity Score' = 'Periodicity',
                                                       'Source Score' = 'Source',
                                                       'Methodology Score' = 'Methodology'))
      
    )
  ),
  
  #dashboard body
  dashboardBody(
    tabItems(
      #first tab item 
      tabItem(tabName = "data",
              #tab box
              tabBox(id="t1", width=12,
                     tabPanel("About", icon=icon("address-card"), fluidRow(
                       column(width = 6, tags$br(),
                              tags$p("The Statistical Capacity Indicator (SCI) 
                              is a composite index that measures the capacity of 
                              a country's statistical system to collect, process, 
                              and disseminate reliable and timely data. It was 
                              developed by the World Bank in collaboration with 
                              other international organizations such as the 
                              United Nations, the International Monetary Fund, 
                              and the Organization for Economic Cooperation and 
                              Development.
                              
                              The SCI is based on 25 indicators grouped into three 
                              dimensions: methodology, source data, and periodicity 
                              and timeliness. The methodology dimension assesses 
                              the adequacy of the statistical techniques used to 
                              collect and process data. The source data dimension 
                              measures the availability and quality of the data 
                              sources used. The periodicity and timeliness dimension 
                              evaluates the frequency and speed of data dissemination.
                              The SCI ranges from 0 to 100, with higher scores 
                              indicating stronger statistical capacity. 
                              
                              A score of 50 or above is considered to be sufficient for 
                              countries to meet their basic statistical needs. 
                              The SCI is used by development agencies and 
                              policymakers to identify gaps in statistical 
                              capacity and to prioritize investments in 
                                     statistical systems."))
                     )),
                     tabPanel(title = "Data", icon=icon("address-card"), 
                              dataTableOutput("worldTable"), br(),br(), 
                              downloadButton("downloadData", "Download"))
              )
      ),
      
      # second tab item or landing page
      tabItem(tabName = "viz",
              tabBox(id="t2", width = 12,
                     fluidRow(
                       column(width = 10,
                              #Leaflet Map
                              box(width = NULL, solidHeader = TRUE, 
                                  leafletOutput("worldMap", height=400)),
                              #Line plot
                              box(width = NULL,
                                  plotOutput("countryPlot")
                              ),
                              #Box plot
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
  
  #Data used for Table
  data_input_Table <- reactive({
    NewData_Table %>%
      filter(Year == input$dataYear) 
  })
  
  #align data
  data_input_Table_Ordered <- reactive({
    data_input_Table()[order(match(data_input_Table()$Country.Code, WorldMap$ISO3)),]
  })
  
  
  #Table output
  output$worldTable <- renderDataTable(NewData_Table,
                                       server = FALSE, 
                                       colnames = c('Country', 'Country Code', 
                                                    'Year', 'Periodicity', 'Source',
                                                    'Methodology','Average'),
                                       options = list(pageLength = 5, autoWidth = TRUE),
                                       rownames= FALSE
  )
  
  
  #Map labels when hovered over
  labels <- reactive({
    paste('<p>', '<b>', data_input_Table_Ordered()$NAME, '</b>', 
          ' (', data_input_Table_Ordered()$Country.Code, ')', '<p>',
          '<p>', '<b>', 'Average Score: ', 
          round(data_input_Table_Ordered()$Average, digits = 3),'</b>', '<p>',
          '<p>', 'Periodicity Score: ', 
          round(data_input_Table_Ordered()$Periodicity, digits = 3),'<p>',
          '<p>', 'Source Score: ', 
          round(data_input_Table_Ordered()$Source, digits = 3),'<p>',
          '<p>', 'Methodology Score: ', 
          round(data_input_Table_Ordered()$Methodology, digits = 3),'<p>') 
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
                                                      bringToFront = TRUE),
                  label = lapply(labels(), HTML), group = "Default") %>%
      addLegend(pal = pal,
                title = "SCI Score",
                values = data_input_ordered()$Percentage,
                opacity = 0.7,
                position = 'topright') %>%
      addLayersControl(
                baseGroups = c("Default",
                     "Map with Markers")
    )
  })
  
  
  # observe add maker layer selections
  observe({
    
    # plot proxy
    leafletProxy("worldMap", data = WorldMap) %>%
    addAwesomeMarkers(lng = -2.79545, lat = 54.04321, 
                      layerId = "hospital", 
                      label = "Hospital",
                      group = "Map with Markers")
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
  
  
  # Bar Charts - Country wise trend
  output$yearPlot <- renderPlot({
    ggplot(data = data_input(), aes_string(x = "NAME", y = data_input()$Percentage)) 
    + geom_bar(stat = "identity", fill="orange") +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
      labs(x = "Countries", y = "Score", title=paste0("SCI Score for ", data_input()$Year))
  })
  
  # download data function
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(NewData_Table, file)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


