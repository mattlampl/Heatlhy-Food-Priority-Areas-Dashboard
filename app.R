library(shiny)
library(tidyverse)
library(tidycensus)
library(plotly)
library(sf)

# Load Data from get_data.R for now

ui <- fluidPage(
  titlePanel(title = 'HFPA'),
  fluidRow(
    column(width = 3,
           selectInput(inputId = 'yearSelect', label = 'Select Year', choices = last.5.years),
           selectInput(inputId = 'varSelect', label = 'Select Metric for Map and Trend Graph', 
                       choices = c('HFPA', 'Availability', 'Access', 'Utilization')),
           selectizeInput(inputId = 'tractSelect', label = 'Select Tracts for Trend Graph', 
                          choices = unique(HFPA.data$GEOID), multiple = T, selected = '42003191700')
           ), # column
    
    column(width = 8,
           tabsetPanel(
             tabPanel('Map', plotlyOutput(outputId = 'mapPlot', height = '900px')),
             tabPanel('Trends', plotlyOutput(outputId = 'comparePlot', height = '600px')),
             tabPanel('Raw Data',
                      downloadLink(outputId = 'downloadData', label = 'Download'),
                      dataTableOutput(outputId = 'yearTable'))
             ), #tabsetPanel
           ), # column
  ) # fluidRow
) # fluidPage

server <- function(input, output, session) {
  year = reactive({input$yearSelect})
  metric = reactive({input$varSelect})
  tracts = reactive({input$tractSelect})
  data.table = reactive({
    HFPA.data %>%
      st_set_geometry(NULL) %>%
      filter(year == year()) %>%
      select(GEOID, year, Availability, Access, Utilization, HFPA)
  })
  
  output$mapPlot = renderPlotly({
    p = ggplot(data = filter(HFPA.data, year == year()), 
               aes(fill = .data[[input$varSelect]], text = paste('Tract:', GEOID))) + # Plot only the PGH Tracts
      geom_sf(color = 'black', size = 0.2) +
      scale_fill_distiller(palette = 'GnBu', direction = 1) +
      theme_void() +
      labs(title = paste(metric(), ': ', year(), sep = ''))
    ggplotly(p)
  }) # mapPlot
  
  output$comparePlot = renderPlotly({
    p = ggplot(data = filter(HFPA.data, GEOID %in% tracts()),
               mapping = aes(x = year, y = .data[[input$varSelect]], color = GEOID)) +
      geom_smooth(size = 1, method = 'lm', se = F) +
      geom_line(size = 1, alpha = .5) +
      geom_point(size = 3, alpha = .5) +
      scale_color_brewer(palette = 'Set1') +
      labs(title = metric(), color = 'Tract') +
      theme_minimal()
    ggplotly(p)
  }) # comparePlot
  
  output$yearTable = renderDataTable({
    data.table()
  }) #yearTable
  
  output$downloadData = downloadHandler(
    filename = function(){
      paste('HFPA_data_', year(), '.csv', sep='')
    },
    content = function(con){
      write.csv(x = data.table(), con)
    }
  )
  
}

shinyApp(ui, server)