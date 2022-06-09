library(shiny)
library(tidyverse)
library(tidycensus)
library(plotly)
library(sf)
library(tmap)

# Load Data from get_data.R for now

ui <- fluidPage(
  titlePanel(title = 'HFPA'),
  fluidRow(
    column(width = 3,
           ), # column
    
    column(width = 10, offset = 1,
           tabsetPanel(
             tabPanel('Map',
                      h1(textOutput(outputId = 'mapTitle')),
                      fluidRow(
                        column(width = 2, selectInput(inputId = 'basemap', label = 'Basemap:', 
                                                      choices = c('Esri.WorldTopoMap', 'OpenStreetMap', 'CartoDB'))),
                        column(width = 2, selectInput(inputId = 'yearSelect', label = 'Select Year', choices = last.10.years)), # select year
                        column(width = 2, selectInput(inputId = 'varSelect', label = 'Select Metric', 
                                                      choices = c('HFPA', 'Availability', 'Access', 'Utilization'))), # select metric
                        column(width = 3, sliderInput(inputId = 'alphaSlider', 
                                                      label = 'Adjust Map Transparency', min = 0, max = 1, value = .8)) # transparency slider
                      ), # fluidRow
                      tmapOutput(outputId = 'tmapPlot', height = '800px')), # Map
             tabPanel('Raw Data',
                      h1(textOutput(outputId = 'rawDataTitle')),
                      downloadButton(outputId = 'downloadData', label = 'Download'),
                      br(),
                      br(),
                      dataTableOutput(outputId = 'yearTable')), # Raw Data
             tabPanel('Trends',
                      fluidRow(
                        column(width = 3, selectizeInput(inputId = 'tractSelect', label = 'Select Tracts for Trend Graph', 
                                                         choices = unique(HFPA.data$GEOID), multiple = T, selected = '42003191700')), # tract select
                        column(width = 2, 
                               checkboxInput(inputId = 'trendlineCheckbox', label = 'Show Trendline', value = T), # trendline box
                               checkboxInput(inputId = 'seCheckbox', label = 'Show Standard Errors', value = F)) # se box
                        ),
                      plotlyOutput(outputId = 'comparePlot', height = '600px'),
                      br(),
                      verbatimTextOutput(outputId = 'regressionResults'),
                      plotlyOutput(outputId = 'rankedPlot', height = '600px'),
                      fluidRow(
                        column(width = 6, plotOutput(outputId = 'distPlot')),
                        column(width = 6, plotOutput(outputId = 'meanPlot'))
                        ),
                      ), # Trends
             ), #tabsetPanel
           ), # column
  ) # fluidRow
) # fluidPage

server <- function(input, output, session) {
  year = reactive({input$yearSelect})
  metric = reactive({input$varSelect})
  tracts = reactive({input$tractSelect})
  map.alpha = reactive({input$alphaSlider})
  show.trendline = reactive({input$trendlineCheckbox})
  show.se = reactive({input$seCheckbox})
  basemap = reactive({input$basemap})
  
  data.table = reactive({
    HFPA.data %>%
      st_set_geometry(NULL) %>%
      filter(year == year()) %>%
      select(GEOID, year, Availability, Access, Utilization, HFPA)
  })
  
  output$mapTitle = renderText({
    paste(metric(), ': ', year(), sep = '')
  })
  
  output$tmapPlot = renderTmap({
    tm_shape(filter(HFPA.data, year == year())) +
      tm_basemap(basemap()) +
      tm_polygons(metric(), alpha = map.alpha(), palette = 'BuPu')
  }) # render Map
  
  output$comparePlot = renderPlotly({
    p = ggplot(data = filter(HFPA.data, GEOID %in% tracts()),
               mapping = aes(x = year, y = .data[[input$varSelect]], color = GEOID)) +
      geom_line(size = 1, alpha = .5) +
      geom_point(size = 3, alpha = .5) +
      scale_color_brewer(palette = 'Set1') +
      labs(title = metric(), color = 'Tract') +
      theme_minimal()
    
    if (show.trendline() == F) {
      ggplotly(p)
    }
    else {
      p = p + geom_smooth(size = 1, method = 'lm', se = show.se())
      ggplotly(p)
    }
    
    
  }) # comparePlot
  
  output$regressionResults = renderText({
    output = ''
    for (tract in tracts()) {
      model = lm(data = filter(HFPA.data, GEOID == tract), formula = paste(metric(), '~ year'))
      year.coef = round(model$coefficients['year'], 2)
      year.pval = round(summary(model)$coef[8], 3)
      
      # Find if it is significant
      if (year.pval <= 0.05) {
        sig.code = 'Significant!'
      }
      else {sig.code = ''}
      
      regression.output = paste('Tract ', tract, 
                                ': \tBeta Coef = ', year.coef,
                                '\tp-value = ', year.pval, 
                                '\t ', sig.code, sep = '')
      #print(tract)
      output = paste(output, regression.output, '\n')
    }
    output
  }) # Regression Results
  
  output$rankedPlot = renderPlotly({
    p = ggplot(data = HFPA.data,
               mapping = aes(sample = .data[[input$varSelect]])) +
      stat_qq(geom = 'point', distribution = 'qunif') +
      facet_grid(cols = vars(year)) +
      labs(title = 'Quantile Plot by Year', x = '', y = metric()) +
      theme_bw() +
      theme(strip.background = element_rect(fill = 'gold1')) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = -1))
    ggplotly(p)
  })
  
  output$distPlot = renderPlot({
    p = ggplot(data = HFPA.data, mapping = aes(x = HFPA, color = as.factor(year))) +
      geom_density() +
      labs(title = paste(metric(), 'Distribution by Year'), x = metric(), y = 'Density') +
      theme_minimal()
    p
  })
  
  output$meanPlot = renderPlot({
    yearly.mean = HFPA.data %>%
      group_by(year) %>%
      summarize(mean = mean(HFPA, na.rm = T))
    
    p = ggplot(data = yearly.mean, mapping = aes(x = year, y = mean)) +
      geom_line(color = 'darkblue') +
      geom_point(size = 3) +
      labs(title = 'Mean HFPA by Year', x = 'Year', y = 'HFPA') +
      ylim(0, 40) +
      theme_minimal()
    p
  })
  
  output$rawDataTitle = renderText({
    paste('Raw Data for', year())
  })
  
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