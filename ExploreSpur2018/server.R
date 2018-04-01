#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)

load('ChildFilteredData.Rda')
# loads data into variable ChildFilteredFindingAnonymisedData, which is a mouthful
# so copy into variable with shorter name
patient_data <- ChildFilteredFindingAnonymisedData
# just keep the columns we need
patient_data <- patient_data %>%
  select(id, Gender, Weight, Height, Age.in.years, SchoolName, Age.in.months,
         Zstat, Pstat, Zweight, Pweight, bmi, Zbmi, Pbmi, Finding, Finding.Category)
# by default, each child could have multiple rows if they have multiple findings
# create a data table which just has unique child names
unique_child <- patient_data %>%
  distinct(id, .keep_all = TRUE) %>%
  # for this table we don't need finding list (since we need to refer to original table)
  select(-Finding, -Finding.Category)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$age1 <- renderText(input$AgeRange[1])
  output$age2 <- renderText(input$AgeRange[2])
  output$g1 <- renderText(input$Gender1)
    
  output$plot1 <- renderPlotly({
    gender <- input$Gender1
    if (gender == 'Any') {
      gender_list <- c('Female','Male')
    } else if (gender == 'Female') {
      gender_list <- 'Female' 
    } else {
      gender_list <- 'Male'
    }
    data_subset <- unique_child %>%
      filter(Gender %in% gender_list) %>%
      filter(!is.na(Height)) %>% # this will be a height plot
      filter(!is.na(Age.in.years)) %>% # must be age data
      filter(Age.in.years >= input$AgeRange[1]) %>%
      filter(Age.in.years <= input$AgeRange[2])

    if (1==1) {ggplotly(NULL,'msg')}
    
    ribbon <- loess(Height ~ Age.in.years,
                data = data_subset)
    
    p <- data_subset %>%
      plot_ly(x = ~Age.in.years, color = I('red')) %>%
      add_markers(y = ~Height,
                  type = 'scatter', mode = 'markers', name = ~gender,
                  showlegend = FALSE,
                  hoverinfo = 'text',
                  text = ~paste('Age : ', sprintf('%.1f', Age.in.years),
                                '<br>Height :', sprintf('%.1f', Height),
                                '<br>Height-z : ', sprintf('%.1f', Zstat),
                                '<br>Height-% : ', sprintf('%.0f', Pstat*100),
                                '<br>ID :',id)) %>%
      add_lines(y = ~fitted(loess(Height ~ Age.in.years)),
                line = list(color = 'rgba(7,164,181,1)'),
                showlegend = FALSE) %>%
      add_ribbons(data = augment(mf),
                  ymin = ~.fitted - 1.96 * .se.fit,
                  ymax = ~.fitted + 1.96 * .se.fit,
                  line = list(color = 'rgba(7,164,181,0.05)'),
                  fillcolor = 'rgba(7,164,181,0.2)',
                  showlegend = FALSE) %>%
      layout(title = 'Height (cm) vs Age
             Spur Afrika Clinic, January 2018',
             xaxis = list(title = 'Age in years'),
             yaxis = list(title = 'Height (cm)'))
    p
  })
})
