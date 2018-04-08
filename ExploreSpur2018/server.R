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
library(broom)

# load tables of 'normal' range for stature, weight, height
load('StatForAge.Rda')
load('WeightForAge.Rda')
load('BMIForAge.Rda')
# load patient data
load('ChildFilteredData.Rda')
# loads data into variable ChildFilteredFindingAnonymisedData, which is a mouthful
# so copy into variable with shorter name
patient_data <- ChildFilteredFindingAnonymisedData
# just keep the columns we need
patient_data <- patient_data %>%
  select(id, Gender, Weight, Height, Age.in.years, SchoolName, Age.in.months,
         Zstat, Pstat, Zweight, Pweight, bmi, Zbmi, Pbmi, Finding, Finding.Category,
         StatForAgeRows, WeightForAgeRows, BMIForAgeRows)
# by default, each child could have multiple rows if they have multiple findings
# create a data table which just has unique child names
unique_child <- patient_data %>%
  distinct(id, .keep_all = TRUE) %>%
  # for this table we don't need finding list (since we need to refer to original table)
  select(-Finding, -Finding.Category)

# will use data_subset to reactively hold subset data
data_subset <- list()

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  gender_list1 <- reactive({
    if (input$Gender1 == 'Any') {
      c('Female','Male')
    } else if (input$Gender1 == 'Female') {
      'Female' 
    } else {
      'Male'
    }
  })
  
  metric1 <- reactive({
    if (input$Metric1 == 'Height') {
      'Height'
    } else if (input$Metric1 == 'Weight') {
      'Weight'
    } else if (input$Metric1 == 'Body Mass Index') {
      'bmi'
    }
  })

  metric_cluster1 <- reactive({
    if (input$Metric1 == 'Height') {
      c('Height', 'Zstat', 'Pstat')
    } else if (input$Metric1 == 'Weight') {
      c('Weight', 'Zstat', 'Pstat')
    } else if (input$Metric1 == 'Body Mass Index') {
      c('bmi', 'Zbmi', 'Pbmi')
    }
  })
  
  metric_namecluster1 <- reactive({
    if (input$Metric1 == 'Height') {
      c('Height', 'Height-z', 'Height-%')
    } else if (input$Metric1 == 'Weight') {
      c('Weight', 'Weight-z', 'Weight-%')
    }
  })
  
  yaxis_name1 <- reactive({
    if (input$Metric1 == 'Height') {
      'Height (cm)'
    } else if (input$Metric1 == 'Weight') {
      'Weight (kg)'
    } else if (input$Metric1 == 'Body Mass Index') {
      c('BMI', 'BMI-z', 'BMI-%')
    }
  })
  
  percentiles1 <- reactive({
    if (input$Metric1 == 'Height') {
      'StatForAgeRows'
    } else if (input$Metric1 == 'Weight') {
      'WeightForAgeRows'
    } else if (input$Metric1 == 'Body Mass Index') {
      'BMIForAgeRows'
    }
  })
  
  data_subset[[1]] <- reactive({
    unique_child %>%
      filter(Gender %in% gender_list1()) %>%
      filter(!is.na(!!metric1())) %>% # is the metric present?
      filter(!is.na(Age.in.years)) %>% # must be age data
      filter(Age.in.years >= input$AgeRange[1]) %>%
      filter(Age.in.years <= input$AgeRange[2])
  })
  
  add_percentiles <- function(p, gender, metric, minmax_rows) {
    p <- p %>%
      add_lines(x = ~StatForAge[[gender]][minmax_rows,]$Month/12,
                y = ~StatForAge[[gender]][minmax_rows,]$P3,
                line = list(color = 'rgba(0,0,0,0.1)', dash = 'dash'),
                name = '3rd centile',
                showlegend = FALSE,
                hoverinfo = 'text',
                text = ~paste('3rd centile')) %>%
      add_lines(x = ~StatForAge[[gender]][minmax_rows,]$Month/12,
                y = ~StatForAge[[gender]][minmax_rows,]$P25,
                line = list(color = 'rgba(0,0,0,0.1)'),
                name = '25th centile',
                showlegend = FALSE,
                hoverinfo = 'text',
                text = ~paste('25th centile')) %>%
      add_lines(x = ~StatForAge[[gender]][minmax_rows,]$Month/12,
                y = ~StatForAge[[gender]][minmax_rows,]$P50,
                line = list(color = 'rgba(0,0,0,0.25), width = 5'),
                name = '50th centile',
                showlegend = FALSE,
                hoverinfo = 'text',
                text = ~paste('50th centile')) %>%
      add_lines(x = ~StatForAge[[gender]][minmax_rows,]$Month/12,
                y = ~StatForAge[[gender]][minmax_rows,]$P75,
                line = list(color = 'rgba(0,0,0,0.1)'),
                name = '75th centile',
                showlegend = FALSE,
                hoverinfo = 'text',
                text = ~paste('75th centile')) %>%
      add_lines(x = ~StatForAge[[gender]][minmax_rows,]$Month/12,
                y = ~StatForAge[[gender]][minmax_rows,]$P97,
                line = list(color = 'rgba(0,0,0,0.1)', dash = 'dash'),
                name = '97th centile',
                showlegend = FALSE,
                hoverinfo = 'text',
                text = ~paste('97th centile'))
    return(p)
  }
  
  output$stature1_median <- renderText({sprintf('Median height: %.2f',median(data_subset[[1]]()$Height))})
  output$stature1_mean <- renderText({sprintf('Mean height: %.2f', mean(data_subset[[1]]()$Height))})
  output$stature1_sd <- renderText({sprintf('SD height: %.2f', sd(data_subset[[1]]()$Height))})
  output$age1_median <- renderText({sprintf('Median age: %.2f', median(data_subset[[1]]()$Age.in.years))})
  output$age1_mean <- renderText({sprintf('Mean age: %.2f', mean(data_subset[[1]]()$Age.in.years))})
  output$age1_sd <- renderText({sprintf('SD age: %.2f', sd(data_subset[[1]]()$Age.in.years))})

  output$plot1 <- renderPlotly({
    gender <- input$Gender1

    if (length(data_subset[[1]]()$id) == 0) {
      plotly_empty() # no data to plot
    } else {
      
      # Loess ribbon
      ribbon <- loess(as.formula(paste(metric1(),' ~ Age.in.years')),
                      data = data_subset[[1]]())
      
      # minimum/maximum rows of percentiles rows required for plotting
      minmax_rows <- min(data_subset[[1]]()[[percentiles1()]]):max(data_subset[[1]]()[[percentiles1()]])

      p <- data_subset[[1]]() %>%
        plot_ly(x = ~Age.in.years) %>%
        add_markers(y = ~get(metric1()),
                    type = 'scatter', mode = 'markers', color = ~Gender,
                    showlegend = FALSE,
                    hoverinfo = 'text',
                    text = ~paste('Age : ', sprintf('%.1f', Age.in.years),
                                  '<br>', metric_namecluster1()[1],' : ', sprintf('%.1f', get(metric_cluster1()[1])),
                                  '<br>', metric_namecluster1()[2],' : ', sprintf('%.1f', get(metric_cluster1()[2])),
                                  '<br>', metric_namecluster1()[3],' : ', sprintf('%.0f', get(metric_cluster1()[3])*100),
                                  '<br>ID :',id)) %>%
        add_lines(y = ~fitted(loess(as.formula(paste(metric1(),' ~ Age.in.years')))),
                  line = list(color = 'rgba(7,164,181,1)'),
                  showlegend = FALSE) %>%
        add_ribbons(data = augment(ribbon),
                    ymin = ~.fitted - 1.96 * .se.fit,
                    ymax = ~.fitted + 1.96 * .se.fit,
                    line = list(color = 'rgba(7,164,181,0.05)'),
                    fillcolor = 'rgba(7,164,181,0.2)',
                    showlegend = FALSE) %>%
        layout(title = paste(yaxis_name1(),' vs Age
               Spur Afrika Clinic, January 2018'),
               xaxis = list(title = 'Age in years'),
               yaxis = list(title = yaxis_name1()))
      if (gender != 'Any') {
        if (metric1() %in% c('Height','Weight','BMI')) {
          p <- add_percentiles(p, gender, metric1(), minmax_rows)
        }
      }
      p
    }
  })

  output$plot2 <- renderPlotly({
    gender <- input$Gender2
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
    
    if (length(data_subset$id) == 0) {
      plotly_empty() # no data to plot
    } else {
      # loess ribbon
      ribbon <- loess(Height ~ Age.in.years,
                      data = data_subset)
      
      # minimum/maximum rows of StatForAge rows required for plotting
      minmax_rows <- min(data_subset$StatForAgeRows):max(data_subset$StatForAgeRows)
      
      p <- data_subset %>%
        plot_ly(x = ~Age.in.years) %>%
        add_markers(y = ~Height,
                    type = 'scatter', mode = 'markers', color = ~Gender,
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
        add_ribbons(data = augment(ribbon),
                    ymin = ~.fitted - 1.96 * .se.fit,
                    ymax = ~.fitted + 1.96 * .se.fit,
                    line = list(color = 'rgba(7,164,181,0.05)'),
                    fillcolor = 'rgba(7,164,181,0.2)',
                    showlegend = FALSE) %>%
        layout(title = 'Height (cm) vs Age
               Spur Afrika Clinic, January 2018',
               xaxis = list(title = 'Age in years'),
               yaxis = list(title = 'Height (cm)'))
      if (gender != 'Any') {
        p <- p %>%
          add_lines(x = ~StatForAge[[gender]][minmax_rows,]$Month/12,
                    y = ~StatForAge[[gender]][minmax_rows,]$P3,
                    line = list(color = 'rgba(0,0,0,0.1)', dash = 'dash'),
                    name = '3rd centile',
                    showlegend = FALSE,
                    hoverinfo = 'text',
                    text = ~paste('3rd centile')) %>%
          add_lines(x = ~StatForAge[[gender]][minmax_rows,]$Month/12,
                    y = ~StatForAge[[gender]][minmax_rows,]$P25,
                    line = list(color = 'rgba(0,0,0,0.1)'),
                    name = '25th centile',
                    showlegend = FALSE,
                    hoverinfo = 'text',
                    text = ~paste('25th centile')) %>%
          add_lines(x = ~StatForAge[[gender]][minmax_rows,]$Month/12,
                    y = ~StatForAge[[gender]][minmax_rows,]$P50,
                    line = list(color = 'rgba(0,0,0,0.25), width = 5'),
                    name = '50th centile',
                    showlegend = FALSE,
                    hoverinfo = 'text',
                    text = ~paste('50th centile')) %>%
          add_lines(x = ~StatForAge[[gender]][minmax_rows,]$Month/12,
                    y = ~StatForAge[[gender]][minmax_rows,]$P75,
                    line = list(color = 'rgba(0,0,0,0.1)'),
                    name = '75th centile',
                    showlegend = FALSE,
                    hoverinfo = 'text',
                    text = ~paste('75th centile')) %>%
          add_lines(x = ~StatForAge[[gender]][minmax_rows,]$Month/12,
                    y = ~StatForAge[[gender]][minmax_rows,]$P97,
                    line = list(color = 'rgba(0,0,0,0.1)', dash = 'dash'),
                    name = '97th centile',
                    showlegend = FALSE,
                    hoverinfo = 'text',
                    text = ~paste('97th centile'))
      }
      p
    }
  })
})
