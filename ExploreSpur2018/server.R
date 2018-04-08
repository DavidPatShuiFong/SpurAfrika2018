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
unique_ids <- unique_child$id # list of unique IDs

# will use data_subset to reactively hold subset data
data_subset <- list() # age filter
data_condition_set <- list() # condition filter

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  observeEvent(input$copyfindings, {
    # copies conditions from left plot selection to right plot
    updateSelectInput(session, 'Conditions2',
                      selected = input$Conditions1)
  })

  gender_list1 <- reactive({
    if (input$Gender1 == 'Any') {
      c('Female','Male')
    } else if (input$Gender1 == 'Female') {
      'Female' 
    } else {
      'Male'
    }
  })

  gender_list2 <- reactive({
    if (input$Gender2 == 'Any') {
      c('Female','Male')
    } else if (input$Gender2 == 'Female') {
      'Female' 
    } else {
      'Male'
    }
  })
    
  metric1 <- reactive({
    # metric to plot
    return (metric_variable(input$Metric1))
  })

  metric2 <- reactive({
    # metric to plot
    return (metric_variable(input$Metric2))
  })
  
  metric_variable <- function (choice) {
    # return variable name depending on choice
    # used by reactive variable metric1
    
    return (list('Height', 'Weight', 'bmi', 'Zstat', 'Zweight', 'Zbmi')[[
      match(choice,
            c('Height', 'Weight', 'Body Mass Index',
              'Height-z', 'Weight-z', 'Body Mass Index-z'))]])
  }
  
  metric_cluster1 <- reactive({
    # group of variables to put in point labels of data
    
    return (metric_cluster(input$Metric1))
  })
  
  metric_cluster2 <- reactive({
    # group of variables to put in point labels of data
    
    return (metric_cluster(input$Metric2))
  })
  
  metric_cluster <- function (choice) {
    # return labels depending on choice
    # used by reactive variable metric_cluster1
    
    return (list(c('Height', 'Zstat', 'Pstat'),
                 c('Weight', 'Zstat', 'Pstat'),
                 c('bmi', 'Zbmi', 'Pbmi'),
                 c('Height', 'Zstat', 'Pstat'),
                 c('Weight', 'Zstat', 'Pstat'),
                 c('bmi', 'Zbmi', 'Pbmi'))[[
                   match(choice,
                         c('Height', 'Weight', 'Body Mass Index',
                           'Height-z', 'Weight-z', 'Body Mass Index-z'))]])
  }
    
  metric_namecluster1 <- reactive({
    # variable names depending on metric choice
    
    return (metric_namecluster(input$Metric1))
            
  })

  metric_namecluster2 <- reactive({
    # variable names depending on metric choice
    
    return (metric_namecluster(input$Metric2))
    
  })
  
  metric_namecluster <- function (choice) {
    # return variable names depending on choice
    # used by reactive variable metric_namecluster1
    
    return (list(c('Height', 'Height-z', 'Height-%'),
                 c('Weight', 'Weight-z', 'Weight-%'),
                 c('BMI', 'BMI-z', 'BMI-%'),
                 c('Height', 'Height-z', 'Height-%'),
                 c('Weight', 'Weight-z', 'Weight-%'),
                 c('BMI', 'BMI-z', 'BMI-%'))[[
                   match(choice,
                         c('Height', 'Weight', 'Body Mass Index',
                           'Height-z', 'Weight-z', 'Body Mass Index-z'))]])
  }
  
  yaxis_name1 <- reactive({
    # name of the y-axis, also used for name of chart
    
    return (yaxis_name(input$Metric1))
  })

  yaxis_name2 <- reactive({
    # name of the y-axis, also used for name of chart
    
    return (yaxis_name(input$Metric2))
  })
    
  yaxis_name <- function (choice) {
    # return axis labels depending on choice
    # used by reactive variable yaxis_name1
    
    return (list('Height (cm)', 'Weight (kg)', 'Body Mass Index (kg/m<sup>2</sup>)',
                 'Height (z)', 'Weight (z)', 'Body Mass Index (z)')[[
                   match(choice,
                         c('Height', 'Weight', 'Body Mass Index',
                           'Height-z', 'Weight-z', 'Body Mass Index-z'))]])
  }
  
  percentiles1 <- reactive({
    if (input$Metric1 == 'Height') {
      'StatForAgeRows'
    } else if (input$Metric1 == 'Weight') {
      'WeightForAgeRows'
    } else if (input$Metric1 == 'Body Mass Index') {
      'BMIForAgeRows'
    } else {
      NA # no percentile table look-up required for this metric
    }
  })

  percentiles2 <- reactive({
    if (input$Metric2 == 'Height') {
      'StatForAgeRows'
    } else if (input$Metric2 == 'Weight') {
      'WeightForAgeRows'
    } else if (input$Metric2 == 'Body Mass Index') {
      'BMIForAgeRows'
    } else {
      NA # no percentile table look-up required for this metric
    }
  })
  
  data_subset[[1]] <- reactive({
    unique_child %>%
      filter(Gender %in% gender_list1()) %>%
      filter(!is.na(!!metric1())) %>% # is the metric present?
      filter(!is.na(Age.in.years)) %>% # must be age data
      filter(Age.in.years >= input$AgeRange[1]) %>% # filter by age
      filter(Age.in.years <= input$AgeRange[2]) %>%
      filter(id %in% data_condition_set[[1]]()) # filter by condition
  })

  data_subset[[2]] <- reactive({
    unique_child %>%
      filter(Gender %in% gender_list2()) %>%
      filter(!is.na(!!metric2())) %>% # is the metric present?
      filter(!is.na(Age.in.years)) %>% # must be age data
      filter(Age.in.years >= input$AgeRange[1]) %>% # filter by age
      filter(Age.in.years <= input$AgeRange[2]) %>%
      filter(id %in% data_condition_set[[2]]()) # filter by condition
  })
  
  data_condition_set[[1]] <- reactive({
    if (input$Include1 == 'No condition filter') {
      return (unique(patient_data$id)) # include everyone
    } else if (input$Include1 == 'Include') {
      patient_list <- patient_data %>%
        filter(Finding %in% input$Conditions1) # filter by condition list
      return(unique(patient_list$id))
    } else if (input$Include1 == 'Exclude') {
      patient_list <- patient_data %>%
        filter(Finding %in% input$Conditions1) # filter by condition list
      id <- setdiff(unique_ids, unique(patient_list$id)) # the list of IDs which are not in the condition list
      return (id)
    }
  })

  data_condition_set[[2]] <- reactive({
    if (input$Include2 == 'No condition filter') {
      return (unique(patient_data$id)) # include everyone
    } else if (input$Include2 == 'Include') {
      patient_list <- patient_data %>%
        filter(Finding %in% input$Conditions2) # filter by condition list
      return(unique(patient_list$id))
    } else if (input$Include2 == 'Exclude') {
      patient_list <- patient_data %>%
        filter(Finding %in% input$Conditions2) # filter by condition list
      id <- setdiff(unique_ids, unique(patient_list$id)) # the list of IDs which are not in the condition list
      return (id)
    }
  })

  add_percentiles <- function(p, gender, metric, percentiles, minmax_rows) {
    # add percentiles lines to plotly chart
    
    # p is the plotly chart which we are adding to
    # metric is the measurement (one of Height, Weight, BMI)
    # percentiles is the name of the table (plus 'Rows') to read from 
    # minmax_rows is the pre-determind (age) rows to add
    
    percentiles <- substr(percentiles, 1, nchar(percentiles)-4)
    # the calling function will pass a name like for 'StatForAgeRows',
    # which needs to be truncated to 'StatForAge', which is the true name
    # of the required table
  
    if (metric %in% c('Height','Weight')) {
      p <- p %>%
        
        add_lines(x = ~get(percentiles)[[gender]][minmax_rows,]$Month/12,
                  y = ~get(percentiles)[[gender]][minmax_rows,]$P3,
                  line = list(color = 'rgba(0,0,0,0.1)', dash = 'dash'),
                  name = '3rd centile',
                  showlegend = FALSE,
                  hoverinfo = 'text',
                  text = ~paste('3rd centile')) %>%
        add_lines(x = ~get(percentiles)[[gender]][minmax_rows,]$Month/12,
                  y = ~get(percentiles)[[gender]][minmax_rows,]$P25,
                  line = list(color = 'rgba(0,0,0,0.1)'),
                  name = '25th centile',
                  showlegend = FALSE,
                  hoverinfo = 'text',
                  text = ~paste('25th centile')) %>%
        add_lines(x = ~get(percentiles)[[gender]][minmax_rows,]$Month/12,
                  y = ~get(percentiles)[[gender]][minmax_rows,]$P50,
                  line = list(color = 'rgba(0,0,0,0.25), width = 5'),
                  name = '50th centile',
                  showlegend = FALSE,
                  hoverinfo = 'text',
                  text = ~paste('50th centile')) %>%
        add_lines(x = ~get(percentiles)[[gender]][minmax_rows,]$Month/12,
                  y = ~get(percentiles)[[gender]][minmax_rows,]$P75,
                  line = list(color = 'rgba(0,0,0,0.1)'),
                  name = '75th centile',
                  showlegend = FALSE,
                  hoverinfo = 'text',
                  text = ~paste('75th centile')) %>%
        add_lines(x = ~get(percentiles)[[gender]][minmax_rows,]$Month/12,
                  y = ~get(percentiles)[[gender]][minmax_rows,]$P97,
                  line = list(color = 'rgba(0,0,0,0.1)', dash = 'dash'),
                  name = '97th centile',
                  showlegend = FALSE,
                  hoverinfo = 'text',
                  text = ~paste('97th centile'))
    } else if (metric == 'bmi') {
      # bmi tables contain SD lines, not percentile lines
      p <- p %>%
        add_lines(x = ~get(percentiles)[[gender]][minmax_rows,]$Month/12,
                  y = ~get(percentiles)[[gender]][minmax_rows,]$SD4neg,
                  line = list(color = 'rgba(0,0,0,0.1)', dash = 'dash'),
                  name = 'SD -4',
                  showlegend = FALSE,
                  hoverinfo = 'text',
                  text = ~paste('SD -4')) %>%
        add_lines(x = ~get(percentiles)[[gender]][minmax_rows,]$Month/12,
                  y = ~get(percentiles)[[gender]][minmax_rows,]$SD3neg,
                  line = list(color = 'rgba(0,0,0,0.1)'),
                  name = 'SD -3',
                  showlegend = FALSE,
                  hoverinfo = 'text',
                  text = ~paste('SD -3')) %>%
        add_lines(x = ~get(percentiles)[[gender]][minmax_rows,]$Month/12,
                  y = ~get(percentiles)[[gender]][minmax_rows,]$SD2neg,
                  line = list(color = 'rgba(0,0,0,0.1)', dash = 'dash'),
                  name = 'SD -2',
                  showlegend = FALSE,
                  hoverinfo = 'text',
                  text = ~paste('SD -2')) %>%
        add_lines(x = ~get(percentiles)[[gender]][minmax_rows,]$Month/12,
                  y = ~get(percentiles)[[gender]][minmax_rows,]$SD1neg,
                  line = list(color = 'rgba(0,0,0,0.1)'),
                  name = 'SD -1',
                  showlegend = FALSE,
                  hoverinfo = 'text',
                  text = ~paste('SD -1')) %>%
        add_lines(x = ~get(percentiles)[[gender]][minmax_rows,]$Month/12,
                  y = ~get(percentiles)[[gender]][minmax_rows,]$SD0,
                  line = list(color = 'rgba(0,0,0,0.25), width = 5'),
                  name = 'SD 0',
                  showlegend = FALSE,
                  hoverinfo = 'text',
                  text = ~paste('SD 0')) %>%
        add_lines(x = ~get(percentiles)[[gender]][minmax_rows,]$Month/12,
                  y = ~get(percentiles)[[gender]][minmax_rows,]$SD1,
                  line = list(color = 'rgba(0,0,0,0.1)'),
                  name = 'SD +1',
                  showlegend = FALSE,
                  hoverinfo = 'text',
                  text = ~paste('SD +1')) %>%
        add_lines(x = ~get(percentiles)[[gender]][minmax_rows,]$Month/12,
                  y = ~get(percentiles)[[gender]][minmax_rows,]$SD2,
                  line = list(color = 'rgba(0,0,0,0.1)', dash = 'dash'),
                  name = 'SD +2',
                  showlegend = FALSE,
                  hoverinfo = 'text',
                  text = ~paste('SD +2'))
      # don't add the SD+3 and SD+4 lines, those BMIs aren't very common in this dataset!
#        add_lines(x = ~get(percentiles)[[gender]][minmax_rows,]$Month/12,
#                  y = ~get(percentiles)[[gender]][minmax_rows,]$SD3,
#                  line = list(color = 'rgba(0,0,0,0.1)'),
#                  name = 'SD +3',
#                  showlegend = FALSE,
#                  hoverinfo = 'text',
#                  text = ~paste('SD +3')) %>%
#        add_lines(x = ~get(percentiles)[[gender]][minmax_rows,]$Month/12,
#                  y = ~get(percentiles)[[gender]][minmax_rows,]$SD4,
#                  line = list(color = 'rgba(0,0,0,0.1)', dash = 'dash'),
#                  name = 'SD +4',
#                  showlegend = FALSE,
#                  hoverinfo = 'text',
#                  text = ~paste('SD +4'))
    }
    
    return(p)
  }
  
  output$plot1_age <- renderPlotly({
    if (length(data_subset[[1]]()$id) == 0) {
      plotly_empty() # no data to plot
    } else {
      p <- data_subset[[1]]() %>%
        plot_ly(x = ~Age.in.years, type = 'box', name = 'Age') %>%
        layout(title = '',
               xaxis = list(title = 'Age in years'))
      p
    }
  })
  
  output$plot1_metric <- renderPlotly({
    if (length(data_subset[[1]]()$id) == 0) {
      plotly_empty() # no data to plot
    } else {
      p <- data_subset[[1]]() %>%
        plot_ly(y = ~get(metric1()), type = 'box', name = input$Metric1) %>%
        layout(title = '',
               yaxis = list(title = yaxis_name1()))
      p
    }
  })
  
  output$fit1 <- renderText({
    if (length(data_subset[[1]]()$id) > 5 & (input$Stat1 == 'Linear Model' & length(data_subset[[1]]()$id) > 5)) {
      model <- lm(as.formula(paste(metric1(),' ~ Age.in.years')),
                  data = data_subset[[1]]())
      paste(print(summary.lm(model)))
    } else {
      paste(' ')
    }
  })

  output$plot1 <- renderPlotly({
    gender <- input$Gender1

    if (length(data_subset[[1]]()$id) == 0) {
      plotly_empty() # no data to plot
    } else {

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
        layout(title = paste(yaxis_name1(),' vs Age
               Spur Afrika Clinic, January 2018'),
               xaxis = list(title = 'Age in years'),
               yaxis = list(title = yaxis_name1()),
               margin = list(t = 50)) # some space above title
      if (length(data_subset[[1]]()$id) > 5) {
        if (input$Stat1 == 'Loess' & length(data_subset[[1]]()$id) > 5) {
          # Loess ribbon
          ribbon <- loess(as.formula(paste(metric1(),' ~ Age.in.years')),
                          data = data_subset[[1]]())
        
          p <- p %>%
            add_lines(y = ~fitted(loess(as.formula(paste(metric1(),' ~ Age.in.years')))),
                      line = list(color = 'rgba(7,164,181,1)'),
                      showlegend = FALSE) %>%
            add_ribbons(data = augment(ribbon),
                        ymin = ~.fitted - 1.96 * .se.fit,
                        ymax = ~.fitted + 1.96 * .se.fit,
                        line = list(color = 'rgba(7,164,181,0.05)'),
                        fillcolor = 'rgba(7,164,181,0.2)',
                        showlegend = FALSE)
        } else if (input$Stat1 == 'Linear Model' & length(data_subset[[1]]()$id) > 5){
          ribbon <- lm(as.formula(paste(metric1(),' ~ Age.in.years')),
                          data = data_subset[[1]]())
          p <- p %>%
            add_lines(y = ~fitted(lm(as.formula(paste(metric1(),' ~ Age.in.years')))),
                      line = list(color = 'rgba(7,164,181,1)'),
                      showlegend = FALSE) %>%
            add_ribbons(data = augment(ribbon),
                      ymin = ~.fitted - 1.96 * .se.fit,
                      ymax = ~.fitted + 1.96 * .se.fit,
                      line = list(color = 'rgba(7,164,181,0.05)'),
                      fillcolor = 'rgba(7,164,181,0.2)',
                      showlegend = FALSE)
        }
      }
      if (gender != 'Any') {
        if (metric1() %in% c('Height','Weight','bmi')) {
          minmax_rows <- min(data_subset[[1]]()[[percentiles1()]]):max(data_subset[[1]]()[[percentiles1()]])
          # minimum/maximum rows of percentiles rows required for plotting
          
          p <- add_percentiles(p, gender, metric1(), percentiles1(), minmax_rows)
        }
      }
      p
    }
  })
  
  output$plot2_age <- renderPlotly({
    if (length(data_subset[[2]]()$id) == 0) {
      plotly_empty() # no data to plot
    } else {
      p <- data_subset[[2]]() %>%
        plot_ly(x = ~Age.in.years, type = 'box', name = 'Age') %>%
        layout(title = '',
               xaxis = list(title = 'Age in years'))
      p
    }
  })
  
  output$plot2_metric <- renderPlotly({
    if (length(data_subset[[2]]()$id) == 0) {
      plotly_empty() # no data to plot
    } else {
      p <- data_subset[[2]]() %>%
        plot_ly(y = ~get(metric2()), type = 'box', name = input$Metric2) %>%
        layout(title = '',
               yaxis = list(title = yaxis_name2()))
      p
    }
  })
  
  output$plot2 <- renderPlotly({
    gender <- input$Gender2
    
    if (length(data_subset[[2]]()$id) == 0) {
      plotly_empty() # no data to plot
    } else {
      
      p <- data_subset[[2]]() %>%
        plot_ly(x = ~Age.in.years) %>%
        add_markers(y = ~get(metric2()),
                    type = 'scatter', mode = 'markers', color = ~Gender,
                    showlegend = FALSE,
                    hoverinfo = 'text',
                    text = ~paste('Age : ', sprintf('%.1f', Age.in.years),
                                  '<br>', metric_namecluster2()[1],' : ', sprintf('%.1f', get(metric_cluster2()[1])),
                                  '<br>', metric_namecluster2()[2],' : ', sprintf('%.1f', get(metric_cluster2()[2])),
                                  '<br>', metric_namecluster2()[3],' : ', sprintf('%.0f', get(metric_cluster2()[3])*100),
                                  '<br>ID :',id)) %>%
        layout(title = paste(yaxis_name2(),' vs Age
                             Spur Afrika Clinic, January 2018'),
               xaxis = list(title = 'Age in years'),
               yaxis = list(title = yaxis_name2()),
               margin = list(t = 50)) # some space above title
      if (length(data_subset[[2]]()$id) > 5) {
        if (input$Stat2 == 'Loess') {
          # Loess ribbon
          ribbon <- loess(as.formula(paste(metric2(),' ~ Age.in.years')),
                          data = data_subset[[2]]())
        
          p <- p %>%
            add_lines(y = ~fitted(loess(as.formula(paste(metric2(),' ~ Age.in.years')))),
                      line = list(color = 'rgba(7,164,181,1)'),
                      showlegend = FALSE) %>%
            add_ribbons(data = augment(ribbon),
                        ymin = ~.fitted - 1.96 * .se.fit,
                        ymax = ~.fitted + 1.96 * .se.fit,
                        line = list(color = 'rgba(7,164,181,0.05)'),
                        fillcolor = 'rgba(7,164,181,0.2)',
                        showlegend = FALSE)
          } else if (input$Stat2 == 'Linear Model'){
            ribbon <- lm(as.formula(paste(metric2(),' ~ Age.in.years')),
                         data = data_subset[[2]]())
            p <- p %>%
            add_lines(y = ~fitted(lm(as.formula(paste(metric2(),' ~ Age.in.years')))),
                      line = list(color = 'rgba(7,164,181,1)'),
                      showlegend = FALSE) %>%
            add_ribbons(data = augment(ribbon),
                        ymin = ~.fitted - 1.96 * .se.fit,
                        ymax = ~.fitted + 1.96 * .se.fit,
                        line = list(color = 'rgba(7,164,181,0.05)'),
                        fillcolor = 'rgba(7,164,181,0.2)',
                        showlegend = FALSE)
          }
        }
      if (gender != 'Any') {
        if (metric2() %in% c('Height','Weight','bmi')) {
          minmax_rows <- min(data_subset[[2]]()[[percentiles2()]]):max(data_subset[[2]]()[[percentiles2()]])
          # minimum/maximum rows of percentiles rows required for plotting
          
          p <- add_percentiles(p, gender, metric2(), percentiles2(), minmax_rows)
        }
      }
      p
    }
  })
})
