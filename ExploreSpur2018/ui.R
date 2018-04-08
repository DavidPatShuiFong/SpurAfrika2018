#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# prepare data for use
# data collected from SpurAfrika 2018
# processed and identifying data removed
load('ChildFilteredData.Rda')
# loads data into variable ChildFilteredFindingAnonymisedData, which is a mouthful
# so copy into variable with shorter name
child_data <- ChildFilteredFindingAnonymisedData

# need minimum maximum age for use in age sliders later
minimum_age <- round(min(child_data$Age.in.years)-1)
maximum_age <- round(max(child_data$Age.in.years)+1)

# Define UI
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Spur Afrika, January 2018 clinic - Nairobi, Kenya - Child Data subset"),
  
  # Sidebar with a slider input for number of bins 

  fluidRow(
    column(2,
           h3('Age selection'),
           sliderInput("AgeRange",
                       "Age range (years):",
                       min = minimum_age,
                       max = maximum_age,
                       value = c(minimum_age,maximum_age))
           )
  ),
  fluidRow(
    column(2,
           h2('Left Chart'),
           selectInput('Gender1',
                       'Gender',
                       choices = c('Any', 'Female', 'Male')),
           h2('Measurement'),
           selectInput('Metric1',
                       'Metric',
                       choices = c('Height', 'Weight', 'Body Mass Index',
                                   'Height-z', 'Weight-z', 'Body Mass Index-z')),
           h2('Summary statistics'),
           selectInput('Stat1',
                       'Statistic',
                       choices = c('Loess', 'Linear Model')),
           h3('List of conditions'),
           selectInput('Include1',
                       'Include or Exclude',
                       choices = c('No condition filter', 'Include', 'Exclude')),
           selectInput('Conditions1',
                       'Conditions',
                       # sorted list of unique findings
                       choices = sort(unique(child_data$Finding)), multiple = TRUE, selectize = TRUE)
    ),
    # Show a plot of the generated distribution
    column(4,
           plotlyOutput('plot1'),
           textOutput('list1')
    ),
    column(4,
           plotlyOutput('plot2')
    ),
    column(2,
           h2('Right Chart'),
           selectInput('Gender2',
                       'Gender',
                       choices = c('Any','Female','Male'))
    )
  )
))
