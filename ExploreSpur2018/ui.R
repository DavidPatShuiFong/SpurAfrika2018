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
           ),
    column(8,
           p('Exploratory Data Analysis, for ',
             a('Spur Afrika', href = 'http://www.spurafrika.org'), '.'),
           p('Thanks to the team members, both local Kenyan and Australian, for the opportunity to explore this data.'),
           p("Documentation for this Shiny App available at",
             a("David Fong's GitHub page", href = 'https://github.com/DavidPatShuiFong/SpurAfrika2018')),
           p("Full source code for the Shiny App available in the 'ExploreSpur2018' directory"),
           p("Data used in this app pre-processed by 'SpurAfrika2018.Rmd'.")),
    column(2,
           actionButton('copyfindings',
                        'Copy Condition List'),
           p(''),
           p('Copy conditions from left plot to right plot'))
    
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
           h2('Line fitting'),
           selectInput('Stat1',
                       'Regression',
                       choices = c('Loess', 'Linear Model')),
           h3('List of conditions'),
           selectInput('Include1',
                       'Include or Exclude',
                       choices = c('No condition filter', 'Include', 'Exclude')),
           selectInput('Conditions1',
                       'Conditions',
                       # sorted list of unique findings
                       choices = sort(unique(child_data$Finding)), multiple = TRUE, selectize = TRUE),
           p('Choose conditions from click-list.
             Remove conditions by clicking on condition, and pressing "delete" or "backspace" key.')
    ),
    # Show a plot of the generated distribution
    column(4,
           plotlyOutput('plot1'),
           fluidRow(br(),br()),
           fluidRow(textOutput('fit1')),
           fluidRow(br(),br()),
           fluidRow(plotlyOutput('plot1_metric')),
           fluidRow(br(),br()),
           fluidRow(plotlyOutput('plot1_age'))
    ),
    column(4,
           plotlyOutput('plot2'),
           fluidRow(br(),br()),
           fluidRow(plotlyOutput('plot2_metric')),
           fluidRow(br(),br()),
           fluidRow(plotlyOutput('plot2_age'))
    ),
    column(2,
           h2('Right Chart'),
           selectInput('Gender2',
                       'Gender',
                       choices = c('Any','Female','Male')),
           h2('Measurement'),
           selectInput('Metric2',
                       'Metric',
                       choices = c('Height', 'Weight', 'Body Mass Index',
                                   'Height-z', 'Weight-z', 'Body Mass Index-z')),
           h2('Line fitting'),
           selectInput('Stat2',
                       'Regression',
                       choices = c('Loess', 'Linear Model')),
           h3('List of conditions'),
           selectInput('Include2',
                       'Include or Exclude',
                       choices = c('No condition filter', 'Include', 'Exclude')),
           selectInput('Conditions2',
                       'Conditions',
                       # sorted list of unique findings
                       choices = sort(unique(child_data$Finding)), multiple = TRUE, selectize = TRUE),
           p('Choose conditions from click-list.
             Remove conditions by clicking on condition, and pressing "delete" or "backspace" key.')
    )
  )
))
