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
data <- ChildFilteredFindingAnonymisedData

# need minimum maximum age for use in age sliders later
minimum_age <- round(min(data$Age.in.years)-1)
maximum_age <- round(max(data$Age.in.years)+1)

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
           h2('Left Charts'),
           selectInput('Gender1',
                       'Gender',
                       choices = c('Any','Female','Male')),
           h3('List of conditions'),
           selectInput('Include1',
                       'Include or Exclude',
                       choices = c('No condition filter','Include','Exclude'))
    ),
    # Show a plot of the generated distribution
    column(4,
           plotlyOutput('plot1'),
           textOutput('stature1_median'),
           textOutput('stature1_mean'),
           textOutput('stature1_sd'),
           textOutput('age1_median'),
           textOutput('age1_mean'),
           textOutput('age1_sd')
    ),
    column(4,
           plotlyOutput('plot2')
    ),
    column(2,
           h2('Right Charts'),
           selectInput('Gender2',
                       'Gender',
                       choices = c('Any','Female','Male'))
    )
  )
))
