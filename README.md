# SpurAfrika2018

Data processing and exploration done by SpurAfrika2018.Rmd

## Exploratory data application (Shiny App)

Two sets of plots 'left' and 'right' chart, each with independent filters for Gender and List of Conditions.

* Gender ('Any', 'Female', 'Male') chosen at the top of the left and right side-bars.
* Measured metric can be chosen ('Height', 'Weight', 'BMI (body mass index)' and respective z-scores.
* Loess and linear model line fitting can be chosen.
* List of conditions can be chosen 
    - Conditions can either be ignored ("No condition filter"), included ("Include") or excluded ("Exclude")
    - Multiple conditions can chosen
    - Once chosen, a condition can be removed from the list by clicking on the condition, and then pressing the 'delete' or 'backspace' key
    - COnditions can be copied from the left chart to the right chart, using the 'Copy Condition List' button on the top-right

Age restrictions can be set in the top-left corner

## Plots

* Scatter plot : information on data-points available on hover. Sub-sections can be viewed with click-and-drag.
* Vertical box-plot of metric
* Horizontal box-plot of ages
