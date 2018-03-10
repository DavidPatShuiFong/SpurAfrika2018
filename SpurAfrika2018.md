# SpurAfrika2018



# Spur Afrika January 2018 clinic data


```r
# start necessary libraries

library(airtabler)
# airtabler required to read airtable database
# airtabler is installed by 'devtools::install_github("bergant/airtabler")'

library(tidyverse)
library(lubridate)

library(knitr)

# plotting libraries
library(ggplot2)
library(plotly)
library(webshot)
```


```r
# read in data
ClinicData <- 
  airtable(
    base = 'appXwAbxa7r6bkKnO',
    tables = c('SpurAfrika2018','Schools','FindingCodes','ManagementCodes')
  )

RawPatientData <- as.tibble(ClinicData$SpurAfrika2018$select_all())
RawSchoolData <- as.tibble(ClinicData$Schools$select_all())
RawFindingCodes <- as.tibble(ClinicData$FindingCodes$select_all())
RawMangementCodes <- as.tibble(ClinicData$ManagementCodes$select_all())

# turn variable names into valid R names e.g. remove spaces
names(RawPatientData) <- make.names(names(RawPatientData))
```

## Adjust and re-calculate some data

Attach variable **SchoolName** to each student.
Variable **School.Name** contains the school's database ID

Recalculate **Age.in.years** with **Data.collection.date** and **Date.of.birth**


```r
# find actual ctual name (instead of the ID)
RawPatientData$SchoolName <- RawSchoolData$SchoolName[match(RawPatientData$School.Name,RawSchoolData$id)]

# re-calculate age in years
RawPatientData$Age.in.years <- as.numeric(as.Date(RawPatientData$Data.collection.date)-as.Date(RawPatientData$Date.of.birth))/365
```

## Missing data

Report missing date data (**Date.of.birth** and **Data.collection.date**) and subsequently
unable to calculate **Age.in.years**.

List patients with missing data.

Summarize patients with missing data by recorded **Form.Grade**.


```r
missing.dob <- sum(is.na(as.Date(RawPatientData$Date.of.birth)))
missing.dateofcollection <- sum(is.na(as.Date(RawPatientData$Data.collection.date)))
missing.Age <- sum(is.na(RawPatientData$Age.in.years))

sprintf('Patients with missing Date of Birth: %d', missing.dob)
```

```
## [1] "Patients with missing Date of Birth: 137"
```

```r
sprintf('Patients with missing date of data collection : %d', missing.dateofcollection)
```

```
## [1] "Patients with missing date of data collection : 0"
```

```r
sprintf('Patients unable to calculate age: %d', missing.Age)
```

```
## [1] "Patients unable to calculate age: 137"
```

```r
kable(RawPatientData[is.na(as.Date(RawPatientData$Date.of.birth)),c('Full.name','Date.of.birth','SchoolName','Form.Grade','Height')],
            caption = 'Missing Date of Birth')
```



Table: Missing Date of Birth

Full.name                         Date.of.birth   SchoolName                       Form.Grade    Height
--------------------------------  --------------  -------------------------------  -----------  -------
Metrine Khavukuyi                 NA              K.A.G Olympic Education Centre   Grade 4          130
Daniel Owino                      NA              The Global One Kibera School     Class 4          143
Bruno Omondi                      NA              The Global One Kibera School     Class 2          122
Stephen Oduor                     NA              The Global One Kibera School     Class 5          138
Macylian Moraa                    NA              The Global One Kibera School     Class 5          137
Christine Akoth                   NA              Fairview children centre         Class 5          144
Vivian Muhatia                    NA              Fairview children centre         Class 3          122
Fidel Otieno                      NA              The Global One Kibera School     Class 2          128
Isaac Omondi                      NA              The Global One Kibera School     Grade 6          158
Mayrel Atieno                     NA              The Global One Kibera School     Class 3          123
John Obong                        NA              K.A.G Olympic Education Centre   Grade 5          148
Stephanie Adhiambo                NA              Fairview children centre         Class 4          135
Edward Ouma Opole                 NA              Fairview children centre         Class 5          143
Peter Onyango                     NA              The Global One Kibera School     Class 2          135
Douglas Wanjala                   NA              The Global One Kibera School     Class 3          150
Wycliffe Otieno                   NA              Fairview children centre         Class 5          154
Michelle Achieng                  NA              K.A.G Olympic Education Centre   Grade 5          143
Francis Ochieng                   NA              The Global One Kibera School     Class 4          136
David Ochieng                     NA              K.A.G Olympic Education Centre   Grade 5          136
Sheryl Atieno                     NA              K.A.G Olympic Education Centre   Grade 4          139
David Wanabwa                     NA              K.A.G Olympic Education Centre   Grade 6          139
Salmer Eve                        NA              Fairview children centre         Class 5          137
Kevin                             NA              The Global One Kibera School     Class 3          121
Joy Munjaya                       NA              Fairview children centre         Class 3          129
Phanuel Otieno                    NA              The Global One Kibera School     Class 3          132
Grace Kasidi                      NA              The Global One Kibera School     Class 2          128
Abel Twabe                        NA              St. Juliet                       Class 4          133
Junior Omondi                     NA              The Global One Kibera School     Class 4          136
Morgan Onyango Nyagol             NA              St. Juliet                       Class 4          131
Isaac Nyongesa                    NA              Fairview children centre         Class 3          134
Celine Dion                       NA              K.A.G Olympic Education Centre   Grade 5          146
Yvonne Atieno                     NA              Fairview children centre         Class 2          127
Yvonne Akinyi                     NA              Fairview children centre         Class 4          146
Alex Owino                        NA              The Global One Kibera School     Class 3          133
Morgan Owili                      NA              The Global One Kibera School     Class 2          120
Michelle Achieng                  NA              The Global One Kibera School     Class 3          133
Simon Obuya                       NA              The Global One Kibera School     Class 4          146
Michelle Atieno                   NA              The Global One Kibera School     Class 5          130
Ronney Otieno                     NA              The Global One Kibera School     Grade 8          152
Herine Amondi                     NA              Fairview children centre         Class 5          140
Sharon Senge                      NA              St. Juliet                       Class 4          141
Bidal Adala                       NA              The Global One Kibera School     Class 2          122
Mildred Atieno                    NA              K.A.G Olympic Education Centre   Grade 5          145
Joseph Nabuko                     NA              St. Juliet                       Class 6          148
Victor Onyango                    NA              The Global One Kibera School     Class 5          145
Sella Akinyi Okuo                 NA              The Global One Kibera School     Class 4          153
Travis Akura                      NA              Fairview children centre         Class 3          133
Calvin Sakwa                      NA              Fairview children centre         Class 3          127
Sardinia Atieno                   NA              The Global One Kibera School     Class 2          118
Daniel Roy                        NA              St. Juliet                       Class 4          132
Eugene Odhiambo                   NA              Fairview children centre         Class 4          128
Edward Chweya                     NA              The Global One Kibera School     Class 4          144
Morgan Odhiambo                   NA              The Global One Kibera School     Class 2          135
Latifa Abdallah                   NA              K.A.G Olympic Education Centre   Grade 7          152
Fiona Omondi                      NA              The Global One Kibera School     Class 5          139
Moses Otieno                      NA              The Global One Kibera School     Class 4          138
Winnie Achieng                    NA              The Global One Kibera School     Class 5          145
Roselyn Nyambuki                  NA              NA                               NA                NA
Vidalis Adhiambo                  NA              St. Juliet                       Class 4          141
Stacy Adhiambo                    NA              K.A.G Olympic Education Centre   Grade 5          139
James Omondi                      NA              The Global One Kibera School     Class 2          124
Cridempta Achieng                 NA              Fairview children centre         Class 2           NA
Liz Michelle Oicho                NA              The Global One Kibera School     Class 3          127
Lensar Atieno                     NA              K.A.G Olympic Education Centre   Class 3          148
Fidelis Mandara                   NA              Fairview children centre         Class 5          148
Veronica Atieno                   NA              The Global One Kibera School     Class 2          127
Real Bwamula                      NA              St. Juliet                       Class 4          132
Millicent Moraa                   NA              Fairview children centre         Class 4          136
John Wesonga                      NA              The Global One Kibera School     Class 3          126
Elizabeth Acuuor                  NA              K.A.G Olympic Education Centre   Grade 5          133
Lidia Julius                      NA              The Global One Kibera School     Class 2          145
Moses Mutua                       NA              The Global One Kibera School     Class 4          149
Emmanuel Ochieng                  NA              Fairview children centre         Class 3          139
Arnold Otieno                     NA              The Global One Kibera School     Class 2          133
Bravin Khamasi Isayi              NA              Fairview children centre         Class 7          156
Jef Omondi                        NA              The Global One Kibera School     Class 3          123
Elvis Lutta                       NA              Fairview children centre         Class 7          153
Vivian Achieng                    NA              Fairview children centre         Class 5          144
Ruth Awino                        NA              The Global One Kibera School     Class 4          149
Stephany Wanjiru                  NA              K.A.G Olympic Education Centre   Grade 4          140
Thomas Juma                       NA              The Global One Kibera School     Class 3          140
Anastasia Achieng                 NA              The Global One Kibera School     Class 5          140
Briton Oduor                      NA              The Global One Kibera School     Class 2          126
Paulina Jerusa                    NA              St. Juliet                       Class 5          144
Teddy Jevis                       NA              K.A.G Olympic Education Centre   Grade 6          147
Shown Pall                        NA              The Global One Kibera School     Grade 8          134
Florine Atieno                    NA              St. Juliet                       Class 4          125
Newton Wesonga                    NA              Fairview children centre         Class 3          138
Gloria Akumu                      NA              The Global One Kibera School     Class 2          120
Faith Ominah                      NA              K.A.G Olympic Education Centre   Grade 4          146
Lensa Akinyi                      NA              Fairview children centre         Class 3          129
Ferdinand  Orina                  NA              Fairview children centre         Class 3          128
Lorraine Kagai                    NA              Fairview children centre         Class 5          140
Dilan Odhiambo                    NA              The Global One Kibera School     Class 3          132
Samuel Ochieng Otieno             NA              Fairview children centre         Class 3          129
Sheila Awino                      NA              The Global One Kibera School     Class 2          128
Allan Smith                       NA              The Global One Kibera School     Class 4          143
Drake Karerwa                     NA              Fairview children centre         Class 5          138
Divone Munisa                     NA              Fairview children centre         Class 5          142
James Msudhia                     NA              The Global One Kibera School     Class 3          130
Eunice Omori                      NA              The Global One Kibera School     Class 4          145
Mackinone Boy                     NA              The Global One Kibera School     Class 2          139
Marvellous Phyllis                NA              The Global One Kibera School     Class 4          134
Quincy Bridget (Queency Brijit)   NA              The Global One Kibera School     Class 2          124
Bonface Otieno                    NA              The Global One Kibera School     Class 3          120
Joy Achieng                       NA              The Global One Kibera School     Class 3          134
Fidel Castrol                     NA              K.A.G Olympic Education Centre   Grade 4          128
Bramwel Ochieng                   NA              The Global One Kibera School     Class 2          121
Daniel Kioko                      NA              The Global One Kibera School     Class 5          150
Peninah Achieng                   NA              St. Juliet                       Class 6          154
Ketwinslet Adhiambo               NA              The Global One Kibera School     Class 2          126
Yvonne Harvel                     NA              The Global One Kibera School     Class 3          131
Billy Clinton                     NA              Fairview children centre         Class 3          135
Millicent Atieno                  NA              The Global One Kibera School     Class 2          123
Collins Owino                     NA              The Global One Kibera School     Class 4          138
Poline Migosi                     NA              Fairview children centre         Class 3          147
Elizabeth Akinyi                  NA              Fairview children centre         Class 4          133
Mishel Sarah                      NA              Fairview children centre         Class 3          139
Maryline Akinyi                   NA              The Global One Kibera School     Class 3          134
Vanecious Iminza                  NA              Fairview children centre         Class 4          126
Cynthia Akinyi                    NA              The Global One Kibera School     Class 3          126
Lexy Achieng                      NA              K.A.G Olympic Education Centre   Grade 2          110
Margaret Miteka                   NA              St. Juliet                       Class 4          129
Vona Aoko                         NA              The Global One Kibera School     Class 3          129
Brian Siriga                      NA              The Global One Kibera School     Class 3          162
Redemta8                          NA              The Global One Kibera School     Class 2          118
Frank Nevel Otieno                NA              The Global One Kibera School     Class 4          129
Esther Apiyo                      NA              The Global One Kibera School     Class 4          153
Maras Raya                        NA              The Global One Kibera School     Class 3          129
Florens Akoth                     NA              The Global One Kibera School     Class 2          123
Erick Otieno Onyango              NA              The Global One Kibera School     Class 3          131
Henry Otieno                      NA              The Global One Kibera School     Class 3          145
Claris Akinyi                     NA              The Global One Kibera School     Class 4          136
Evans Ouma                        NA              The Global One Kibera School     Class 3          128
Mary Moraa                        NA              Fairview children centre         Class 4          129
Fidel Otieno Ochieng              NA              The Global One Kibera School     Class 4          138
Dan ONyango                       NA              The Global One Kibera School     Class 4          126

```r
RawPatientData[is.na(as.Date(RawPatientData$Date.of.birth)),
               c('Full.name','Date.of.birth','SchoolName','Form.Grade','Height')] %>% 
  group_by(Form.Grade) %>% 
  summarize(n=n()) %>%
  kable
```



Form.Grade     n
-----------  ---
Class 2       24
Class 3       37
Class 4       33
Class 5       19
Class 6        2
Class 7        2
Grade 2        1
Grade 4        5
Grade 5        7
Grade 6        2
Grade 6        1
Grade 7        1
Grade 8        2
NA             1

## Average age and **Form.Grade**

Summarize mean age of patients by **Form.Grade**, for patients who have calculated **Age.in.years**


```r
RawPatientData[!is.na(as.Date(RawPatientData$Date.of.birth)),
               c('Full.name','Date.of.birth','Age.in.years','SchoolName','Form.Grade','Height')] %>%
  group_by(Form.Grade) %>%
  summarize(n=n(), MeanAge = mean(Age.in.years, na.rm = TRUE)) %>%
  kable
```



Form.Grade           n      MeanAge
-----------------  ---  -----------
13                   1   14.0383562
Baby                 1    4.5123288
Class 1              1    7.1397260
Class 2             36    7.3119482
Class 2              2    9.0315068
Class 3             63    8.9111981
Class 4             99    9.2323232
Class 4              1   11.0328767
Class 5             60   11.4869406
Class 6             77   12.5339619
Class 7             74   13.3061829
Class 8             55   14.5098879
Class 8              1   13.1369863
Form 1               5   12.2482192
Form 2               3   16.1479452
Form 3               3   17.2986301
Form 4               2   18.3123288
Form 5               1   11.0191781
Grade 1              2    6.3123288
Grade 2             26    7.5034773
Grade 3             26    8.4700738
Grade 4             31   10.4494034
Grade 4              1    9.6657534
Grade 5             32   11.3953767
Grade 5              2   10.4301370
Grade 5 20080120     1   -0.6109589
Grade 6             58   12.0330657
Grade 7             61   12.5492028
Grade 8             34   14.0472200
Teacher              3   25.2009132
University           1   20.4136986
NA                   4   18.5095890


## Dubious data


```r
q <- RawPatientData %>%
  filter(as.integer(Age.in.years) <= 0) %>%
  select(c('Full.name','Date.of.birth','Age.in.years','Gender','Data.collection.date','Weight','Height'))

kable(q, caption = 'Dubious dates and ages')
```



Table: Dubious dates and ages

Full.name                 Date.of.birth    Age.in.years  Gender   Data.collection.date    Weight   Height
------------------------  --------------  -------------  -------  ---------------------  -------  -------
Morgan Okello             2018-02-01          0.0273973  Male     2018-02-11                  26      137
Catherine Gesare          2026-06-20         -8.4602740  Female   2018-01-05                  41      152
Brian Mwanza              2003-01-01          0.0000000  Male     2003-01-01                  50      168
Beatrice Wanza Alzilani   2018-01-11          0.0000000  Female   2018-01-11                  20      135
Shirley Nelima            2010-01-01          0.0000000  Female   2010-01-01                  21      123
Lenny Ouma                2010-01-01          0.0000000  Male     2010-01-01                  30      138
Irene Muonja              2018-01-01          0.0273973  Female   2018-01-11                  29      140
Verah Zenah               2012-01-01          0.0219178  Female   2012-01-09                  18      116
Alice Nina                2018-01-11        -12.1671233  Female   2005-11-14                  29      145
Shawn Indeku              2018-01-01          0.0273973  Male     2018-01-11                  30      137
Bridget Ambuso            2023-02-20         -5.1287671  Female   2018-01-05                  48      155
Sheilah Atieno            2018-01-10          0.0000000  Female   2018-01-10                  26      132
Marion Amour              2018-01-01          0.0273973  Female   2018-01-11                  25      142
Lancer Achieng            2018-02-01          0.0273973  Female   2018-02-11                  NA       NA
Monte Bibiana             2018-01-01          0.0246575  Female   2018-01-10                  21      136
Isaac Musita              2018-09-16         -0.6958904  Male     2018-01-05                  44      154
Ada Mbone                 2018-01-01          0.0273973  Female   2018-01-11                  22      125
Mary Mwikali              2018-01-01          0.0273973  Female   2018-01-11                  25      130
Michael Oleyi             2018-01-01          0.0273973  Male     2018-01-11                  28      128
Sherline Mukoya           2008-08-30         -0.6109589  Female   2008-01-20                  29       NA

```r
# remove dubious data

PatientData <- RawPatientData %>%
  filter(as.integer(Age.in.years) > 0)
```


```r
# create summary of patients by age and gender (only those aged 18 or below)
p <- PatientData %>%
  filter(as.integer(Age.in.years)<=18) %>%
  mutate(Age = as.integer(Age.in.years)) %>%
  group_by(Age, Gender) %>%
  summarize(n = n())

TotalPatients <- sum(p$n)
MedianAge <- median(as.numeric(PatientData$Age.in.years[PatientData$Age.in.years<=18]))
MeanAge <- mean(as.numeric(PatientData$Age.in.years[PatientData$Age.in.years<=18]))

# ggplot version of plot
gg <- p %>%
  mutate(PlotCount = n*ifelse(Gender == 'Female',1,-1)) %>%
  ggplot(aes(x = Age, y = PlotCount, fill = Gender)) +
  geom_bar(stat = 'identity') + 
  labs(title = "Number of patients in each age group") +
  labs(x = "Age", y = "Number") +
  scale_y_discrete(labels=abs) + 
                     ### remove negative values for axis for 'negative' part of chart
  guides(fill = guide_legend(title="")) +
  theme(legend.position=c(0.8,0.5)) +
  coord_flip()

# plotly version of plot
gp <- p %>%
  spread(Gender, n) %>% # divide male and female into two different variables
  mutate(MalePlot = -Male) %>% # negative male count for 'left'-side of plot
  plot_ly(x = ~Female, y = ~Age, name = 'Female',
          type = 'bar', orientation = 'h',
          hoverinfo = 'text', 
          text = ~paste('Age: ',Age,'years<br>Number :',Female,'<br>Female')) %>%
  add_trace (x = ~MalePlot, name = 'Male',
             hoverinfo = 'text', 
             text = ~paste('Age: ',Age,'years<br>Number :',Male,'<br>Male')) %>%
  layout (xaxis = list(title='Number of children in each age group', tickmode = 'array',
                       tickvals = c(-60, -40, -20, 0, 20, 40, 60),
                       # for 'negative' number, change to absolute value
                       ticktext = c('60', '40', '20', '0', '20', '40', '60')),
          yaxis = list(title='Age'),
          barmode = 'overlay',
          title = 'Spur Afrika 2018 clinics - age of children seen',
          annotations = list(list(x = 40, y = 3, showarrow = F,
                                 text = ~paste('Total children', TotalPatients,
                                               '<br>Median age: ', sprintf('%.1f',MedianAge),
                                               '<br>Mean age: ', sprintf('%.1f',MeanAge)))))

gp
```

```
## Warning: Ignoring 2 observations

## Warning: Ignoring 2 observations
```

<!--html_preserve--><div id="34a0532a389a" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="34a0532a389a">{"x":{"visdat":{"34a07e5155a":["function () ","plotlyVisDat"]},"cur_data":"34a07e5155a","attrs":{"34a07e5155a":{"x":{},"y":{},"name":"Female","orientation":"h","hoverinfo":"text","text":{},"alpha":1,"sizes":[10,100],"type":"bar"},"34a07e5155a.1":{"x":{},"y":{},"name":"Male","orientation":"h","hoverinfo":"text","text":{},"alpha":1,"sizes":[10,100],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"title":"Number of children in each age group","tickmode":"array","tickvals":[-60,-40,-20,0,20,40,60],"ticktext":["60","40","20","0","20","40","60"]},"yaxis":{"domain":[0,1],"title":"Age"},"barmode":"overlay","title":"Spur Afrika 2018 clinics - age of children seen","annotations":[{"x":40,"y":3,"showarrow":false,"text":"Total children 741 <br>Median age:  12.0 <br>Mean age:  11.6"}],"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"data":[{"x":[1,1,1,7,20,37,28,60,52,58,63,46,15,7,2],"y":[1,3,5,6,7,8,9,10,11,12,13,14,15,16,17],"name":"Female","orientation":"h","hoverinfo":["text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"],"text":["Age:  1 years<br>Number : 1 <br>Female","Age:  3 years<br>Number : 1 <br>Female","Age:  5 years<br>Number : 1 <br>Female","Age:  6 years<br>Number : 7 <br>Female","Age:  7 years<br>Number : 20 <br>Female","Age:  8 years<br>Number : 37 <br>Female","Age:  9 years<br>Number : 28 <br>Female","Age:  10 years<br>Number : 60 <br>Female","Age:  11 years<br>Number : 52 <br>Female","Age:  12 years<br>Number : 58 <br>Female","Age:  13 years<br>Number : 63 <br>Female","Age:  14 years<br>Number : 46 <br>Female","Age:  15 years<br>Number : 15 <br>Female","Age:  16 years<br>Number : 7 <br>Female","Age:  17 years<br>Number : 2 <br>Female"],"type":"bar","marker":{"fillcolor":"rgba(31,119,180,1)","color":"rgba(31,119,180,1)","line":{"color":"transparent"}},"xaxis":"x","yaxis":"y","frame":null},{"x":[-1,-1,-4,-11,-23,-33,-38,-37,-58,-53,-47,-19,-12,-3,-3],"y":[4,5,6,7,8,9,10,11,12,13,14,15,16,17,18],"name":"Male","orientation":"h","hoverinfo":["text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"],"text":["Age:  4 years<br>Number : 1 <br>Male","Age:  5 years<br>Number : 1 <br>Male","Age:  6 years<br>Number : 4 <br>Male","Age:  7 years<br>Number : 11 <br>Male","Age:  8 years<br>Number : 23 <br>Male","Age:  9 years<br>Number : 33 <br>Male","Age:  10 years<br>Number : 38 <br>Male","Age:  11 years<br>Number : 37 <br>Male","Age:  12 years<br>Number : 58 <br>Male","Age:  13 years<br>Number : 53 <br>Male","Age:  14 years<br>Number : 47 <br>Male","Age:  15 years<br>Number : 19 <br>Male","Age:  16 years<br>Number : 12 <br>Male","Age:  17 years<br>Number : 3 <br>Male","Age:  18 years<br>Number : 3 <br>Male"],"type":"bar","marker":{"fillcolor":"rgba(255,127,14,1)","color":"rgba(255,127,14,1)","line":{"color":"transparent"}},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1}},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":{"render":[{"code":"function(el, x) { var ctConfig = crosstalk.var('plotlyCrosstalkOpts').set({\"on\":\"plotly_click\",\"persistent\":false,\"dynamic\":false,\"selectize\":false,\"opacityDim\":0.2,\"selected\":{\"opacity\":1}}); }","data":null}]}}</script><!--/html_preserve-->
