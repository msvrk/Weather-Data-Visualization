# Required libraries included



library(shinycssloaders)
library(hash)
library(stringr)
library(grid)
library(shinyalert)
library(RColorBrewer)
library(sf)
library(tigris)
library(shinyWidgets)
library(shiny)
library(shinyjs)
library(shinythemes)
library(ggthemes)
library(maps)
library(ggplot2)
library(dplyr)
library(ozmaps)
library(leaflet)
library(readr)
library(plotly)
library(htmltools)
library(ggiraph)
library(shinyglide)
library(hrbrthemes)
library(forcats)
library(quantmod)
library(zoo)
library(lubridate)




# Clearing the current working environment variables
rm(list=ls())

# Setting option for loading symbol
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

# Loading the datasets
data <- read_csv("data.csv")
r_2007_df <- read_csv("2007_spatial.csv",col_types = cols(Location = col_character()))
r_2008_df <- read_csv("2008_spatial.csv",col_types = cols(Location = col_character()))
r_2009_df <- read_csv("2009_spatial.csv",col_types = cols(Location = col_character()))
r_2010_df <- read_csv("2010_spatial.csv",col_types = cols(Location = col_character()))
r_2011_df <- read_csv("2011_spatial.csv",col_types = cols(Location = col_character()))
r_2012_df <- read_csv("2012_spatial.csv",col_types = cols(Location = col_character()))
r_2013_df <- read_csv("2013_spatial.csv",col_types = cols(Location = col_character()))
r_2014_df <- read_csv("2014_spatial.csv",col_types = cols(Location = col_character()))
r_2015_df <- read_csv("2015_spatial.csv",col_types = cols(Location = col_character()))
r_2016_df <- read_csv("2016_spatial.csv",col_types = cols(Location = col_character()))
r_2017_df <- read_csv("2017_spatial.csv",col_types = cols(Location = col_character()))


# Using Hash Maps for faster access
overall <- hash()
overall[["2007"]] <- r_2007_df
overall[["2008"]] <- r_2008_df
overall[["2009"]] <- r_2009_df
overall[["2010"]] <- r_2010_df
overall[["2011"]] <- r_2011_df
overall[["2012"]] <- r_2012_df
overall[["2013"]] <- r_2013_df
overall[["2014"]] <- r_2014_df
overall[["2015"]] <- r_2015_df
overall[["2016"]] <- r_2016_df
overall[["2017"]] <- r_2017_df

year = ""
for(year in as.character(c(2007:2017))){
  state = ""
  for(state in zoo::na.trim(unique(overall[[year]]$State))){
    overall[[paste0(state,"_total")]] <- 0
    month = ""
    for(month in month.name){
      overall[[paste0(state,"_",month,"_total")]] <- 0
    }
    
    season = ""
    for(season in c("Summer","Autumn","Winter","Spring")){
      overall[[paste0(state,"_",season,"_total")]] <- 0
    }
    
    
  }
}

year = ""
for(year in as.character(c(2007:2017))){
  state = ""
  for(state in zoo::na.trim(unique(overall[[year]]$State))){
    overall[[paste0(state,"_total")]] <- overall[[paste0(state,"_total")]] + overall[[year]] %>% dplyr::filter(State == state) %>% dplyr::select(Total) %>% sum()
    month = ""
    for(month in month.name){
      overall[[paste0(state,"_",month,"_total")]] <- overall[[paste0(state,"_",month,"_total")]] + overall[[year]] %>% dplyr::filter(State == state) %>% dplyr::select(.data[[month]]) %>% sum()
    }
    
    season = ""
    for(season in c("Summer","Autumn","Winter","Spring")){
      overall[[paste0(state,"_",season,"_total")]] <- overall[[paste0(state,"_",season,"_total")]] + overall[[year]] %>% dplyr::filter(State == state) %>% dplyr::select(.data[[season]]) %>% sum()
    }
    
  }
}



year = ""
overall[["Total"]] <- 0
for(year in as.character(c(2007:2017))){
  overall[["Total"]] = overall[["Total"]] + sum(overall[[year]]$Total)
}

year = ""
overall[["Spring_total"]] <- 0
overall[["Summer_total"]] <- 0
overall[["Autumn_total"]] <- 0
overall[["Winter_total"]] <- 0
for(year in as.character(c(2007:2017))){
  overall[["Spring_total"]] <- overall[["Spring_total"]] + sum(overall[[year]]$Spring)
  overall[["Summer_total"]] <- overall[["Summer_total"]] + sum(overall[[year]]$Summer)
  overall[["Autumn_total"]] <- overall[["Autumn_total"]] + sum(overall[[year]]$Autumn)
  overall[["Winter_total"]] <- overall[["Winter_total"]] + sum(overall[[year]]$Winter)   
}

month = ""
for(month in month.name){
  overall[[paste0(month,"_total")]] <- 0
}


year = ""
for(year in as.character(c(2007:2017))){
  month = ""
  for(month in month.name){
    overall[[paste0(month,"_total")]] <- overall[[paste0(month,"_total")]] + overall[[year]][[month]] %>% sum()
  }
  
}

# Setting the carousel controls
controls <- fluidRow(
  column(width = 2,  class="col-xs-6 text-right",
      prevButton(class = "btn btn-primary")
  ),
  column(width = 8),
  column(width = 2, class="col-xs-6 text-left",
      nextButton(class = "btn btn-primary")
  )
)

# User Interface
# Using HTML Template to seperate static UI away from the shiny app
ui <- shiny::htmlTemplate("www/index.html", 
                 slider_style = chooseSliderSkin("Flat", color = "#112446"),
                 shinyalert = useShinyalert(), # Shiny Alert Modal dialog
                 
        carousel1 =  shinyglide::glide(
          custom_controls = controls,
          height = "1100px",
          controls_position = "bottom",
          next_label = "Next",
          previous_label = "Previous",
          screen(
           
            withSpinner(girafeOutput(outputId = "ggiraph_contour_whole" , width = "100%", height = "675px"),type = 3)

            
          ),
          screen(
          fluidRow(column(width=1, actionButton("show1", "HOW TO INTERACT",class = "btn-warning") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 24px; font-weight: bold; box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19);')),
                   column(width=2),
                   column(width=2),
                   column(width=3,wellPanel(prettyRadioButtons(inputId = "month_or_year", "Timeline", choices = c("Yearly", "Monthly"),selected = "Yearly",animation = "tada",outline = TRUE))),
                   column(width = 4)),
            conditionalPanel(condition = "input.month_or_year == 'Yearly'",
          fluidRow(
            column(width=1),
            column(width=10,wellPanel(
              sliderInput(inputId = "Year_1a", 
                                  label = "Year",
                                  min = 2007,
                                  max = 2017,
                                  value = 2007,
                                  step = 1,
                                  sep = "",
                                  animate = animationOptions(interval = 1800,playButton = "PLAY",pauseButton = "PAUSE")))
              ),
            column(width=1)
            ),
          girafeOutput("ggiraph_contour" , width = "100%", height = "675px"),
          textOutput(outputId = "text_carousel1_2") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 20px')),
          
          conditionalPanel(condition = "input.month_or_year == 'Monthly'",
          fluidRow(
            column(width=1),
            column(width=10,wellPanel(
              sliderTextInput(inputId = "Month_Year_1a", 
                              label = "Month-Year",
                              choices = c(as.yearmon("2007-01-01"),as.yearmon("2007-02-01"),as.yearmon("2007-03-01"),as.yearmon("2007-04-01"),as.yearmon("2007-05-01"),as.yearmon("2007-06-01"),as.yearmon("2007-07-01"),as.yearmon("2007-08-01"),as.yearmon("2007-09-01"),as.yearmon("2007-10-01"),as.yearmon("2007-11-01"),as.yearmon("2007-12-01"),as.yearmon("2008-01-01"),as.yearmon("2008-02-01"),as.yearmon("2008-03-01"),as.yearmon("2008-04-01"),as.yearmon("2008-05-01"),as.yearmon("2008-06-01"),as.yearmon("2008-07-01"),as.yearmon("2008-08-01"),as.yearmon("2008-09-01"),as.yearmon("2008-10-01"),as.yearmon("2008-11-01"),as.yearmon("2008-12-01"),as.yearmon("2009-01-01"),as.yearmon("2009-02-01"),as.yearmon("2009-03-01"),as.yearmon("2009-04-01"),as.yearmon("2009-05-01"),as.yearmon("2009-06-01"),as.yearmon("2009-07-01"),as.yearmon("2009-08-01"),as.yearmon("2009-09-01"),as.yearmon("2009-10-01"),as.yearmon("2009-11-01"),as.yearmon("2009-12-01"),as.yearmon("2010-01-01"),as.yearmon("2010-02-01"),as.yearmon("2010-03-01"),as.yearmon("2010-04-01"),as.yearmon("2010-05-01"),as.yearmon("2010-06-01"),as.yearmon("2010-07-01"),as.yearmon("2010-08-01"),as.yearmon("2010-09-01"),as.yearmon("2010-10-01"),as.yearmon("2010-11-01"),as.yearmon("2010-12-01"),as.yearmon("2011-01-01"),as.yearmon("2011-02-01"),as.yearmon("2011-03-01"),as.yearmon("2011-04-01"),as.yearmon("2011-05-01"),as.yearmon("2011-06-01"),as.yearmon("2011-07-01"),as.yearmon("2011-08-01"),as.yearmon("2011-09-01"),as.yearmon("2011-10-01"),as.yearmon("2011-11-01"),as.yearmon("2011-12-01"),as.yearmon("2012-01-01"),as.yearmon("2012-02-01"),as.yearmon("2012-03-01"),as.yearmon("2012-04-01"),as.yearmon("2012-05-01"),as.yearmon("2012-06-01"),as.yearmon("2012-07-01"),as.yearmon("2012-08-01"),as.yearmon("2012-09-01"),as.yearmon("2012-10-01"),as.yearmon("2012-11-01"),as.yearmon("2012-12-01"),as.yearmon("2013-01-01"),as.yearmon("2013-02-01"),as.yearmon("2013-03-01"),as.yearmon("2013-04-01"),as.yearmon("2013-05-01"),as.yearmon("2013-06-01"),as.yearmon("2013-07-01"),as.yearmon("2013-08-01"),as.yearmon("2013-09-01"),as.yearmon("2013-10-01"),as.yearmon("2013-11-01"),as.yearmon("2013-12-01"),as.yearmon("2014-01-01"),as.yearmon("2014-02-01"),as.yearmon("2014-03-01"),as.yearmon("2014-04-01"),as.yearmon("2014-05-01"),as.yearmon("2014-06-01"),as.yearmon("2014-07-01"),as.yearmon("2014-08-01"),as.yearmon("2014-09-01"),as.yearmon("2014-10-01"),as.yearmon("2014-11-01"),as.yearmon("2014-12-01"),as.yearmon("2015-01-01"),as.yearmon("2015-02-01"),as.yearmon("2015-03-01"),as.yearmon("2015-04-01"),as.yearmon("2015-05-01"),as.yearmon("2015-06-01"),as.yearmon("2015-07-01"),as.yearmon("2015-08-01"),as.yearmon("2015-09-01"),as.yearmon("2015-10-01"),as.yearmon("2015-11-01"),as.yearmon("2015-12-01"),as.yearmon("2016-01-01"),as.yearmon("2016-02-01"),as.yearmon("2016-03-01"),as.yearmon("2016-04-01"),as.yearmon("2016-05-01"),as.yearmon("2016-06-01"),as.yearmon("2016-07-01"),as.yearmon("2016-08-01"),as.yearmon("2016-09-01"),as.yearmon("2016-10-01"),as.yearmon("2016-11-01"),as.yearmon("2016-12-01"),as.yearmon("2017-01-01"),as.yearmon("2017-02-01"),as.yearmon("2017-03-01"),as.yearmon("2017-04-01"),as.yearmon("2017-05-01"),as.yearmon("2017-06-01"),as.yearmon("2017-07-01"),as.yearmon("2017-08-01"),as.yearmon("2017-09-01"),as.yearmon("2017-10-01"),as.yearmon("2017-11-01"),as.yearmon("2017-12-01")),
                              selected  = "Jan 2007",
                              grid = TRUE,
                              animate = animationOptions(interval = 1800,playButton = "PLAY",pauseButton = "PAUSE")))
            ),
            column(width=1)
          ),
          girafeOutput("ggiraph_contour_monthly", width = "100%", height = "675px"),
          textOutput(outputId = "text_carousel1_3") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 20px; padding-top: 1in;'))

          
          ),
          screen(
            useShinyalert(),
            fluidRow(column(width=1, actionButton("show2", "HOW TO INTERACT",class = "btn-warning") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 24px; font-weight: bold; box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19);')),
                     column(width=2),
                     column(width=2),
                     column(width=3,wellPanel(prettyRadioButtons(inputId = "month_or_year2", "Timeline", choices = c("Yearly", "Monthly"),selected = "Yearly",animation = "tada",outline = TRUE))),
                     column(width = 4)),
            conditionalPanel(condition = "input.month_or_year2 == 'Yearly'",
                             fluidRow(
                               column(width=1),
                               column(width=10,wellPanel(
                                 sliderInput(inputId = "Year_1b", 
                                             label = "Year:",
                                             min = 2007,
                                             max = 2017,
                                             value = 2007,
                                             sep = "",
                                             step = 1,
                                             animate = animationOptions(interval = 1800,playButton = "PLAY",pauseButton = "PAUSE"))) %>% tagAppendAttributes(style= 'padding-top: 0.5in;')
                               ),
                               column(width=1)
                             ),
                             fluidRow(
                               column(width=5,leafletOutput(outputId = "choropleth")),
                               column(width=7,plotlyOutput("locationbar"))
                               
                             ),
                             fluidRow(
                               column(width=5,textOutput(outputId = "text_carousel1_4") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 20px; padding-top: 1in;')),
                               column(width=7,textOutput(outputId = "text_carousel1_5") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 20px; padding-top: 1in;'))
                               
                             )),
            
            conditionalPanel(condition = "input.month_or_year2 == 'Monthly'",
                             fluidRow(
                               column(width=1),
                               column(width=10,wellPanel(
                                 sliderTextInput(inputId = "Month_Year_1b", 
                                                 label = "Month-Year",
                                                 choices = c(as.yearmon("2007-01-01"),as.yearmon("2007-02-01"),as.yearmon("2007-03-01"),as.yearmon("2007-04-01"),as.yearmon("2007-05-01"),as.yearmon("2007-06-01"),as.yearmon("2007-07-01"),as.yearmon("2007-08-01"),as.yearmon("2007-09-01"),as.yearmon("2007-10-01"),as.yearmon("2007-11-01"),as.yearmon("2007-12-01"),as.yearmon("2008-01-01"),as.yearmon("2008-02-01"),as.yearmon("2008-03-01"),as.yearmon("2008-04-01"),as.yearmon("2008-05-01"),as.yearmon("2008-06-01"),as.yearmon("2008-07-01"),as.yearmon("2008-08-01"),as.yearmon("2008-09-01"),as.yearmon("2008-10-01"),as.yearmon("2008-11-01"),as.yearmon("2008-12-01"),as.yearmon("2009-01-01"),as.yearmon("2009-02-01"),as.yearmon("2009-03-01"),as.yearmon("2009-04-01"),as.yearmon("2009-05-01"),as.yearmon("2009-06-01"),as.yearmon("2009-07-01"),as.yearmon("2009-08-01"),as.yearmon("2009-09-01"),as.yearmon("2009-10-01"),as.yearmon("2009-11-01"),as.yearmon("2009-12-01"),as.yearmon("2010-01-01"),as.yearmon("2010-02-01"),as.yearmon("2010-03-01"),as.yearmon("2010-04-01"),as.yearmon("2010-05-01"),as.yearmon("2010-06-01"),as.yearmon("2010-07-01"),as.yearmon("2010-08-01"),as.yearmon("2010-09-01"),as.yearmon("2010-10-01"),as.yearmon("2010-11-01"),as.yearmon("2010-12-01"),as.yearmon("2011-01-01"),as.yearmon("2011-02-01"),as.yearmon("2011-03-01"),as.yearmon("2011-04-01"),as.yearmon("2011-05-01"),as.yearmon("2011-06-01"),as.yearmon("2011-07-01"),as.yearmon("2011-08-01"),as.yearmon("2011-09-01"),as.yearmon("2011-10-01"),as.yearmon("2011-11-01"),as.yearmon("2011-12-01"),as.yearmon("2012-01-01"),as.yearmon("2012-02-01"),as.yearmon("2012-03-01"),as.yearmon("2012-04-01"),as.yearmon("2012-05-01"),as.yearmon("2012-06-01"),as.yearmon("2012-07-01"),as.yearmon("2012-08-01"),as.yearmon("2012-09-01"),as.yearmon("2012-10-01"),as.yearmon("2012-11-01"),as.yearmon("2012-12-01"),as.yearmon("2013-01-01"),as.yearmon("2013-02-01"),as.yearmon("2013-03-01"),as.yearmon("2013-04-01"),as.yearmon("2013-05-01"),as.yearmon("2013-06-01"),as.yearmon("2013-07-01"),as.yearmon("2013-08-01"),as.yearmon("2013-09-01"),as.yearmon("2013-10-01"),as.yearmon("2013-11-01"),as.yearmon("2013-12-01"),as.yearmon("2014-01-01"),as.yearmon("2014-02-01"),as.yearmon("2014-03-01"),as.yearmon("2014-04-01"),as.yearmon("2014-05-01"),as.yearmon("2014-06-01"),as.yearmon("2014-07-01"),as.yearmon("2014-08-01"),as.yearmon("2014-09-01"),as.yearmon("2014-10-01"),as.yearmon("2014-11-01"),as.yearmon("2014-12-01"),as.yearmon("2015-01-01"),as.yearmon("2015-02-01"),as.yearmon("2015-03-01"),as.yearmon("2015-04-01"),as.yearmon("2015-05-01"),as.yearmon("2015-06-01"),as.yearmon("2015-07-01"),as.yearmon("2015-08-01"),as.yearmon("2015-09-01"),as.yearmon("2015-10-01"),as.yearmon("2015-11-01"),as.yearmon("2015-12-01"),as.yearmon("2016-01-01"),as.yearmon("2016-02-01"),as.yearmon("2016-03-01"),as.yearmon("2016-04-01"),as.yearmon("2016-05-01"),as.yearmon("2016-06-01"),as.yearmon("2016-07-01"),as.yearmon("2016-08-01"),as.yearmon("2016-09-01"),as.yearmon("2016-10-01"),as.yearmon("2016-11-01"),as.yearmon("2016-12-01"),as.yearmon("2017-01-01"),as.yearmon("2017-02-01"),as.yearmon("2017-03-01"),as.yearmon("2017-04-01"),as.yearmon("2017-05-01"),as.yearmon("2017-06-01"),as.yearmon("2017-07-01"),as.yearmon("2017-08-01"),as.yearmon("2017-09-01"),as.yearmon("2017-10-01"),as.yearmon("2017-11-01"),as.yearmon("2017-12-01")),
                                                 selected  = "Jan 2007",
                                                 grid = TRUE,
                                                 animate = animationOptions(interval = 1800,playButton = "PLAY",pauseButton = "PAUSE")))%>% tagAppendAttributes(style= 'padding-top: 0.5in;')
                               ),
                               column(width=1)
                             ),
                             fluidRow(
                               column(width=5,leafletOutput(outputId = "choropleth_monthly")),
                               # column(width=6,leafletOutput("symbol_map")),
                               column(width=7,plotlyOutput("locationbar_monthly"))
                               
                             ),
                             fluidRow(
                               column(width=5,textOutput(outputId = "text_carousel1_6") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 20px; padding-top: 1in;')),
                               column(width=7,textOutput(outputId = "text_carousel1_7") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 20px; padding-top: 1in;'))
                               
                             ))
            
            
          )
        ),
        
        
        carousel2 =  shinyglide::glide(
          custom_controls = controls,
          height = "1000px",
          controls_position = "bottom",
          next_label = "Next",
          previous_label = "Previous",
          screen(
            
            fluidRow(column(width=6,withSpinner(plotlyOutput(outputId = "season_whole",inline = TRUE),type = 3,color.background = "#c8ebff")),
            column(width=6,withSpinner(plotlyOutput(outputId = "monthly_total",inline = TRUE),type = 3,color.background = "#c8ebff"))),
            fluidRow(
              column(width=6, textOutput(outputId = "text_carousel2a") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 20px; padding-top: 2in;')),
              column(width=6,textOutput(outputId = "text_carousel2b") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 20px; padding-top: 2in;'))
            )
          ),
          screen(
            fluidRow(
              column(width=1, actionButton("show3", "HOW TO INTERACT",class = "btn-warning") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 24px; font-weight: bold; box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19);')),
              column(width=1),
              column(width=7,wellPanel(
                sliderInput(inputId = "Year_2", 
                            label = "Year:",
                            min = 2007,
                            max = 2017,
                            value = 2007,
                            sep = "",
                            step = 1,
                            animate = animationOptions(interval = 1800,playButton = "PLAY",pauseButton = "PAUSE")))
              ),
              column(width=2, prettyRadioButtons(inputId = "State1", label = "State", choices = append(as.vector(unique(r_2007_df$State)[2:9]),"All States",0),selected = "All States",animation = "tada",outline = TRUE)),
              column(width = 1)

            ),
            
            
            fluidRow(column(width=6,plotlyOutput(outputId = "season",inline = TRUE)),
                     column(width=6,plotlyOutput(outputId = "monthly",inline = TRUE))),
            
            fluidRow(column(width=6,textOutput(outputId = "text_carousel2_1") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 20px; padding-top: 2in;')),
                     column(width=6,textOutput(outputId = "text_carousel2_2")) %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 20px; padding-top: 2in;'))
          )
        ),
        
        carousel3 =  shinyglide::glide(
          height = "1300px",
          custom_controls = controls,
          controls_position = "bottom",
          next_label = "Next",
          previous_label = "Previous",
          screen(
            fluidRow( 
                      column(width=4,withSpinner(plotlyOutput(outputId = "factor_whole",inline = TRUE),type = 3)),
                     column(width=4,withSpinner(plotlyOutput(outputId = "density_whole",inline = TRUE),type = 3)),
                    column(width=4,
                           fluidRow(
                                      column(width=12,textOutput(outputId = "text_carousel3a") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 19px;padding-top: 1in;padding-left: 0.75in;')),
                             ),
                           fluidRow(
                             
                             column(width=12,textOutput(outputId = "text_carousel3b") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 19px;padding-top: 0.25in;padding-left: 0.75in;'))
                             
                           ),
                           fluidRow(
                             
                             
                             column(width=12,textOutput(outputId = "text_carousel3c") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 19px;padding-top: 0.25in;padding-left: 0.75in;'))
                           )
                     )
            ),
            
            fluidRow(
              column(width = 4,textOutput(outputId = "text_carousel3_factor_whole_info") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 19px;padding-top: 1.5in')),
              column(width = 4,textOutput(outputId = "text_carousel3_density_whole_info") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 19px;padding-top: 1.5in')),
              column(width=4)
            )
            
          ),
          screen( useShinyalert(),
            fluidRow(column(width=1, actionButton("show4", "HOW TO INTERACT",class = "btn-warning") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 24px; font-weight: bold; box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19);')),
                     column(width=1),
                     column(width=3,prettyRadioButtons(inputId = "month_or_year3", "Timeline", choices = c("Yearly", "Monthly"),selected = "Yearly",animation = "tada",outline = TRUE)),
                     column(width=4, 
                            conditionalPanel(condition = "input.month_or_year3 == 'Yearly'",prettyRadioButtons(inputId = "State2", label = "State", choices = append(append(as.vector(unique(r_2007_df$State)[2:9]),"All States",0),"Cumulative",0)[1:9],selected = "All States",animation = "tada",outline = TRUE)),
                     conditionalPanel(condition = "input.month_or_year3 == 'Monthly'",prettyRadioButtons(inputId = "State2", label = "Area", choices = c("All States"),selected = "All States",animation = "tada",outline = TRUE))),
                            
                     column(width=3,prettyRadioButtons(inputId = "variable2", "Select Variable", choices = c("Sunshine", "Cloud Cover","Humidity"),selected = "Sunshine",animation = "smooth",outline = TRUE)),
                     column(width = 2)
                     ),
            conditionalPanel(condition = "input.month_or_year3 == 'Yearly'",
                             fluidRow(
                               column(width=1),
                               column(width=10,wellPanel(
                                 sliderInput(inputId = "Year_3", 
                                             label = "Year:",
                                             min = 2008,
                                             max = 2017,
                                             value = 2008,
                                             sep = "",
                                             step = 1,
                                             animate = animationOptions(interval = 1800,playButton = "PLAY",pauseButton = "PAUSE")))
                               ),
                               column(width=1)
                             ),
                             fluidRow(column(width=6,plotlyOutput(outputId = "factor")),
                                      column(width=6,plotlyOutput(outputId = "factor_density"))),
                             fluidRow(column(width=1),
                                      column(width=10,tableOutput(outputId = "text_carousel3_1") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 20px; padding-top: 1in;')),
                                      column(width = 1)
                                      )
                             ),
            
            conditionalPanel(condition = "input.month_or_year3 == 'Monthly'",
                             fluidRow(
                               column(width=1),
                               column(width=10,wellPanel(
                                 sliderTextInput(inputId = "Month_Year_3", 
                                                 label = "Month-Year",
                                                 grid = TRUE,
                                                 choices = c(as.yearmon("2008-01-01"),as.yearmon("2008-02-01"),as.yearmon("2008-03-01"),as.yearmon("2008-04-01"),as.yearmon("2008-05-01"),as.yearmon("2008-06-01"),as.yearmon("2008-07-01"),as.yearmon("2008-08-01"),as.yearmon("2008-09-01"),as.yearmon("2008-10-01"),as.yearmon("2008-11-01"),as.yearmon("2008-12-01"),as.yearmon("2009-01-01"),as.yearmon("2009-02-01"),as.yearmon("2009-03-01"),as.yearmon("2009-04-01"),as.yearmon("2009-05-01"),as.yearmon("2009-06-01"),as.yearmon("2009-07-01"),as.yearmon("2009-08-01"),as.yearmon("2009-09-01"),as.yearmon("2009-10-01"),as.yearmon("2009-11-01"),as.yearmon("2009-12-01"),as.yearmon("2010-01-01"),as.yearmon("2010-02-01"),as.yearmon("2010-03-01"),as.yearmon("2010-04-01"),as.yearmon("2010-05-01"),as.yearmon("2010-06-01"),as.yearmon("2010-07-01"),as.yearmon("2010-08-01"),as.yearmon("2010-09-01"),as.yearmon("2010-10-01"),as.yearmon("2010-11-01"),as.yearmon("2010-12-01"),as.yearmon("2011-01-01"),as.yearmon("2011-02-01"),as.yearmon("2011-03-01"),as.yearmon("2011-04-01"),as.yearmon("2011-05-01"),as.yearmon("2011-06-01"),as.yearmon("2011-07-01"),as.yearmon("2011-08-01"),as.yearmon("2011-09-01"),as.yearmon("2011-10-01"),as.yearmon("2011-11-01"),as.yearmon("2011-12-01"),as.yearmon("2012-01-01"),as.yearmon("2012-02-01"),as.yearmon("2012-03-01"),as.yearmon("2012-04-01"),as.yearmon("2012-05-01"),as.yearmon("2012-06-01"),as.yearmon("2012-07-01"),as.yearmon("2012-08-01"),as.yearmon("2012-09-01"),as.yearmon("2012-10-01"),as.yearmon("2012-11-01"),as.yearmon("2012-12-01"),as.yearmon("2013-01-01"),as.yearmon("2013-02-01"),as.yearmon("2013-03-01"),as.yearmon("2013-04-01"),as.yearmon("2013-05-01"),as.yearmon("2013-06-01"),as.yearmon("2013-07-01"),as.yearmon("2013-08-01"),as.yearmon("2013-09-01"),as.yearmon("2013-10-01"),as.yearmon("2013-11-01"),as.yearmon("2013-12-01"),as.yearmon("2014-01-01"),as.yearmon("2014-02-01"),as.yearmon("2014-03-01"),as.yearmon("2014-04-01"),as.yearmon("2014-05-01"),as.yearmon("2014-06-01"),as.yearmon("2014-07-01"),as.yearmon("2014-08-01"),as.yearmon("2014-09-01"),as.yearmon("2014-10-01"),as.yearmon("2014-11-01"),as.yearmon("2014-12-01"),as.yearmon("2015-01-01"),as.yearmon("2015-02-01"),as.yearmon("2015-03-01"),as.yearmon("2015-04-01"),as.yearmon("2015-05-01"),as.yearmon("2015-06-01"),as.yearmon("2015-07-01"),as.yearmon("2015-08-01"),as.yearmon("2015-09-01"),as.yearmon("2015-10-01"),as.yearmon("2015-11-01"),as.yearmon("2015-12-01"),as.yearmon("2016-01-01"),as.yearmon("2016-02-01"),as.yearmon("2016-03-01"),as.yearmon("2016-04-01"),as.yearmon("2016-05-01"),as.yearmon("2016-06-01"),as.yearmon("2016-07-01"),as.yearmon("2016-08-01"),as.yearmon("2016-09-01"),as.yearmon("2016-10-01"),as.yearmon("2016-11-01"),as.yearmon("2016-12-01"),as.yearmon("2017-01-01"),as.yearmon("2017-02-01"),as.yearmon("2017-03-01"),as.yearmon("2017-04-01"),as.yearmon("2017-05-01"),as.yearmon("2017-06-01")),
                                                 selected  = "Jan 2008",
                                                 animate = animationOptions(interval = 1800,playButton = "PLAY",pauseButton = "PAUSE")))
                               ),
                               column(width=1)
                             ),
                             fluidRow(column(width=6,plotlyOutput(outputId = "factor_monthly")),
                                      column(width=6,plotlyOutput(outputId = "factor_density_monthly"))),
                             fluidRow(
                                      column(width=12,tableOutput(outputId = "text_carousel3_2") %>% tagAppendAttributes(style= 'font-family: Lucida Console; font-size: 20px; padding-top: 1in;'))
                                     
                             )
                             )
            
            
          )
        )
        
     

               
               
)


# Server for the app
server <- function(input, output){
  
  # How to interact dialog boxes
  
  observeEvent(input$show1, {
    shinyalert(title = 'How to interact', imageUrl ="H1.png",closeOnEsc = TRUE,
               closeOnClickOutside = TRUE, imageWidth = 1000, imageHeight = 600,size = "l")
  })
  
  observeEvent(input$show2, {
    shinyalert(title = 'How to interact', imageUrl ="H2.png",closeOnEsc = TRUE,
               closeOnClickOutside = TRUE, imageWidth = 1000, imageHeight = 600,size = "l")
  })
  
  observeEvent(input$show3, {
    shinyalert(title = 'How to interact', imageUrl ="H3.png",closeOnEsc = TRUE,
               closeOnClickOutside = TRUE, imageWidth = 1000, imageHeight = 600,size = "l")
  })
  
  observeEvent(input$show4, {
    shinyalert(title = 'How to interact', imageUrl ="H4.png",closeOnEsc = TRUE,
               closeOnClickOutside = TRUE, imageWidth = 1000, imageHeight = 600,size = "l")
  })
  
  
  
  
  
  # Dynamic Text Outputs

  observeEvent(input$ggiraph_contour_selected,{
    
    
    output$text_carousel1_2 <- renderText({
      if(!is.null(input$ggiraph_contour_selected)){
      print(paste("Area Coverage %",round(nrow(overall[[as.character(input$Year_1a)]] %>% dplyr::filter(Total > as.numeric(strsplit(str_replace_all(input$ggiraph_contour_selected,"[:punct:]"," ") , " ")[1][[1]][2]) & Total < as.numeric(strsplit(str_replace_all(input$ggiraph_contour_selected,"[:punct:]"," ") , " ")[1][[1]][4]))) / nrow(overall[[as.character(input$Year_1a)]]) * 100, digits = 2)))
      }
        
         })
    
    
    
  })   
  

  observeEvent(input$ggiraph_contour_monthly_selected,{
    
    
    output$text_carousel1_3 <- renderText({
  
      if(!is.null(input$ggiraph_contour_monthly_selected)){
    
        Month = month.name[startsWith(month.name,substr(input$Month_Year_1a,1,3))]
        Year = substr(input$Month_Year_1a,5,8)
        print(paste("Area Coverage %",round(nrow(overall[[as.character(Year)]] %>% dplyr::filter(overall[[as.character(Year)]][[Month]] > as.numeric(strsplit(str_replace_all(input$ggiraph_contour_monthly_selected,"[:punct:]"," ") , " ")[1][[1]][2]) & overall[[as.character(Year)]][[Month]] < as.numeric(strsplit(str_replace_all(input$ggiraph_contour_monthly_selected,"[:punct:]"," ") , " ")[1][[1]][4]))) / nrow(overall[[as.character(Year)]]) * 100, digits = 2)))
      }
    })
    
  })
  
    output$text_carousel1_4 <- renderText({
      
      if(!is.null(current_state()))
      {
        if(input$month_or_year2 == "Yearly"){
          print(paste("Total Rainfall in ",current_state()$State,"in ", input$Year_1b, "differs from the decade average by",round(((round(overall[[as.character(input$Year_1b)]] %>% dplyr::filter(State  == as.character(current_state())) %>% dplyr::select(Total) %>% sum() - (overall[[paste0(as.character(current_state()), "_total")]] / 11), digits= 2)) / (overall[[paste0(as.character(current_state()), "_total")]] / 11)) * 100 , digits = 2), "%"))
        }
  
        
      }
      
    })
   
    
    output$text_carousel1_5 <- renderText({
      if(is.null(current_state())){
        if(!is.null(event_data("plotly_hover" , source = "locationbar_overall")$x) & !is.null(event_data("plotly_hover" , source = "locationbar_overall")$y)){
          location = dplyr::select(overall[[as.character(input$Year_1b)]] %>% dplyr::filter(! is.na(Location))  %>% dplyr::group_by(Location) %>% dplyr::summarise(sum(Total)) %>% dplyr::rename(Rainfall = 'sum(Total)'), .data[["Location"]])[[1]][as.numeric(event_data("plotly_hover" , source = "locationbar_overall")$x)]
          print(location)
          sum = overall[["2007"]] %>% dplyr::filter(Location == location) %>% dplyr::select(Total) %>% sum()
          for(year in c(2008:2017)){

            sum = sum +  overall[[as.character(year)]] %>% dplyr::filter(Location == location) %>% dplyr::select(Total) %>% sum()

          }
          avg = sum / 11
          perc = (((as.numeric(event_data("plotly_hover" , source = "locationbar_overall")$y)) - avg)/ avg) * 100
          

          print(paste("Rainfall in ",location," in ", input$Year_1b, "differs from the decade average by",round(perc,digits=2)," %" ))

        }
        
       
      }
      else{
        if(!is.null(event_data("plotly_hover" , source = "locationbar_state")$x) & !is.null(event_data("plotly_hover" , source = "locationbar_state")$y)){
        location = dplyr::select(overall[[as.character(input$Year_1b)]] %>% dplyr::filter(State == as.character(current_state())) %>% dplyr::group_by(Location) %>% dplyr::summarise(sum(Total)) %>% dplyr::rename(Rainfall = 'sum(Total)'), .data[["Location"]])[[1]][as.numeric(event_data("plotly_hover" , source = "locationbar_state")$x)]
        print(location)
        sum = overall[["2007"]] %>% dplyr::filter(Location == location) %>% dplyr::select(Total) %>% sum()
        for(year in c(2008:2017)){
          
          sum = sum +  overall[[as.character(year)]] %>% dplyr::filter(Location == location) %>% dplyr::select(Total) %>% sum()
          
        }
        avg = sum / 11
        perc = (((as.numeric(event_data("plotly_hover" , source = "locationbar_state")$y)) - avg)/ avg) * 100
        
        
        print(paste("Rainfall in ",location," in ", input$Year_1b, "differs from the decade average by",round(perc,digits = 2)," %" ))
        
        }
      }
    })
 


    
    
    output$text_carousel1_6 <- renderText({
      
      if(!is.null(current_state_monthly()))
      {

        
      if(input$month_or_year2 == "Monthly"){
        Month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
        Year = substr(input$Month_Year_1b,5,8)
        print(paste("Total Rainfall in ",current_state_monthly()$State,"in ", input$Month_Year_1b, "differs from the decade average by",round(((round(overall[[as.character(Year)]] %>% dplyr::filter(State  == as.character(current_state_monthly())) %>% dplyr::select(.data[[as.character(Month)]]) %>% sum() - (overall[[paste0(as.character(current_state_monthly()),"_",Month,"_total")]] / 11), digits= 2)) / (overall[[paste0(as.character(current_state_monthly()),"_",Month,"_total")]] / 11)) * 100 , digits = 2), "%"))
      }
      }
    })
    

    output$text_carousel1_7 <- renderText({

    if(is.null(current_state_monthly())){
      if(!is.null(event_data("plotly_hover" , source = "locationbar_monthly_overall")$x) & !is.null(event_data("plotly_hover" , source = "locationbar_monthly_overall")$y)){
        
        Year = substr(input$Month_Year_1b,5,8)
        Month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
        location = dplyr::select(overall[[as.character(Year)]] %>% dplyr::filter(! is.na(Location))  %>% dplyr::group_by(Location) %>% dplyr::summarise(sum(.data[[Month]])), .data[["Location"]])[[1]][as.numeric(event_data("plotly_hover" , source = "locationbar_monthly_overall")$x)]
        print(location)
        sum = overall[["2007"]] %>% dplyr::filter(Location == location) %>% dplyr::select(.data[[Month]]) %>% sum()
        for(year in c(2008:2017)){
          
          sum = sum +  overall[[as.character(year)]] %>% dplyr::filter(Location == location) %>% dplyr::select(.data[[Month]]) %>% sum()
          
        }
        avg = sum / 11
        perc = (((as.numeric(event_data("plotly_hover" , source = "locationbar_monthly_overall")$y)) - avg)/ avg) * 100
        
        
        print(paste("Rainfall in ",location," in ", input$Month_Year_1b, "differs from the decade average by",round(perc,digits=2)," %" ))
        
      }
      
    }
    else{
      
      if(!is.null(event_data("plotly_hover" , source = "locationbar_monthly_state")$x) & !is.null(event_data("plotly_hover" , source = "locationbar_monthly_state")$y)){
        
        Year = substr(input$Month_Year_1b,5,8)
        Month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
        location = dplyr::select(overall[[as.character(Year)]] %>% dplyr::filter(State == as.character(current_state_monthly())) %>% dplyr::group_by(Location) %>% dplyr::summarise(sum(.data[[Month]])), .data[["Location"]])[[1]][as.numeric(event_data("plotly_hover" , source = "locationbar_monthly_state")$x)]
        print(location)
        sum = overall[["2007"]] %>% dplyr::filter(Location == location) %>% dplyr::select(.data[[Month]]) %>% sum()
        for(year in c(2008:2017)){
          
          sum = sum +  overall[[as.character(year)]] %>% dplyr::filter(Location == location) %>% dplyr::select(.data[[Month]]) %>% sum()
          
        }
        avg = sum / 11
        perc = (((as.numeric(event_data("plotly_hover" , source = "locationbar_monthly_state")$y)) - avg)/ avg) * 100
        
        print(paste("Rainfall in ",location," in ", input$Month_Year_1b, "differs from the decade average by",round(perc,digits = 2)," %" ))
        
      }
    }
    

  

  })
 
    
    
    
    observe({
      
      output$text_carousel2a <- renderText({
        
        print("The above plot indicates that overall, Summers recieve the maximum rainfall while Winter recieves the least rainfall")
        
      })
    })
    
    
    
    observe({
      
      output$text_carousel2b <- renderText({
        
        print("The above plot indicates that the rainfall values gradually decrease from January as months pass and start increasing from September")
        
        
      })
    })
    
    
  
    observe({
      output$text_carousel2_1 <- renderText({
        
        if(input$State1 == "All States"){
          
          if(!is.null(event_data("plotly_hover", source = "season")$x) & !is.null(event_data("plotly_hover", source = "season")$y)){
            season_num = event_data("plotly_hover", source = "season")$x
            
            if(season_num == 1){
              season = "Summer"
            }
            else if(season_num == 2){
              season = "Autumn"
            }
            else if(season_num == 3){
              season = "Winter"
            }
            else if(season_num == 4){
              season = "Spring"
            }
            
            
            print(paste("Total rainfall in ",season," of ",input$Year_2,"differs from the decade average by ",round(((as.numeric(current_season_rainfall()) - (overall[[paste0(season,"_total")]] / 11)) / (overall[[paste0(season,"_total")]] / 11)) * 100, digits= 2)," %"))
            
            
          }
        }
        
        else{
          
          if(!is.null(event_data("plotly_hover", source = "season")$x) & !is.null(event_data("plotly_hover", source = "season")$y)){
            
            
            season_num = event_data("plotly_hover", source = "season")$x
            
            if(season_num == 1){
              season = "Summer"
            }
            else if(season_num == 2){
              season = "Autumn"
            }
            else if(season_num == 3){
              season = "Winter"
            }
            else if(season_num == 4){
              season = "Spring"
            } 
            
            print(paste("Total rainfall in ",season," of ",input$Year_2," in ", input$State1,"differs from the decade average by ",round(((as.numeric(current_season_rainfall()) - (overall[[paste0(input$State1,"_",season,"_total")]] / 11)) / (overall[[paste0(input$State1,"_",season,"_total")]] / 11)) * 100, digits= 2)," %")) 
            
            
          }
          
        }
        
      })
    })
    
    observe({
      output$text_carousel2_2 <- renderText({

        if(input$State1 == "All States"){

          if(!is.null(event_data("plotly_hover", source = "month")$x) & !is.null(event_data("plotly_hover", source = "month")$y)){
          Month = month.name[as.numeric(event_data("plotly_hover", source = "month")$x)]

          print(paste("Total Rainfall in ", Month," ",input$Year_2, "differs from the decade average by",round((((as.numeric(event_data("plotly_hover", source = "month")$y)) - (overall[[paste0(Month,"_total")]] / 11)) / (overall[[paste0(Month,"_total")]] / 11)) * 100,digits = 2), "%"))
        }
        }
        else{
          if(!is.null(event_data("plotly_hover", source = "month")$x) & !is.null(event_data("plotly_hover", source = "month")$y)){
            Month = month.name[as.numeric(event_data("plotly_hover", source = "month")$x)]

            print(paste("Total Rainfall in ",input$State1," in ", Month," ",input$Year_2, "differs from the decade average by",round((((as.numeric(event_data("plotly_hover", source = "month")$y)) - (overall[[paste0(input$State1,"_",Month,"_total")]] / 11)) / (overall[[paste0(input$State1,"_",Month,"_total")]] / 11)) * 100,digits = 2), "%"))
          }


        }

      })
    })
    
    
    
    
    observe({
      
      output$text_carousel3_factor_whole_info <- renderText(
        
        print("Focus on the increasing/decreasing trend as you move from left to right")
        
        
      )
      
    }) 
    
    
    observe({
      
      output$text_carousel3_density_whole_info <- renderText(
        
        print("Focus on the extra bulging out area on the left and the right of the intersection point of the two area lines")
        
        
      )
      
    }) 
    
    
    observe({
      
      output$text_carousel3a <- renderText(
        
        print("The plot on the right shows how the probability that it rained on a particular day is low for higher values of Sunshine represented by the extra bulging red area on the right end and vice-versa seen from the green bulging area on the left end")
                    
                   
      )
      
    }) 
    
    observe({
      
      output$text_carousel3b <- renderText(
        
        print("The plot on the right shows how the probability that it rained on a particular day is high for higher values of Cloud Cover represented by the extra bulging green area on the right end and vice-versa seen from the red bulging area on the left end")
                    
                   
      )
      
    }) 
    
    observe({
      
      output$text_carousel3c <- renderText(
        
        print("The plot on the right shows how the probability that it rained on a particular day is high for higher values of Humidity represented by the extra bulging green area on the right end and vice-versa seen from the red bulging area on the left end")
      )
      
    }) 
    
    
    
    # Dynamic Tables 
    
    observe({
      output$text_carousel3_1 <- renderTable({
        variable_access <- hash()
        variable_access[["Sunshine"]] <- "Sunshine"
        variable_access[["Cloud Cover"]] <- "Cloud9am"
        variable_access[["Humidity"]] <- "Humidity9am"
        
        
        if(input$State2 == "All States" | input$State2 == "Cumulative"){
          
          
          if(variable_access[[input$variable2]] == "Sunshine"){
            
            a <- data[data$Year == input$Year_3,] %>% summarise(min(Rainfall), median(Rainfall),max(Rainfall),min(Sunshine), median(Sunshine),max(Sunshine))
            
            # Min Sunshine Max Rainfall
            a1 <- data[data$Year == input$Year_3 ,]  %>% dplyr::select(Date,State,Location,Rainfall,Sunshine) %>% dplyr::filter(Sunshine == a[[4]]) %>% dplyr::slice_max(Rainfall) %>% dplyr::slice_head()
            
            # Median Sunshine 1 Rainfall row
            a2 <- data[data$Year == input$Year_3 ,]  %>% dplyr::select(Date,State,Location,Rainfall,Sunshine) %>% dplyr::filter(Sunshine == a[[5]]) %>% dplyr::slice_head()
            
            # Max Sunshine Min Rainfall
            a3 <- data[data$Year == input$Year_3 ,] %>% dplyr::select(Date,State,Location,Rainfall,Sunshine) %>% dplyr::filter(Sunshine == a[[6]]) %>% dplyr::slice_min(Rainfall) %>% dplyr::slice_head()
            
            outputTable = do.call("rbind", list(a1,a2,a3))
            outputTable$Date = format(outputTable$Date,'%Y-%m-%d')
            outputTable[["Significance of the row"]] = c("HIGHEST RAINFALL observed for LOWEST SUNSHINE","AVERAGE RAINFALL observed for AVERAGE SUNSHINE","LOWEST RAINFALL observed for HIGHEST SUNSHINE")
            names(outputTable)[names(outputTable) == "Rainfall"] <- "Rainfall(in mm)"
            names(outputTable)[names(outputTable) == "Sunshine"] <- "Sunshine(in hours per day)"
            print(outputTable) 
          }
          else{
            a <- data[data$Year == input$Year_3,] %>% summarise(min(Rainfall), median(Rainfall),max(Rainfall),min(.data[[variable_access[[input$variable2]]]]), median(.data[[variable_access[[input$variable2]]]]),max(.data[[variable_access[[input$variable2]]]]))
            
            # Min Factor Min Rainfall
            a1 <- data[data$Year == input$Year_3 ,]  %>% dplyr::select(Date,State,Location,Rainfall,.data[[variable_access[[input$variable2]]]]) %>% dplyr::filter(.data[[variable_access[[input$variable2]]]] == a[[4]]) %>% dplyr::slice_min(Rainfall) %>% dplyr::slice_head()
            
            # Median Sunshine 1 Rainfall row
            a2 <- data[data$Year == input$Year_3 ,]  %>% dplyr::select(Date,State,Location,Rainfall,.data[[variable_access[[input$variable2]]]]) %>% dplyr::filter(.data[[variable_access[[input$variable2]]]] == a[[5]]) %>% dplyr::slice_head()
            
            # Max Factor Max Rainfall
            a3 <- data[data$Year == input$Year_3 ,] %>% dplyr::select(Date,State,Location,Rainfall,.data[[variable_access[[input$variable2]]]]) %>% dplyr::filter(.data[[variable_access[[input$variable2]]]] == a[[6]]) %>% dplyr::slice_max(Rainfall) %>% dplyr::slice_head()
            
            outputTable = do.call("rbind", list(a1,a2,a3))
            outputTable$Date = format(outputTable$Date,'%Y-%m-%d')
            outputTable[["Significance of the row"]] = c(paste("LOWEST RAINFALL observed for LOWEST", toupper(input$variable2)),paste("AVERAGE RAINFALL observed for AVERAGE", toupper(input$variable2)),paste("HIGHEST RAINFALL observed for HIGHEST", toupper(input$variable2)))
            
            if(input$variable2 == "Cloud Cover"){
              names(outputTable)[names(outputTable) == "Cloud9am"] <- "Cloud Cover(in oktas)"
            }
            else if(input$variable2 == "Humidity"){
              names(outputTable)[names(outputTable) == "Humidity9am"] <- "Humidity(in %)"
            }
            
            names(outputTable)[names(outputTable) == "Rainfall"] <- "Rainfall(in mm)"
            print(outputTable)
            
            
            
          }
          
          
          
        }
        
        else{
          
        if(variable_access[[input$variable2]] == "Sunshine"){
          
          a <- data[data$Year == input$Year_3 & data$State == input$State2,] %>% dplyr::group_by(State) %>% summarise(min(Rainfall), median(Rainfall),max(Rainfall),min(Sunshine), median(Sunshine),max(Sunshine))
          
          # Min Sunshine Max Rainfall
          a1 <- data[data$Year == input$Year_3 & data$State == input$State2,] %>% dplyr::group_by(State)  %>% dplyr::select(Date,State,Location,Rainfall,Sunshine) %>% dplyr::filter(Sunshine == a[[5]]) %>% dplyr::slice_max(Rainfall) %>% dplyr::slice_head()
          
          # Median Sunshine 1 Rainfall row
          a2 <- data[data$Year == input$Year_3 & data$State == input$State2,] %>% dplyr::group_by(State)  %>% dplyr::select(Date,State,Location,Rainfall,Sunshine) %>% dplyr::filter(Sunshine == a[[6]]) %>% dplyr::slice_head()
          
          # Max Sunshine Min Rainfall
          a3 <- data[data$Year == input$Year_3 & data$State == input$State2,] %>% dplyr::group_by(State) %>% dplyr::select(Date,State,Location,Rainfall,Sunshine) %>% dplyr::filter(Sunshine == a[[7]]) %>% dplyr::slice_min(Rainfall) %>% dplyr::slice_head()
          
          outputTable = do.call("rbind", list(a1,a2,a3))
          outputTable$Date = format(outputTable$Date,'%Y-%m-%d')
          if(nrow(outputTable) == 2){
            outputTable[["Significance of the row"]] = c("HIGHEST RAINFALL observed for LOWEST SUNSHINE","LOWEST RAINFALL observed for HIGHEST SUNSHINE")
          }
          else if(nrow(outputTable) == 3){
            outputTable[["Significance of the row"]] = c("HIGHEST RAINFALL observed for LOWEST SUNSHINE","AVERAGE RAINFALL observed for AVERAGE SUNSHINE","LOWEST RAINFALL observed for HIGHEST SUNSHINE")
            
          }
          
          names(outputTable)[names(outputTable) == "Rainfall"] <- "Rainfall(in mm)"
          names(outputTable)[names(outputTable) == "Sunshine"] <- "Sunshine(in hours per day)"
          print(outputTable)
        }
          
        else{
          
          a <- data[data$Year == input$Year_3 & data$State == input$State2,] %>% dplyr::group_by(State) %>% summarise(min(Rainfall), median(Rainfall),max(Rainfall),min(.data[[variable_access[[input$variable2]]]]), median(.data[[variable_access[[input$variable2]]]]),max(.data[[variable_access[[input$variable2]]]]))
          
          # Min Factor Min Rainfall
          a1 <- data[data$Year == input$Year_3 & data$State == input$State2,] %>% dplyr::group_by(State)  %>% dplyr::select(Date,State,Location,Rainfall,.data[[variable_access[[input$variable2]]]]) %>% dplyr::filter(.data[[variable_access[[input$variable2]]]] == a[[5]]) %>% dplyr::slice_min(Rainfall) %>% dplyr::slice_head()
          
          # Median Sunshine 1 Rainfall row
          a2 <- data[data$Year == input$Year_3 & data$State == input$State2,] %>% dplyr::group_by(State)  %>% dplyr::select(Date,State,Location,Rainfall,.data[[variable_access[[input$variable2]]]]) %>% dplyr::filter(.data[[variable_access[[input$variable2]]]] == a[[6]]) %>% dplyr::slice_head()
          
          # Max Factor Max Rainfall
          a3 <- data[data$Year == input$Year_3 & data$State == input$State2,] %>% dplyr::group_by(State) %>% dplyr::select(Date,State,Location,Rainfall,.data[[variable_access[[input$variable2]]]]) %>% dplyr::filter(.data[[variable_access[[input$variable2]]]] == a[[7]]) %>% dplyr::slice_max(Rainfall) %>% dplyr::slice_head()
          
          outputTable = do.call("rbind", list(a1,a2,a3))
          outputTable$Date = format(outputTable$Date,'%Y-%m-%d')
          if(nrow(outputTable) == 2){
            outputTable[["Significance of the row"]] = c(paste("LOWEST RAINFALL observed for LOWEST", toupper(input$variable2)),paste("HIGHEST RAINFALL observed for HIGHEST", toupper(input$variable2)))
          }
          else if(nrow(outputTable) == 3){
            outputTable[["Significance of the row"]] = c(paste("LOWEST RAINFALL observed for LOWEST", toupper(input$variable2)),paste("AVERAGE RAINFALL observed for AVERAGE", toupper(input$variable2)),paste("HIGHEST RAINFALL observed for HIGHEST", toupper(input$variable2)))
            
          }
          
          if(input$variable2 == "Cloud Cover"){
            names(outputTable)[names(outputTable) == "Cloud9am"] <- "Cloud Cover(in oktas)"
          }
          else if(input$variable2 == "Humidity"){
            names(outputTable)[names(outputTable) == "Humidity9am"] <- "Humidity(in %)"
          }
          names(outputTable)[names(outputTable) == "Rainfall"] <- "Rainfall(in mm)"
          print(outputTable)
          
          
        }
        }
        
        
        
      })
    })
    
    observe({
      output$text_carousel3_2 <- renderTable({
        variable_access <- hash()
        variable_access[["Sunshine"]] <- "Sunshine"
        variable_access[["Cloud Cover"]] <- "Cloud9am"
        variable_access[["Humidity"]] <- "Humidity9am"
        
        month = as.numeric(c(1:12)[month.name[startsWith(month.name,substr(input$Month_Year_3,1,3))] == month.name])
        year = as.numeric(substr(input$Month_Year_3,5,8))
        # if(input$State2 == "All States" | input$State2 == "Cumulative"){
          
        if(month == 4 & year == 2011){
          year = 2015
        }
        else if(month == 12 & year == 2012){
          year = 2015
        }
        else if(month == 2 & year == 2013){
          year = 2015
        }
        


          if(variable_access[[input$variable2]] == "Sunshine"){
            
            a <- data[data$Year == year & data$Month == month,] %>% summarise(min(Rainfall), median(Rainfall),max(Rainfall),min(Sunshine), median(Sunshine),max(Sunshine))
            
            # Min Sunshine Max Rainfall
            a1 <- data[data$Year == year & data$Month == month,]  %>% dplyr::select(Date,State,Location,Rainfall,Sunshine) %>% dplyr::filter(Sunshine == a[[4]]) %>% dplyr::slice_max(Rainfall) %>% dplyr::slice_head()
            
            # Median Sunshine 1 Rainfall row
            a2 <- data[data$Year == year & data$Month == month,]  %>% dplyr::select(Date,State,Location,Rainfall,Sunshine) %>% dplyr::filter(Sunshine == a[[5]]) %>% dplyr::slice_head()
            
            # Max Sunshine Min Rainfall
            a3 <- data[data$Year == year & data$Month == month,] %>% dplyr::select(Date,State,Location,Rainfall,Sunshine) %>% dplyr::filter(Sunshine == a[[6]]) %>% dplyr::slice_min(Rainfall) %>% dplyr::slice_head()
            
            outputTable = do.call("rbind", list(a1,a2,a3))
            
            
            
            if(month == 4 & as.numeric(substr(input$Month_Year_3,5,8)) == 2011){
              outputTable$Date <- as.Date(outputTable$Date)
              lubridate::year(outputTable$Date) <- 2011
            }
            else if(month == 12 & as.numeric(substr(input$Month_Year_3,5,8)) == 2012){
              outputTable$Date <- as.Date(outputTable$Date)
              lubridate::year(outputTable$Date) <- 2012

            }
            else if(month == 2 & as.numeric(substr(input$Month_Year_3,5,8)) == 2013){
              outputTable$Date <- as.Date(outputTable$Date)
              lubridate::year(outputTable$Date) <- 2013
    
            }
            outputTable$Date = format(outputTable$Date,'%Y-%m-%d')
            
            if(nrow(outputTable) == 2){
              outputTable[["Significance of the row"]] = c("HIGHEST RAINFALL observed for LOWEST SUNSHINE","LOWEST RAINFALL observed for HIGHEST SUNSHINE")
            }
            else if(nrow(outputTable) == 3){
              outputTable[["Significance of the row"]] = c("HIGHEST RAINFALL observed for LOWEST SUNSHINE","AVERAGE RAINFALL observed for AVERAGE SUNSHINE","LOWEST RAINFALL observed for HIGHEST SUNSHINE")
              
            }
            names(outputTable)[names(outputTable) == "Rainfall"] <- "Rainfall(in mm)"
            names(outputTable)[names(outputTable) == "Sunshine"] <- "Sunshine(in hours per day)"
            print(outputTable) 
          }
          else{
            a <- data[data$Year == year & data$Month == month,] %>% summarise(min(Rainfall), median(Rainfall),max(Rainfall),min(.data[[variable_access[[input$variable2]]]]), median(.data[[variable_access[[input$variable2]]]]),max(.data[[variable_access[[input$variable2]]]]))
            
            # Min Factor Min Rainfall
            a1 <- data[data$Year == year & data$Month == month,]  %>% dplyr::select(Date,State,Location,Rainfall,.data[[variable_access[[input$variable2]]]]) %>% dplyr::filter(.data[[variable_access[[input$variable2]]]] == a[[4]]) %>% dplyr::slice_min(Rainfall) %>% dplyr::slice_head()
            
            # Median Sunshine 1 Rainfall row
            a2 <- data[data$Year == year & data$Month == month,]  %>% dplyr::select(Date,State,Location,Rainfall,.data[[variable_access[[input$variable2]]]]) %>% dplyr::filter(.data[[variable_access[[input$variable2]]]] == a[[5]]) %>% dplyr::slice_head()
            
            # Max Factor Max Rainfall
            a3 <- data[data$Year == year & data$Month == month,] %>% dplyr::select(Date,State,Location,Rainfall,.data[[variable_access[[input$variable2]]]]) %>% dplyr::filter(.data[[variable_access[[input$variable2]]]] == a[[6]]) %>% dplyr::slice_max(Rainfall) %>% dplyr::slice_head()
            
            outputTable = do.call("rbind", list(a1,a2,a3))
            
            if(month == 4 & as.numeric(substr(input$Month_Year_3,5,8)) == 2011){
              outputTable$Date <- as.Date(outputTable$Date)
              lubridate::year(outputTable$Date) <- 2011
            }
            else if(month == 12 & as.numeric(substr(input$Month_Year_3,5,8)) == 2012){
              outputTable$Date <- as.Date(outputTable$Date)
              lubridate::year(outputTable$Date) <- 2012
              
            }
            else if(month == 2 & as.numeric(substr(input$Month_Year_3,5,8)) == 2013){
              outputTable$Date <- as.Date(outputTable$Date)
              lubridate::year(outputTable$Date) <- 2013
              
            }
            
            outputTable$Date = format(outputTable$Date,'%Y-%m-%d')
            if((month == 4 & year == 2011) | (month == 12 & year == 2012) | (month == 2 & year == 2013)){
              lubridate::year(outputTable$Date) <- 2011
              print(month)
              print(year)
            }
            if(nrow(outputTable) == 2){
              outputTable[["Significance of the row"]] = c(paste("LOWEST RAINFALL observed for LOWEST", toupper(input$variable2)),paste("HIGHEST RAINFALL observed for HIGHEST", toupper(input$variable2)))
            }
            else if(nrow(outputTable) == 3){
              outputTable[["Significance of the row"]] = c(paste("LOWEST RAINFALL observed for LOWEST", toupper(input$variable2)),paste("AVERAGE RAINFALL observed for AVERAGE", toupper(input$variable2)),paste("HIGHEST RAINFALL observed for HIGHEST", toupper(input$variable2)))
              
            }
            if(input$variable2 == "Cloud Cover"){
              names(outputTable)[names(outputTable) == "Cloud9am"] <- "Cloud Cover(in oktas)"
            }
            else if(input$variable2 == "Humidity"){
              names(outputTable)[names(outputTable) == "Humidity9am"] <- "Humidity(in %)"
            }
            names(outputTable)[names(outputTable) == "Rainfall"] <- "Rainfall(in mm)"
            print(outputTable)
            
            
            
          }
          
      
    
      })
    })
    
    
    
  
    # Factor relationship plot for the whole decade
    
    output$factor_whole <- renderPlotly({

     s1 <- ggplotly(ggplot(data,aes(y = Rainfall, x = Sunshine)) +
                 geom_smooth(se=FALSE)  +
                   scale_color_brewer(type = "qual",palette = "Accent") +
                 labs(x = "Sunshine(in number of hours per day)", y = "Rainfall(in mm)") +
                 theme_par()
                 ) %>% layout(height = 1000,width = 400)  %>% config(displayModeBar = FALSE)
    
     Cloud_Cover = data$Cloud9am
     
     c1 <- ggplotly(ggplot(data,aes(y = Rainfall, x = Cloud_Cover)) +
                geom_smooth(se=FALSE)  +
                labs(x = "Cloud Cover(in oktas)", y = "Rainfall(in mm)") +
                theme_par()
     ) %>% layout(height = 1000,width = 400)   %>% config(displayModeBar = FALSE)
     
     
     
     Humidity = data$Humidity9am
     h1 <- ggplotly(ggplot(data,aes(y = Rainfall, x = Humidity)) +
                geom_smooth(se=FALSE)  +
                labs(x = "Humidity(in %)" , y = "Rainfall(in mm)") +
                theme_par()
     ) %>% layout(height = 1000,width = 400)  %>% config(displayModeBar = FALSE)
     

     subplot(s1,c1,h1,nrows = 3,titleX = TRUE,titleY = TRUE,margin = 0.05)
      
    })
    
    # Multi density plot for the whole decade
    
    output$density_whole <- renderPlotly({
      
      s2 <- ggplotly(ggplot(data,aes(x = Sunshine, group = RainToday, fill = RainToday)) +
                 geom_density(adjust=1.5,alpha = 0.7,show.legend = FALSE) +
                 labs(x = "Sunshine(in number of hours per day)", y ="Density of records observed") +
                 theme_par() + theme( legend.title = element_blank(), legend.position = "none") 
      ,tooltip = c("x","group") )   %>% layout(height = 1000,width = 500)  %>% config(displayModeBar = FALSE) 
     
      Cloud_Cover = data$Cloud9am
      c2 <- ggplotly(ggplot(data,aes(x = Cloud_Cover, group = RainToday, fill = RainToday)) +
                 geom_density(adjust=1.5,alpha = 0.7,show.legend = FALSE) +
                 labs(x = "Cloud Cover(in oktas)", y ="Density of records observed") +
                 theme_par() + theme( legend.title = element_blank(), legend.position = "none") 
               ,tooltip = c("x","group"))  %>% layout(height = 1000,width = 500)  %>% config(displayModeBar = FALSE)
      
      
      Humidity = data$Humidity9am
      h2 <- ggplotly(ggplot(data,aes(x = Humidity, group = RainToday, fill = RainToday)) +
                 geom_density(adjust=1.5,alpha = 0.7,show.legend = TRUE) +
                 labs(x = "Humidity(in %)", y ="Density of records observed") +
                 theme_par() + theme( legend.title = element_blank()) 
               ,tooltip = c("x","group"))  %>% layout(height = 1000,width = 500, legend=list(title=list(text='<b> Did it Rain? </b>'))) %>% config(displayModeBar = FALSE)
    
      
      subplot(style(s2,showlegend = F),style(c2,showlegend = F),h2,nrows = 3,titleX = TRUE,titleY = TRUE, margin = 0.05)
      
    })
    
    # Factor relationship plot interactive yearly
    
      output$factor <- renderPlotly({
      variable_label <- hash()
      variable_label[["Sunshine"]] <- "Sunshine(in number of hours per day)"
      variable_label[["Cloud9am"]] <- "Cloud Cover(in oktas)"
      variable_label[["Humidity9am"]] <- "Humidity(in %)"
      
      variable_access <- hash()
      variable_access[["Sunshine"]] <- "Sunshine"
      variable_access[["Cloud Cover"]] <- "Cloud9am"
      variable_access[["Humidity"]] <- "Humidity9am"
      
      if(input$variable2 == "Sunshine" & input$State2 == "Australian Capital Territory" & input$Year_3 %in% c(2013,2014,2015,2016,2017)){
      
        shinyalert("Oops!", "Sunshine values are not spread out enough to show trend here", type = "warning")
      
      }
      
      if(input$variable2 == "Sunshine"){
        
        if(input$State2 == "All States"){
          dm <- data[data$Year == input$Year_3,]
          Variable = dm[[variable_access[[input$variable2]]]]
          
          Sunshine = Variable
          ggplotly(ggplot(dm,aes(y = Rainfall, x = Sunshine, text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]))) +
                     geom_smooth(se=FALSE,aes(group = 1))  +
                     ggtitle("Relationship between Sunshine and Rainfall") +
                     labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                     scale_color_brewer(type = "qual",palette = "Accent") +
                     theme_par(),tooltip = c("text","x","y"),source = "factor_yearly_overall")  %>% layout(autosize = TRUE, height = 500) %>% hide_legend() %>% config(displayModeBar = FALSE)
          
        
        }
        else if(input$State2 == "Cumulative"){
          dm <- data[data$Year == input$Year_3,]
          Variable = dm[[variable_access[[input$variable2]]]]
          Sunshine = Variable
          ggplotly(ggplot(dm,aes(y = Rainfall, x = Sunshine, color = State, text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]))) +
                     geom_smooth(method = "loess", se=FALSE,aes(group = State), span=1)  +
                     ggtitle("Relationship between Sunshine and Rainfall") +
                     theme(plot.title = element_text(size = 8, hjust = 1)) +
                     labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                     scale_color_brewer(type = "qual",palette = "Accent") +
                     theme_par(),tooltip = c("text","x","y","color"),source = "factor_yearly_cumulative")  %>% layout(autosize = TRUE, height = 500, title= list(x=0.6))  %>% config(displayModeBar = FALSE)
          
          
          
        }
        else{
          
          
          dm <- data[data$Year == input$Year_3 & data$State == input$State2,]
          Variable = dm[[variable_access[[input$variable2]]]]
          
          Sunshine = Variable
          
          ggplotly(ggplot(dm,aes(y = Rainfall, x = Sunshine, text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]))) +
                     geom_smooth(se=FALSE,aes(group = 1),span = 1)  +
                     ggtitle("Relationship between Sunshine and Rainfall") +
                     labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                     scale_color_brewer(type = "qual",palette = "Accent") +
                     theme_par(),tooltip = c("text","x","y"),source = "factor_yearly_monthly")  %>% layout(autosize = TRUE, height = 500)  %>% hide_legend()  %>% config(displayModeBar = FALSE)
          
        
          
          
        }
      }
      else if(input$variable2 == "Cloud Cover"){
        
       
        
        if(input$State2 == "All States"){
          dm <- data[data$Year == input$Year_3,]
          Variable = dm[[variable_access[[input$variable2]]]]
          
          Cloud_Cover = Variable
          
          ggplotly(ggplot(dm,aes(y = Rainfall, x = Cloud_Cover, text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]))) +
                     geom_smooth(se=FALSE,aes(group = 1))  +
                     ggtitle("Relationship between Cloud Cover and Rainfall") +
                     labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                     scale_color_brewer(type = "qual",palette = "Accent") +
                     theme_par(),tooltip = c("text","x","y"),source = "factor_yearly_overall")  %>% layout(autosize = TRUE, height = 500) %>% hide_legend()  %>% config(displayModeBar = FALSE)
          
        
        }
        else if(input$State2 == "Cumulative"){
          dm <- data[data$Year == input$Year_3,]
          Variable = dm[[variable_access[[input$variable2]]]]
          Cloud_Cover = Variable
          ggplotly(ggplot(dm,aes(y = Rainfall, x = Cloud_Cover, color = State, text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]))) +
                     geom_smooth(method = "loess", se=FALSE,aes(group = State), span=1)  +
                     ggtitle("Relationship between Cloud Cover and Rainfall") +
                     theme(plot.title = element_text(size = 8, hjust = 1)) +
                     labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                     scale_color_brewer(type = "qual",palette = "Accent") +
                     theme_par(),tooltip = c("text","x","y","color"),source = "factor_yearly_cumulative")  %>% layout(autosize = TRUE, height = 500, title= list(x=0.6)) %>% config(displayModeBar = FALSE)
          
          
          
        }
        else{
          
          
          dm <- data[data$Year == input$Year_3 & data$State == input$State2,]
          Variable = dm[[variable_access[[input$variable2]]]]
          
          Cloud_Cover = Variable
          
          ggplotly(ggplot(dm,aes(y = Rainfall, x = Cloud_Cover, text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]))) +
                     geom_smooth(se=FALSE,aes(group = 1),span = 1)  +
                     ggtitle("Relationship between Cloud Cover and Rainfall") +
                     labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                     scale_color_brewer(type = "qual",palette = "Accent") +
                     theme_par(),tooltip = c("text","x","y"),source = "factor_yearly_monthly")  %>% layout(autosize = TRUE, height = 500)  %>% hide_legend()  %>% config(displayModeBar = FALSE)
          
        
          
          
        }
      }
      else if(input$variable2 == "Humidity"){
        
         
        
        if(input$State2 == "All States"){
          dm <- data[data$Year == input$Year_3,]
          Variable = dm[[variable_access[[input$variable2]]]]
          
          Humidity = Variable
          
          ggplotly(ggplot(dm,aes(y = Rainfall, x = Humidity, text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]))) +
                     geom_smooth(se=FALSE,aes(group = 1))  +
                     ggtitle("Relationship between Humidity and Rainfall") +
                     labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                     scale_color_brewer(type = "qual",palette = "Accent") +
                     theme_par(),tooltip = c("text","x","y"),source = "factor_yearly_overall")  %>% layout(autosize = TRUE, height = 500) %>% hide_legend() %>% config(displayModeBar = FALSE)
          
        
        }
        else if(input$State2 == "Cumulative"){
          dm <- data[data$Year == input$Year_3,]
          Variable = dm[[variable_access[[input$variable2]]]]
          Humidity = Variable
          ggplotly(ggplot(dm,aes(y = Rainfall, x = Humidity, color = State, text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]))) +
                     geom_smooth(method = "loess", se=FALSE,aes(group = State), span=1)  +
                     ggtitle("Relationship between Humidity and Rainfall") +
                     theme(plot.title = element_text(size = 8, hjust = 1)) +
                     labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                     scale_color_brewer(type = "qual",palette = "Accent") +
                     theme_par(),tooltip = c("text","x","y","color"),source = "factor_yearly_cumulative")  %>% layout(autosize = TRUE, height = 500, title= list(x=0.6)) %>% config(displayModeBar = FALSE)
          
          
          
        }
        else{
          
          
          dm <- data[data$Year == input$Year_3 & data$State == input$State2,]
          Variable = dm[[variable_access[[input$variable2]]]]
          
          Humidity = Variable
          
          ggplotly(ggplot(dm,aes(y = Rainfall, x = Humidity, text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]))) +
                     geom_smooth(se=FALSE,aes(group = 1),span = 1)  +
                     ggtitle("Relationship between Humidity and Rainfall") +
                     labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                     scale_color_brewer(type = "qual",palette = "Accent") +
                     theme_par(),tooltip = c("text","x","y"),source = "factor_yearly_monthly")  %>% layout(autosize = TRUE, height = 500)  %>% hide_legend() %>% config(displayModeBar = FALSE)
          
          

          
        }
      }
      
      
      })

      # Factor relationship plot interactive monthly
      
      output$factor_monthly <- renderPlotly({
        variable_label <- hash()
        variable_label[["Sunshine"]] <- "Sunshine(in number of hours per day)"
        variable_label[["Cloud9am"]] <- "Cloud Cover(in oktas)"
        variable_label[["Humidity9am"]] <- "Humidity(in %)"
         
        variable_access <- hash()
        variable_access[["Sunshine"]] <- "Sunshine"
        variable_access[["Cloud Cover"]] <- "Cloud9am"
        variable_access[["Humidity"]] <- "Humidity9am"
        if(input$variable2 == "Sunshine"){
          if(input$Month_Year_3 == "Apr 2011"){
          dm = data[data$Year == 2015 & data$Month == 4,]
          Variable <- dm[[variable_access[[input$variable2]]]]
          Sunshine = Variable
          ggplotly(ggplot(dm,aes(y = Rainfall, x = Sunshine,text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]))) +
                     geom_smooth(se=FALSE,aes(group = 1), span=1)  +
                     ggtitle("Relationship between Sunshine and Rainfall") +
                     labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                     theme_par(),tooltip = c("text","x","y"),source = "factor_monthly") %>% layout(autosize = TRUE, height = 500)  %>% config(displayModeBar = FALSE)
          }
          
          else if(input$Month_Year_3 == "Dec 2012"){
            dm = data[data$Year == 2015 & data$Month == 12,]
            Variable <- dm[[variable_access[[input$variable2]]]]
            Sunshine = Variable
            ggplotly(ggplot(dm,aes(y = Rainfall, x = Sunshine,text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]))) +
                       geom_smooth(se=FALSE,aes(group = 1), span=1)  +
                       ggtitle("Relationship between Sunshine and Rainfall") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                       theme_par(),tooltip = c("text","x","y"),source = "factor_monthly") %>% layout(autosize = TRUE, height = 500) %>% config(displayModeBar = FALSE)
            
          }
          
          else if(input$Month_Year_3 == "Feb 2013"){
            dm = data[data$Year == 2015 & data$Month == 2,]
            Variable <- dm[[variable_access[[input$variable2]]]]
            Sunshine = Variable
            ggplotly(ggplot(dm,aes(y = Rainfall, x = Sunshine,text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]))) +
                       geom_smooth(se=FALSE,aes(group = 1), span=1)  +
                       ggtitle("Relationship between Sunshine and Rainfall") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                       theme_par(),tooltip = c("text","x","y"),source = "factor_monthly") %>% layout(autosize = TRUE, height = 500) %>% config(displayModeBar = FALSE)
            
          }
          
          else{
            dm <- data[data$Year == as.numeric(substr(input$Month_Year_3,5,8)),]
            
            dm <- dm[(dm$Month == as.numeric(c(1:12)[month.name[startsWith(month.name,substr(input$Month_Year_3,1,3))] == month.name])),]
            
            Variable <- dm[[variable_access[[input$variable2]]]]
            Sunshine = Variable
            ggplotly(ggplot(dm,aes(y = Rainfall, x = Sunshine,text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]),group=1)) +
                       geom_smooth(se=FALSE,aes(group = 1), span=1)  +
                       ggtitle("Relationship between Sunshine and Rainfall") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                       theme_par(),tooltip = c("text","x","y"),source = "factor_monthly")  %>% layout(autosize = TRUE, height = 500)  %>% config(displayModeBar = FALSE)
        
          }
          }
        else if(input$variable2 == "Cloud Cover"){
          
          if(input$Month_Year_3 == "Apr 2011"){
            dm = data[data$Year == 2015 & data$Month == 4,]
            Variable <- dm[[variable_access[[input$variable2]]]]
            Cloud_Cover = Variable
            ggplotly(ggplot(dm,aes(y = Rainfall, x = Cloud_Cover,text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]))) +
                       geom_smooth(se=FALSE,aes(group = 1), span=1)  +
                       ggtitle("Relationship between Cloud Cover and Rainfall") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                       theme_par(),tooltip = c("text","x","y"),source = "factor_monthly") %>% layout(autosize = TRUE, height = 500)  %>% config(displayModeBar = FALSE)
          }
          
          else if(input$Month_Year_3 == "Dec 2012"){
            dm = data[data$Year == 2015 & data$Month == 12,]
            Variable <- dm[[variable_access[[input$variable2]]]]
            Cloud_Cover = Variable
            ggplotly(ggplot(dm,aes(y = Rainfall, x = Cloud_Cover,text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]))) +
                       geom_smooth(se=FALSE,aes(group = 1), span=1)  +
                       ggtitle("Relationship between Cloud Cover and Rainfall") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                       theme_par(),tooltip = c("text","x","y"),source = "factor_monthly") %>% layout(autosize = TRUE, height = 500) %>% config(displayModeBar = FALSE)
            
          }
          
          else if(input$Month_Year_3 == "Feb 2013"){
            dm = data[data$Year == 2015 & data$Month == 2,]
            Variable <- dm[[variable_access[[input$variable2]]]]
            Cloud_Cover = Variable
            ggplotly(ggplot(dm,aes(y = Rainfall, x = Cloud_Cover,text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]))) +
                       geom_smooth(se=FALSE,aes(group = 1), span=1)  +
                       ggtitle("Relationship between Cloud Cover and Rainfall") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                       theme_par(),tooltip = c("text","x","y"),source = "factor_monthly") %>% layout(autosize = TRUE, height = 500) %>% config(displayModeBar = FALSE)
            
          }
          
          else{
            dm <- data[data$Year == as.numeric(substr(input$Month_Year_3,5,8)),]
            
            dm <- dm[(dm$Month == as.numeric(c(1:12)[month.name[startsWith(month.name,substr(input$Month_Year_3,1,3))] == month.name])),]
            
            Variable <- dm[[variable_access[[input$variable2]]]]
            Cloud_Cover = Variable
            ggplotly(ggplot(dm,aes(y = Rainfall, x = Cloud_Cover,text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]),group=1)) +
                       geom_smooth(se=FALSE,aes(group = 1), span=1)  +
                       ggtitle("Relationship between Cloud Cover and Rainfall") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                       theme_par(),tooltip = c("text","x","y"),source = "factor_monthly")  %>% layout(autosize = TRUE, height = 500)  %>% config(displayModeBar = FALSE)
        
          }
        }
        else if(input$variable2 == "Humidity"){
          if(input$Month_Year_3 == "Apr 2011"){
            dm = data[data$Year == 2015 & data$Month == 4,]
            Variable <- dm[[variable_access[[input$variable2]]]]
            Humidity = Variable
            ggplotly(ggplot(dm,aes(y = Rainfall, x = Humidity,text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]))) +
                       geom_smooth(se=FALSE,aes(group = 1), span=1)  +
                       ggtitle("Relationship between Humidity and Rainfall") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                       theme_par(),tooltip = c("text","x","y"),source = "factor_monthly") %>% layout(autosize = TRUE, height = 500)  %>% config(displayModeBar = FALSE)
          }
          
          else if(input$Month_Year_3 == "Dec 2012"){
            dm = data[data$Year == 2015 & data$Month == 12,]
            Variable <- dm[[variable_access[[input$variable2]]]]
            Humidity = Variable
            ggplotly(ggplot(dm,aes(y = Rainfall, x = Humidity,text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]))) +
                       geom_smooth(se=FALSE,aes(group = 1), span=1)  +
                       ggtitle("Relationship between Humidity and Rainfall") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                       theme_par(),tooltip = c("text","x","y"),source = "factor_monthly") %>% layout(autosize = TRUE, height = 500) %>% config(displayModeBar = FALSE)
            
          }
          
          else if(input$Month_Year_3 == "Feb 2013"){
            dm = data[data$Year == 2015 & data$Month == 2,]
            Variable <- dm[[variable_access[[input$variable2]]]]
            Humidity = Variable
            ggplotly(ggplot(dm,aes(y = Rainfall, x = Humidity,text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]))) +
                       geom_smooth(se=FALSE,aes(group = 1), span=1)  +
                       ggtitle("Relationship between Humidity and Rainfall") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                       theme_par(),tooltip = c("text","x","y"),source = "factor_monthly") %>% layout(autosize = TRUE, height = 500) %>% config(displayModeBar = FALSE)
            
          }
          else{
            dm <- data[data$Year == as.numeric(substr(input$Month_Year_3,5,8)),]
            
            dm <- dm[(dm$Month == as.numeric(c(1:12)[month.name[startsWith(month.name,substr(input$Month_Year_3,1,3))] == month.name])),]
            
            Variable <- dm[[variable_access[[input$variable2]]]]
            Humidity = Variable
            ggplotly(ggplot(dm,aes(y = Rainfall, x = Humidity,text=paste(input$variable2,":",dm[[variable_access[[input$variable2]]]]),group=1)) +
                       geom_smooth(se=FALSE,aes(group = 1), span=1)  +
                       ggtitle("Relationship between Humidity and Rainfall") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y = "Rainfall(in mm)") +
                       theme_par(),tooltip = c("text","x","y"),source = "factor_monthly")  %>% layout(autosize = TRUE, height = 500)  %>% config(displayModeBar = FALSE)

          }
        }
        
        
       
      })
      
      # Multi density plot yearly
      
      output$factor_density <- renderPlotly({
        
        variable_label <- hash()
        variable_label[["Sunshine"]] <- "Sunshine(in number of hours per day)"
        variable_label[["Cloud9am"]] <- "Cloud Cover(in oktas)"
        variable_label[["Humidity9am"]] <- "Humidity(in %)"
        
        variable_access <- hash()
        variable_access[["Sunshine"]] <- "Sunshine"
        variable_access[["Cloud Cover"]] <- "Cloud9am"
        variable_access[["Humidity"]] <- "Humidity9am"
        
        if(input$variable2 == "Sunshine"){
          if(input$State2 == "All States" | input$State2 == "Cumulative"){
            dm <- data[data$Year == input$Year_3,]
            Variable = dm[[variable_access[[input$variable2]]]]
            Sunshine = Variable
            ggplotly(ggplot(dm,aes(x = Sunshine, group = RainToday, fill = RainToday)) +
                       geom_density(adjust=1.5,alpha = 0.7) +
                       ggtitle("Distribution of Sunshine based on rainfall occurence") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]] , y ="Density of records observed") +
                       theme_par() + theme( legend.title = element_blank()) 
                     ,tooltip = c("x","group"),source = "factor_density_yearly")  %>% layout(autosize = TRUE, height = 500, legend=list(title=list(text='<b> Did it Rain? </b>')))  %>% config(displayModeBar = FALSE)

          }
          else{
            
            dm <- data[data$Year == input$Year_3 & data$State == input$State2,]
            Variable = dm[[variable_access[[input$variable2]]]]
            Sunshine = Variable
            ggplotly(ggplot(dm,aes(x = Sunshine, group = RainToday, fill = RainToday)) +
                       geom_density(adjust=1.5,alpha = 0.7) +
                       ggtitle("Distribution of Sunshine based on rainfall occurence") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y ="Density of records observed") +
                       theme_par() + theme( legend.title = element_blank()) 
                     ,tooltip = c("x","group"),source = "factor_density_yearly")  %>% layout(autosize = TRUE, height = 500, legend=list(title=list(text='<b> Did it Rain? </b>')))  %>% config(displayModeBar = FALSE)

            
            
          }
        }
        else if(input$variable2 == "Cloud Cover"){
          if(input$State2 == "All States" | input$State2 == "Cumulative"){
            dm <- data[data$Year == input$Year_3,]
            Variable = dm[[variable_access[[input$variable2]]]]
            Cloud_Cover = Variable
            ggplotly(ggplot(dm,aes(x = Cloud_Cover, group = RainToday, fill = RainToday)) +
                       geom_density(adjust=1.5,alpha = 0.7) +
                       ggtitle("Distribution of Cloud Cover based on rainfall occurence") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]] , y ="Density of records observed") +
                       theme_par() + theme( legend.title = element_blank()) 
                     ,tooltip = c("x","group"),source = "factor_density_yearly")  %>% layout(autosize = TRUE, height = 500, legend=list(title=list(text='<b> Did it Rain? </b>')))  %>% config(displayModeBar = FALSE)
 
          }
          else{
            
            dm <- data[data$Year == input$Year_3 & data$State == input$State2,]
            Variable = dm[[variable_access[[input$variable2]]]]
            Cloud_Cover = Variable
            ggplotly(ggplot(dm,aes(x = Cloud_Cover, group = RainToday, fill = RainToday)) +
                       geom_density(adjust=1.5,alpha = 0.7) +
                       ggtitle("Distribution of Cloud Cover based on rainfall occurence") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y ="Density of records observed") +
                       theme_par() + theme( legend.title = element_blank()) 
                     ,tooltip = c("x","group"),source = "factor_density_yearly")  %>% layout(autosize = TRUE, height = 500, legend=list(title=list(text='<b> Did it Rain? </b>')))  %>% config(displayModeBar = FALSE)
       
            
            
          }
        }
        else if(input$variable2 == "Humidity"){
          if(input$State2 == "All States" | input$State2 == "Cumulative"){
            dm <- data[data$Year == input$Year_3,]
            Variable = dm[[variable_access[[input$variable2]]]]
            Humidity = Variable
            ggplotly(ggplot(dm,aes(x = Humidity, group = RainToday, fill = RainToday)) +
                       geom_density(adjust=1.5,alpha = 0.7) +
                       ggtitle("Distribution of Humidity based on rainfall occurence") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]] , y ="Density of records observed") +
                       theme_par() + theme( legend.title = element_blank()) 
                     ,tooltip = c("x","group"),source = "factor_density_yearly")  %>% layout(autosize = TRUE, height = 500, legend=list(title=list(text='<b> Did it Rain? </b>')))  %>% config(displayModeBar = FALSE)
      
          }
          else{
            
            dm <- data[data$Year == input$Year_3 & data$State == input$State2,]
            Variable = dm[[variable_access[[input$variable2]]]]
            Humidity = Variable
            ggplotly(ggplot(dm,aes(x = Humidity, group = RainToday, fill = RainToday)) +
                       geom_density(adjust=1.5,alpha = 0.7) +
                       ggtitle("Distribution of Humidity based on rainfall occurence") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y ="Density of records observed") +
                       theme_par() + theme( legend.title = element_blank()) 
                     ,tooltip = c("x","group"),source = "factor_density_yearly")  %>% layout(autosize = TRUE, height = 500, legend=list(title=list(text='<b> Did it Rain? </b>')))  %>% config(displayModeBar = FALSE)
        
            
          }
        }

      })
      
      # Multi density plot monthly
      
      output$factor_density_monthly <- renderPlotly({
      
        
        variable_label <- hash()
        variable_label[["Sunshine"]] <- "Sunshine(in number of hours per day)"
        variable_label[["Cloud9am"]] <- "Cloud Cover(in oktas)"
        variable_label[["Humidity9am"]] <- "Humidity(in %)"
        
        variable_access <- hash()
        variable_access[["Sunshine"]] <- "Sunshine"
        variable_access[["Cloud Cover"]] <- "Cloud9am"
        variable_access[["Humidity"]] <- "Humidity9am"
        
        if(input$variable2 == "Sunshine"){
          if(input$Month_Year_3 == "Apr 2011"){
            dm = data[data$Year == 2015 & data$Month == 4,]
            Variable <- dm[[variable_access[[input$variable2]]]]
            Sunshine = Variable
            ggplotly(ggplot(dm,aes(x = Sunshine, group = RainToday, fill = RainToday)) +
                       geom_density(adjust=1.5,alpha = 0.7) +
                       ggtitle("Distribution of Sunshine based on rainfall occurence") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y ="Density of records observed") +
                       theme_par() + theme( legend.title = element_blank()) 
                     ,tooltip = c("x","group"),source= "factor_density_monthly")   %>% layout(autosize = TRUE, height = 500, legend=list(title=list(text='<b> Did it Rain? </b>')))  %>% config(displayModeBar = FALSE)
          }
          
          else if(input$Month_Year_3 == "Dec 2012"){
            dm = data[data$Year == 2015 & data$Month == 12,]
            Variable <- dm[[variable_access[[input$variable2]]]]
            Sunshine = Variable
            ggplotly(ggplot(dm,aes(x = Sunshine, group = RainToday, fill = RainToday)) +
                       geom_density(adjust=1.5,alpha = 0.7) +
                       ggtitle("Distribution of Sunshine based on rainfall occurence") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y ="Density of records observed") +
                       theme_par() + theme( legend.title = element_blank()) 
                     ,tooltip = c("x","group"),source= "factor_density_monthly")   %>% layout(autosize = TRUE, height = 500, legend=list(title=list(text='<b> Did it Rain? </b>')))  %>% config(displayModeBar = FALSE)
          }
          
          else if(input$Month_Year_3 == "Feb 2013"){
            dm = data[data$Year == 2015 & data$Month == 2,]
            Variable <- dm[[variable_access[[input$variable2]]]]
            Sunshine = Variable
            ggplotly(ggplot(dm,aes(x = Sunshine, group = RainToday, fill = RainToday)) +
                       geom_density(adjust=1.5,alpha = 0.7) +
                       ggtitle("Distribution of Sunshine based on rainfall occurence") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y ="Density of records observed") +
                       theme_par() + theme( legend.title = element_blank()) 
                     ,tooltip = c("x","group"),source= "factor_density_monthly")   %>% layout(autosize = TRUE, height = 500, legend=list(title=list(text='<b> Did it Rain? </b>')))  %>% config(displayModeBar = FALSE)
          }
          
          else{
            dm <- data[(data$Year == as.numeric(substr(input$Month_Year_3,5,8))),]
            dm <- dm[(dm$Month == as.numeric(c(1:12)[month.name[startsWith(month.name,substr(input$Month_Year_3,1,3))] == month.name])),]
            Variable = dm[[variable_access[[input$variable2]]]]
            Sunshine = Variable
            ggplotly(ggplot(dm,aes(x = Sunshine, group = RainToday, fill = RainToday)) +
                       geom_density(adjust=1.5,alpha = 0.7) +
                       ggtitle("Distribution of Sunshine based on rainfall occurence") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y ="Density of records observed") +
                       theme_par() + theme( legend.title = element_blank()) 
                     ,tooltip = c("x","group"),source= "factor_density_monthly")  %>% layout(autosize = TRUE, height = 500, legend=list(title=list(text='<b> Did it Rain? </b>')))  %>% config(displayModeBar = FALSE)

          }
        }
        else if(input$variable2 == "Cloud Cover"){
          if(input$Month_Year_3 == "Apr 2011"){
            dm = data[data$Year == 2015 & data$Month == 4,]
            Variable <- dm[[variable_access[[input$variable2]]]]
            Cloud_Cover = Variable
            ggplotly(ggplot(dm,aes(x = Cloud_Cover, group = RainToday, fill = RainToday)) +
                       geom_density(adjust=1.5,alpha = 0.7) +
                       ggtitle("Distribution of Cloud Cover based on rainfall occurence") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y ="Density of records observed") +
                       theme_par() + theme( legend.title = element_blank()) 
                     ,tooltip = c("x","group"),source= "factor_density_monthly")   %>% layout(autosize = TRUE, height = 500, legend=list(title=list(text='<b> Did it Rain? </b>')))  %>% config(displayModeBar = FALSE)
          }
          
          else if(input$Month_Year_3 == "Dec 2012"){
            dm = data[data$Year == 2015 & data$Month == 12,]
            Variable <- dm[[variable_access[[input$variable2]]]]
            Cloud_Cover = Variable
            ggplotly(ggplot(dm,aes(x = Cloud_Cover, group = RainToday, fill = RainToday)) +
                       geom_density(adjust=1.5,alpha = 0.7) +
                       ggtitle("Distribution of Cloud Cover based on rainfall occurence") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y ="Density of records observed") +
                       theme_par() + theme( legend.title = element_blank()) 
                     ,tooltip = c("x","group"),source= "factor_density_monthly")   %>% layout(autosize = TRUE, height = 500, legend=list(title=list(text='<b> Did it Rain? </b>')))  %>% config(displayModeBar = FALSE)
          }
          
          else if(input$Month_Year_3 == "Feb 2013"){
            dm = data[data$Year == 2015 & data$Month == 2,]
            Variable <- dm[[variable_access[[input$variable2]]]]
            Cloud_Cover = Variable
            ggplotly(ggplot(dm,aes(x = Cloud_Cover, group = RainToday, fill = RainToday)) +
                       geom_density(adjust=1.5,alpha = 0.7) +
                       ggtitle("Distribution of Cloud Cover based on rainfall occurence") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y ="Density of records observed") +
                       theme_par() + theme( legend.title = element_blank()) 
                     ,tooltip = c("x","group"),source= "factor_density_monthly")   %>% layout(autosize = TRUE, height = 500, legend=list(title=list(text='<b> Did it Rain? </b>')))  %>% config(displayModeBar = FALSE)
          }
          
          else{
            dm <- data[(data$Year == as.numeric(substr(input$Month_Year_3,5,8))),]
            dm <- dm[(dm$Month == as.numeric(c(1:12)[month.name[startsWith(month.name,substr(input$Month_Year_3,1,3))] == month.name])),]
            Variable = dm[[variable_access[[input$variable2]]]]
            Cloud_Cover = Variable
            ggplotly(ggplot(dm,aes(x = Cloud_Cover, group = RainToday, fill = RainToday)) +
                       geom_density(adjust=1.5,alpha = 0.7) +
                       ggtitle("Distribution of Cloud Cover based on rainfall occurence") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y ="Density of records observed") +
                       theme_par() + theme( legend.title = element_blank()) 
                     ,tooltip = c("x","group"),source= "factor_density_monthly")  %>% layout(autosize = TRUE, height = 500, legend=list(title=list(text='<b> Did it Rain? </b>')))  %>% config(displayModeBar = FALSE)
      
          }
        }
        else if(input$variable2 == "Humidity"){
          if(input$Month_Year_3 == "Apr 2011"){
            dm = data[data$Year == 2015 & data$Month == 4,]
            Variable <- dm[[variable_access[[input$variable2]]]]
            Humidity = Variable
            ggplotly(ggplot(dm,aes(x = Humidity, group = RainToday, fill = RainToday)) +
                       geom_density(adjust=1.5,alpha = 0.7) +
                       ggtitle("Distribution of Humidity based on rainfall occurence") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y ="Density of records observed") +
                       theme_par() + theme( legend.title = element_blank()) 
                     ,tooltip = c("x","group"),source= "factor_density_monthly")   %>% layout(autosize = TRUE, height = 500, legend=list(title=list(text='<b> Did it Rain? </b>')))  %>% config(displayModeBar = FALSE)
          }
          
          else if(input$Month_Year_3 == "Dec 2012"){
            dm = data[data$Year == 2015 & data$Month == 12,]
            Variable <- dm[[variable_access[[input$variable2]]]]
            Humidity = Variable
            ggplotly(ggplot(dm,aes(x = Humidity, group = RainToday, fill = RainToday)) +
                       geom_density(adjust=1.5,alpha = 0.7) +
                       ggtitle("Distribution of Humidity based on rainfall occurence") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y ="Density of records observed") +
                       theme_par() + theme( legend.title = element_blank()) 
                     ,tooltip = c("x","group"),source= "factor_density_monthly")   %>% layout(autosize = TRUE, height = 500, legend=list(title=list(text='<b> Did it Rain? </b>')))  %>% config(displayModeBar = FALSE)
          }
          
          else if(input$Month_Year_3 == "Feb 2013"){
            dm = data[data$Year == 2015 & data$Month == 2,]
            Variable <- dm[[variable_access[[input$variable2]]]]
            Humidity = Variable
            ggplotly(ggplot(dm,aes(x = Humidity, group = RainToday, fill = RainToday)) +
                       geom_density(adjust=1.5,alpha = 0.7) +
                       ggtitle("Distribution of Humidity based on rainfall occurence") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y ="Density of records observed") +
                       theme_par() + theme( legend.title = element_blank()) 
                     ,tooltip = c("x","group"),source= "factor_density_monthly")   %>% layout(autosize = TRUE, height = 500, legend=list(title=list(text='<b> Did it Rain? </b>')))  %>% config(displayModeBar = FALSE)
          }
          
          else{
            dm <- data[(data$Year == as.numeric(substr(input$Month_Year_3,5,8))),]
            dm <- dm[(dm$Month == as.numeric(c(1:12)[month.name[startsWith(month.name,substr(input$Month_Year_3,1,3))] == month.name])),]
            Variable = dm[[variable_access[[input$variable2]]]]
            Humidity = Variable
            ggplotly(ggplot(dm,aes(x = Humidity, group = RainToday, fill = RainToday)) +
                       geom_density(adjust=1.5,alpha = 0.7) +
                       ggtitle("Distribution of Humidity based on rainfall occurence") +
                       labs(x = variable_label[[variable_access[[input$variable2]]]], y ="Density of records observed") +
                       theme_par() + theme( legend.title = element_blank()) 
                     ,tooltip = c("x","group"),source= "factor_density_monthly")  %>% layout(autosize = TRUE, height = 500, legend=list(title=list(text='<b> Did it Rain? </b>')))  %>% config(displayModeBar = FALSE)
          
          }
        }
        
        
          
       
      })

 
  # Contour Map for the decade overall
      
  output$ggiraph_contour_whole <- renderGirafe({
    
    rain_sum <- rowSums(r_2007_df[,1:12]) +
      rowSums(r_2008_df[,1:12]) +
      rowSums(r_2009_df[,1:12]) +
      rowSums(r_2010_df[,1:12]) +
      rowSums(r_2011_df[,1:12]) +
      rowSums(r_2012_df[,1:12]) +
      rowSums(r_2013_df[,1:12]) +
      rowSums(r_2014_df[,1:12]) +
      rowSums(r_2015_df[,1:12]) +
      rowSums(r_2016_df[,1:12]) +
      rowSums(r_2017_df[,1:12])
    
    
    girafe(ggobj = ggplot(r_2007_df,aes(x=x,y=y,z=rain_sum)) + 
             theme_void() + 
             geom_contour_filled_interactive(aes(data_id = stat(level),
                                                fill = stat(level),
                                tooltip = paste("Rainfall Range", stat(level))), bins = 9)
           + labs(fill = "Rainfall Range(in mm)")
            + scale_fill_brewer(palette = "Blues")
           ,
           options = list(opts_selection(type = "single", only_shiny = TRUE))
           )
    
  })
  
  
  
  # Reactive values for current state yearly and monthly
  
  current_state <- reactiveVal()
  
  current_state_monthly <- reactiveVal()
  
  observeEvent(input$choropleth_click,{
    
    exp <- r_2007_df %>% dplyr::group_by(State) %>% dplyr::summarise(as.integer(min(x)),as.integer(max(x)),as.integer(min(y)),as.integer(max(y))) %>% dplyr::rename(xmin = 'as.integer(min(x))',ymin ='as.integer(min(y))', xmax = 'as.integer(max(x))',ymax ='as.integer(max(y))' ) %>% na.omit(NA)
    
    if(nrow(exp[(input$choropleth_click$lng <= exp$xmax & input$choropleth_click$lng >= exp$xmin) &(input$choropleth_click$lat <= exp$ymax & input$choropleth_click$lat >= exp$ymin),"State"]) == 1){
      current_state(exp[(input$choropleth_click$lng <= exp$xmax & input$choropleth_click$lng >= exp$xmin) &(input$choropleth_click$lat <= exp$ymax & input$choropleth_click$lat >= exp$ymin),"State"])
    }
    
    else if(nrow(exp[(input$choropleth_click$lng <= exp$xmax & input$choropleth_click$lng >= exp$xmin) &(input$choropleth_click$lat <= exp$ymax & input$choropleth_click$lat >= exp$ymin),"State"]) == 2){
      shinyalert("Oops!", "Please click on the state again", type = "warning")
    }
    else{
      current_state(NULL)
    }
    print(current_state())
    print(input$choropleth_click)
  })
  
  # Choropleth map click event observer
  observeEvent(input$choropleth_monthly_click,{
    
    exp <- r_2007_df %>% dplyr::group_by(State) %>% dplyr::summarise(as.integer(min(x)),as.integer(max(x)),as.integer(min(y)),as.integer(max(y))) %>% dplyr::rename(xmin = 'as.integer(min(x))',ymin ='as.integer(min(y))', xmax = 'as.integer(max(x))',ymax ='as.integer(max(y))' ) %>% na.omit(NA)
    
    if(nrow(exp[(input$choropleth_monthly_click$lng <= exp$xmax & input$choropleth_monthly_click$lng >= exp$xmin) &(input$choropleth_monthly_click$lat <= exp$ymax & input$choropleth_monthly_click$lat >= exp$ymin),"State"]) == 1){
      current_state_monthly(exp[(input$choropleth_monthly_click$lng <= exp$xmax & input$choropleth_monthly_click$lng >= exp$xmin) &(input$choropleth_monthly_click$lat <= exp$ymax & input$choropleth_monthly_click$lat >= exp$ymin),"State"])
    }
    
    else if(nrow(exp[(input$choropleth_monthly_click$lng <= exp$xmax & input$choropleth_monthly_click$lng >= exp$xmin) &(input$choropleth_monthly_click$lat <= exp$ymax & input$choropleth_monthly_click$lat >= exp$ymin),"State"]) == 2){
      shinyalert("Oops!", "Please click on the state again", type = "warning")
    }
    else{
      current_state_monthly(NULL)
    }
    print(current_state_monthly())
    print(input$choropleth_monthly_click)
  })
  
  observeEvent(input$Year_1b, {
    
   # Choropleth Map Yearly Rainfall
      
   output$choropleth <- renderLeaflet({
     if(input$Year_1b == 2007){
       choropleth_df = r_2007_df
     }
     else if(input$Year_1b == 2008){
       choropleth_df = r_2008_df
     }
     else if(input$Year_1b == 2009){
       choropleth_df = r_2009_df
     }
     else if(input$Year_1b == 2010){
       choropleth_df = r_2010_df
     }
     else if(input$Year_1b == 2011){
       choropleth_df = r_2011_df
     }
     else if(input$Year_1b == 2012){
       choropleth_df = r_2012_df
     }
     else if(input$Year_1b == 2013){
       choropleth_df = r_2013_df
     }
     else if(input$Year_1b == 2014){
       choropleth_df = r_2014_df
     }
     else if(input$Year_1b == 2015){
       choropleth_df = r_2015_df
     }
     else if(input$Year_1b == 2016){
       choropleth_df = r_2016_df
     }
     else if(input$Year_1b == 2017){
       choropleth_df = r_2017_df
     }
     
     pal <- colorNumeric("Blues", domain=geo_join(ozmap_states, choropleth_df %>% dplyr::group_by(State) %>% summarise(sum(Total)) %>% dplyr::rename(Rainfall = 'sum(Total)') %>% na.omit(NA), "NAME", "State")$Rainfall)
     labels <- sprintf(
       "<strong>%s</strong><br/>%s mm",
       geo_join(ozmap_states, choropleth_df %>% dplyr::group_by(State) %>% summarise(sum(Total)) %>% dplyr::rename(Rainfall = 'sum(Total)') %>% na.omit(NA), "NAME", "State")$NAME, format(geo_join(ozmap_states, choropleth_df %>% dplyr::group_by(State) %>% summarise(sum(Total)) %>% dplyr::rename(Rainfall = 'sum(Total)') %>% na.omit(NA), "NAME", "State")$Rainfall,big.mark=",",scientific = FALSE)
     ) %>% lapply(htmltools::HTML)
     leaflet() %>%
       setView(lng = 133.27515449999999, lat = -26.853387500000004,   zoom = 3) %>%
       addProviderTiles("Stamen.Watercolor") %>%
       addPolygons(data = geo_join(ozmap_states, choropleth_df %>% dplyr::group_by(State) %>% summarise(sum(Total)) %>% dplyr::rename(Rainfall = 'sum(Total)') %>% na.omit(NA), "NAME", "State") , 
                   fillColor = ~pal(geo_join(ozmap_states, choropleth_df %>% dplyr::group_by(State) %>% summarise(sum(Total)) %>% dplyr::rename(Rainfall = 'sum(Total)') %>% na.omit(NA), "NAME", "State")$Rainfall), 
                   fillOpacity = 0.9,
                   weight = 2,
                   opacity = 1,
                   smoothFactor = 0.2, 
                   color = "white",
                   dashArray = "3",
                   highlight = highlightOptions(
                     weight = 4,
                     color = "#666",
                     dashArray = "",
                     fillOpacity = 0.9,
                     bringToFront = TRUE),
                   label = labels
       ) %>%
       addLabelOnlyMarkers(
         lng = 92.23447, lat = -47.98992,
         label = "Click anywhere outside the Australian borders to reset plot on the right",
         labelOptions = labelOptions(noHide = TRUE, textsize = "15px")) %>%
    leaflet::addLegend(pal = pal, 
                       values = geo_join(ozmap_states, choropleth_df %>% dplyr::group_by(State) %>% summarise(sum(Total)) %>% dplyr::rename(Rainfall = 'sum(Total)') %>% na.omit(NA), "NAME", "State")$Rainfall, 
                       position = "topright",
                       title = "Rainfall(in mm)")
     
   })
   
   
   # Loccation-wise bar chart yearly
   
   output$locationbar <- renderPlotly({
     if(input$Year_1b == 2007){
       location_df = r_2007_df
     }
     else if(input$Year_1b == 2008){
       location_df = r_2008_df
     }
     else if(input$Year_1b == 2009){
       location_df = r_2009_df
     }
     else if(input$Year_1b == 2010){
       location_df = r_2010_df
     }
     else if(input$Year_1b == 2011){
       location_df = r_2011_df
     }
     else if(input$Year_1b == 2012){
       location_df = r_2012_df
     }
     else if(input$Year_1b == 2013){
       location_df = r_2013_df
     }
     else if(input$Year_1b == 2014){
       location_df = r_2014_df
     }
     else if(input$Year_1b == 2015){
       location_df = r_2015_df
     }
     else if(input$Year_1b == 2016){
       location_df = r_2016_df
     }
     else if(input$Year_1b == 2017){
       location_df = r_2017_df
     }
     
     
     
     
     if(is.null(current_state())){
       location_wise_rainfall <- location_df[!is.na(location_df$Location),]

       ggplotly(location_wise_rainfall %>%
                  ggplot(aes(x=Location,y=Total,fill=State,text = paste("Rainfall(in mm): ",round(Total,digits = 2)))) + 
                  geom_col(width = 0.5) +
                  labs(title="Location-wise total rainfall", y = "Total Rainfall(in mm)") + theme_bw() +
                  theme(axis.text.x = element_text(angle = 45)) +
                  scale_fill_brewer(palette = "Accent"), tooltip = c("x","fill","text") , source = "locationbar_overall") %>% layout(height= 500, width = 700, legend = list(x = 0.35, y = -0.4, orientation = "h")) %>% config(displayModeBar = FALSE)      
     }
     else{
       
       location_wise_rainfall <- location_df[!is.na(location_df$Location) & location_df$State == as.character(current_state()),]

       ggplotly(location_wise_rainfall %>%
                  ggplot(aes(x=Location,y=Total,text = paste("Rainfall(in mm): ",round(Total,digits = 2)))) + 
                  geom_col(fill="cornflowerblue",color="black",width = 0.5) +
                  labs(title="Location-wise total rainfall", y = "Total Rainfall(in mm)") + theme_bw() +
                  theme(axis.text.x = element_text(angle = 45)) +
                  scale_fill_brewer(palette = "Accent") ,  tooltip = c("x","fill","text") , source = "locationbar_state")  %>% layout(height= 500, width = 700, legend = list(x = 0.35, y = -0.4, orientation = "h"))  %>% config(displayModeBar = FALSE)      
       
       
       
     }
   })
  })
  
  observeEvent(input$Month_Year_1b, {
    
    # Choropleth Map Monthly Rainfall
    
    output$choropleth_monthly <- renderLeaflet({
      if(isTRUE(grepl("2007",input$Month_Year_1b,fixed = TRUE))){
        choropleth_df = r_2007_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2008",input$Month_Year_1b,fixed = TRUE))){
        choropleth_df = r_2008_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2009",input$Month_Year_1b,fixed = TRUE))){
        choropleth_df = r_2009_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2010",input$Month_Year_1b,fixed = TRUE))){
        choropleth_df = r_2010_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2011",input$Month_Year_1b,fixed = TRUE))){
        choropleth_df = r_2011_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2012",input$Month_Year_1b,fixed = TRUE))){
        choropleth_df = r_2012_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2013",input$Month_Year_1b,fixed = TRUE))){
        choropleth_df = r_2013_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2014",input$Month_Year_1b,fixed = TRUE))){
        choropleth_df = r_2014_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2015",input$Month_Year_1b,fixed = TRUE))){
        choropleth_df = r_2015_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2016",input$Month_Year_1b,fixed = TRUE))){
        choropleth_df = r_2016_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2017",input$Month_Year_1b,fixed = TRUE))){
        choropleth_df = r_2017_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      
      pal <- colorNumeric("Blues", domain=geo_join(ozmap_states, choropleth_df %>% dplyr::group_by(State) %>% summarise(sum(.data[[month]])) %>% dplyr::rename(Rainfall = paste('sum(.data[["',month,'"]])',sep = "")) %>% na.omit(NA), "NAME", "State")$Rainfall)
      labels <- sprintf(
        "<strong>%s</strong><br/>%s mm",
        geo_join(ozmap_states, choropleth_df %>% dplyr::group_by(State) %>% summarise(sum(.data[[month]])) %>% dplyr::rename(Rainfall = paste('sum(.data[["',month,'"]])',sep = "")) %>% na.omit(NA), "NAME", "State")$NAME, format(geo_join(ozmap_states, choropleth_df %>% dplyr::group_by(State) %>% summarise(sum(.data[[month]])) %>% dplyr::rename(Rainfall = paste('sum(.data[["',month,'"]])',sep = "")) %>% na.omit(NA), "NAME", "State")$Rainfall,big.mark=",",scientific = FALSE)
      ) %>% lapply(htmltools::HTML)
      leaflet() %>%
        setView(lng = 133.27515449999999, lat = -26.853387500000004,   zoom = 3) %>%
        addProviderTiles("Stamen.Watercolor") %>%
        addPolygons(data = geo_join(ozmap_states, choropleth_df %>% dplyr::group_by(State) %>% summarise(sum(.data[[month]])) %>% dplyr::rename(Rainfall = paste('sum(.data[["',month,'"]])',sep = "")) %>% na.omit(NA), "NAME", "State") , 
                    fillColor = ~pal(geo_join(ozmap_states, choropleth_df %>% dplyr::group_by(State) %>% summarise(sum(.data[[month]])) %>% dplyr::rename(Rainfall = paste('sum(.data[["',month,'"]])',sep = "")) %>% na.omit(NA), "NAME", "State")$Rainfall), 
                    fillOpacity = 0.9,
                    weight = 2,
                    opacity = 1,
                    smoothFactor = 0.2, 
                    color = "white",
                    dashArray = "3",
                    highlight = highlightOptions(
                      weight = 4,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.9,
                      bringToFront = TRUE),
                    label = labels
        ) %>%
        addLabelOnlyMarkers(
          lng = 92.23447, lat = -47.98992,
          label = "Click anywhere outside the Australian borders to reset plot on the right",
          labelOptions = labelOptions(noHide = TRUE, textsize = "15px")) %>%
        leaflet::addLegend(pal = pal, 
                           values = geo_join(ozmap_states, choropleth_df %>% dplyr::group_by(State) %>% summarise(sum(.data[[month]])) %>% dplyr::rename(Rainfall = paste('sum(.data[["',month,'"]])',sep = "")) %>% na.omit(NA), "NAME", "State")$Rainfall, 
                           position = "topright",
                           title = "Rainfall(in mm)")
      
    }) 
    
    # Location-wise bar chart monthly rainfall
    
    output$locationbar_monthly <- renderPlotly({
      if(isTRUE(grepl("2007",input$Month_Year_1b,fixed = TRUE))){
        location_df = r_2007_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2008",input$Month_Year_1b,fixed = TRUE))){
        location_df = r_2008_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2009",input$Month_Year_1b,fixed = TRUE))){
        location_df = r_2009_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2010",input$Month_Year_1b,fixed = TRUE))){
        location_df = r_2010_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2011",input$Month_Year_1b,fixed = TRUE))){
        location_df = r_2011_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2012",input$Month_Year_1b,fixed = TRUE))){
        location_df = r_2012_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2013",input$Month_Year_1b,fixed = TRUE))){
        location_df = r_2013_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2014",input$Month_Year_1b,fixed = TRUE))){
        location_df = r_2014_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2015",input$Month_Year_1b,fixed = TRUE))){
        location_df = r_2015_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2016",input$Month_Year_1b,fixed = TRUE))){
        location_df = r_2016_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      else if(isTRUE(grepl("2017",input$Month_Year_1b,fixed = TRUE))){
        location_df = r_2017_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1b,1,3))]
      }
      
      
      if(is.null(current_state_monthly())){
        location_wise_rainfall <- location_df[!is.na(location_df$Location),]
        
        ggplotly(location_wise_rainfall %>%
                   ggplot(aes(x=Location,y=location_wise_rainfall[[month]],fill=State,text = paste("Rainfall(in mm): ",round(location_wise_rainfall[[month]],digits = 2)))) + 
                   geom_col(width = 0.5) +
                   labs(title="Location-wise total rainfall", y = "Total Rainfall(in mm)") + theme_bw() +
                   theme(axis.text.x = element_text(angle = 45)) +
                   scale_fill_brewer(palette = "Accent"), tooltip = c("x","fill","text") ,source = "locationbar_monthly_overall")  %>% layout(height= 500, width = 700, legend = list(x = 0.35, y = -0.4 , orientation = "h"))    %>% config(displayModeBar = FALSE)
        
      }
      else{
        
        location_wise_rainfall <- location_df[!is.na(location_df$Location) & location_df$State == as.character(current_state_monthly()),]
      
        ggplotly(location_wise_rainfall %>%
                   ggplot(aes(x=Location,y=location_wise_rainfall[[month]],text = paste("Rainfall(in mm): ",round(location_wise_rainfall[[month]],digits = 2)))) + 
                   geom_col(fill="cornflowerblue",color="black",width = 0.5) +
                   labs(title="Location-wise total rainfall", y = "Total Rainfall(in mm)") + theme_bw() +
                   theme(axis.text.x = element_text(angle = 45)) +
                   # coord_flip() +
                   scale_fill_brewer(palette = "Accent") ,  tooltip = c("x","fill","text"),source = "locationbar_monthly_state")  %>% layout(height= 500, width = 700, legend = list(x = 0.35, y = -0.4, orientation = "h"))   %>% config(displayModeBar = FALSE)
        
        
        
      }
    })
    
  })
  
  

  
  
  observeEvent(input$Year_2, {
    
    # Seasons Bar Chart
    
    output$season <- renderPlotly({
      
      if(input$State1 == "All States") {
      
      if(input$Year_2 == 2007){
        season_data = r_2007_df
      }
      else if(input$Year_2 == 2008){
        season_data = r_2008_df
      }
      else if(input$Year_2 == 2009){
        season_data = r_2009_df
      }
      else if(input$Year_2 == 2010){
        season_data = r_2010_df
      }
      else if(input$Year_2 == 2011){
        season_data = r_2011_df
      }
      else if(input$Year_2 == 2012){
        season_data = r_2012_df
      }
      else if(input$Year_2 == 2013){
        season_data = r_2013_df
      }
      else if(input$Year_2 == 2014){
        season_data = r_2014_df
      }
      else if(input$Year_2 == 2015){
        season_data = r_2015_df
      }
      else if(input$Year_2 == 2016){
        season_data = r_2016_df
      }
      else if(input$Year_2 == 2017){
        season_data = r_2017_df
      }
   
      Total_Rainfall = c(sum(season_data$Summer),sum(season_data$Autumn),sum(season_data$Winter),sum(season_data$Spring))
      ggplotly(  ggplot(as.data.frame(c(sum(season_data$Summer),sum(season_data$Autumn),sum(season_data$Winter),sum(season_data$Spring))),aes(x=c(1,2,3,4),y = Total_Rainfall ,text=paste("Season: ",c("Summer","Autumn","Winter","Spring")),group =1)) +
                   geom_bar(stat = "identity",color = "black",fill=c("#CA0020","#2da4e9","#0e1a88","#F4A582"),width = 0.2) +
                   geom_path(size = 1.5) +
                   theme(
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(fill = "transparent",colour = NA),
                     plot.background = element_rect(fill = "transparent",colour = NA),
                     panel.border = element_rect(colour = "black", fill=NA)
                   ) +
                   scale_y_continuous(labels = scales::comma) +
                   labs(title=paste("Total rainfall per season in year",as.character(input$Year_2)),x="Season",y = "Total Rainfall(in mm)") +
                   scale_x_continuous(breaks=1:4,labels = c("Summer","Autumn","Winter","Spring"))
                 , source = "season",tooltip = c("text","y")) %>% layout(height= 500, width = 650)  %>% config(displayModeBar = FALSE)
      }
      else{
        
        if(input$Year_2 == 2007){
          season_data = r_2007_df
        }
        else if(input$Year_2 == 2008){
          season_data = r_2008_df
        }
        else if(input$Year_2 == 2009){
          season_data = r_2009_df
        }
        else if(input$Year_2 == 2010){
          season_data = r_2010_df
        }
        else if(input$Year_2 == 2011){
          season_data = r_2011_df
        }
        else if(input$Year_2 == 2012){
          season_data = r_2012_df
        }
        else if(input$Year_2 == 2013){
          season_data = r_2013_df
        }
        else if(input$Year_2 == 2014){
          season_data = r_2014_df
        }
        else if(input$Year_2 == 2015){
          season_data = r_2015_df
        }
        else if(input$Year_2 == 2016){
          season_data = r_2016_df
        }
        else if(input$Year_2 == 2017){
          season_data = r_2017_df
        }
        season_data =  season_data %>% dplyr::filter(State == input$State1)
        Total_Rainfall = c(sum(season_data$Summer),sum(season_data$Autumn),sum(season_data$Winter),sum(season_data$Spring))
        ggplotly(  ggplot(as.data.frame(c(sum(season_data$Summer),sum(season_data$Autumn),sum(season_data$Winter),sum(season_data$Spring))),aes(x=c(1,2,3,4),y = Total_Rainfall ,text=paste("Season: ",c("Summer","Autumn","Winter","Spring")),group =1)) +
                     geom_bar(stat = "identity",color = "black",fill=c("#CA0020","#2da4e9","#0e1a88","#F4A582"),width = 0.2) +
                     geom_path(size = 1.5) +
                     theme(
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_rect(fill = "transparent",colour = NA),
                       plot.background = element_rect(fill = "transparent",colour = NA),
                       panel.border = element_rect(colour = "black", fill=NA)
                     ) +
                     scale_y_continuous(labels = scales::comma) +
                     labs(title=paste("Total rainfall per season in year",as.character(input$Year_2)),x="Season",y = "Total Rainfall(in mm)") +
                     scale_x_continuous(breaks=1:4,labels = c("Summer","Autumn","Winter","Spring"))
                   , source = "season",tooltip = c("text","y")) %>% layout(height= 500, width = 650)  %>% config(displayModeBar = FALSE)
        
        
        
        
      }
      })
    
    # Monthly Lollipop Chart
    
    output$monthly <- renderPlotly({
      month_range <- season_month()
      
      if(length(month_range) == 12){
        fill_value = c("#CA0020","#2da4e9","#2da4e9","#2da4e9","#0e1a88","#0e1a88","#0e1a88","#F4A582","#F4A582","#F4A582","#CA0020","#CA0020")
      }
      else if(month_range == c(3:5)){
        fill_value = "#2da4e9"
      }
      else if(month_range == c(6:8)){
        fill_value = "#0e1a88"
      }
      else if(month_range == c(9:11)){
        fill_value = "#F4A582"
      }
      else{
        fill_value = "#CA0020"
      }
      
      if(input$State1 == "All States"){
      
      if(input$Year_2 == 2007){
        month_data = r_2007_df
      }
      else if(input$Year_2 == 2008){
        month_data = r_2008_df
      }
      else if(input$Year_2 == 2009){
        month_data = r_2009_df
      }
      else if(input$Year_2 == 2010){
        month_data = r_2010_df
      }
      else if(input$Year_2 == 2011){
        month_data = r_2011_df
      }
      else if(input$Year_2 == 2012){
        month_data = r_2012_df
      }
      else if(input$Year_2 == 2013){
        month_data = r_2013_df
      }
      else if(input$Year_2 == 2014){
        month_data = r_2014_df
      }
      else if(input$Year_2 == 2015){
        month_data = r_2015_df
      }
      else if(input$Year_2 == 2016){
        month_data = r_2016_df
      }
      else if(input$Year_2 == 2017){
        month_data = r_2017_df
      }
      Total_Rainfall = c(colSums(month_data[,season_month()]))
      ggplotly(  ggplot(as.data.frame(c(colSums(month_data[,season_month()]))),aes(x=season_month(),y = Total_Rainfall,text = paste("Month: ",month.name[season_month()]),group =1)) +
                
                   geom_segment( aes(x=season_month(), xend=season_month(), y=0, yend=c(colSums(month_data[,season_month()]))), color="grey") +
                   geom_point( color=fill_value, size=4) +
                   geom_path(size = 1.5) +
                   theme(
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(fill = "transparent",colour = NA),
                     plot.background = element_rect(fill = "transparent",colour = NA),
                     panel.border = element_rect(colour = "black", fill=NA),
                     axis.text.x = element_text(angle = 45)
                   ) +
                   scale_y_continuous(labels = scales::comma) +
                   labs(title=paste("Total rainfall per month in year",as.character(input$Year_2)),x="Month",y = "Total Rainfall(in mm)") +
                   scale_x_continuous(breaks=season_month(),labels = as.vector(colnames(r_2007_df[,season_month()])))
      ,tooltip = c("text","y"),source = "month") %>% layout(height= 500, width = 650) %>% config(displayModeBar = FALSE)
    
      }
      else{
        
        if(input$Year_2 == 2007){
          month_data = r_2007_df
        }
        else if(input$Year_2 == 2008){
          month_data = r_2008_df
        }
        else if(input$Year_2 == 2009){
          month_data = r_2009_df
        }
        else if(input$Year_2 == 2010){
          month_data = r_2010_df
        }
        else if(input$Year_2 == 2011){
          month_data = r_2011_df
        }
        else if(input$Year_2 == 2012){
          month_data = r_2012_df
        }
        else if(input$Year_2 == 2013){
          month_data = r_2013_df
        }
        else if(input$Year_2 == 2014){
          month_data = r_2014_df
        }
        else if(input$Year_2 == 2015){
          month_data = r_2015_df
        }
        else if(input$Year_2 == 2016){
          month_data = r_2016_df
        }
        else if(input$Year_2 == 2017){
          month_data = r_2017_df
        }
        month_data = month_data %>% dplyr::filter(State == input$State1)
        Total_Rainfall = c(colSums(month_data[,season_month()]))
        ggplotly(  ggplot(as.data.frame(c(colSums(month_data[,season_month()]))),aes(x=season_month(),y = Total_Rainfall,text = paste("Month: ",month.name[season_month()]),group =1)) +
                     geom_segment( aes(x=season_month(), xend=season_month(), y=0, yend=c(colSums(month_data[,season_month()]))), color="grey") +
                     geom_point( color=fill_value, size=4) +
                     geom_path(size = 1.5) +
                     theme(
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_rect(fill = "transparent",colour = NA),
                       plot.background = element_rect(fill = "transparent",colour = NA),
                       panel.border = element_rect(colour = "black", fill=NA),
                       axis.text.x = element_text(angle = 45)
                     ) +
                     scale_y_continuous(labels = scales::comma) +
                     labs(title=paste("Total rainfall per month in year",as.character(input$Year_2)),x="Month",y = "Total Rainfall(in mm)") +
                     scale_x_continuous(breaks=season_month(),labels = as.vector(colnames(r_2007_df[,season_month()])))
                   ,tooltip = c("text","y"),source = "month") %>% layout(height= 500, width = 650)  %>% config(displayModeBar = FALSE)
        
      }
      })
    
    })
  
  
  observeEvent(input$Year_1a, {
      
      # Contour Map Yearly  
        
      output$ggiraph_contour <- renderGirafe({
        if(input$Year_1a == 2007){
          ggiraph_countour = r_2007_df
        }
        else if(input$Year_1a == 2008){
          ggiraph_countour = r_2008_df
        }
        else if(input$Year_1a == 2009){
          ggiraph_countour = r_2009_df
        }
        else if(input$Year_1a == 2010){
          ggiraph_countour = r_2010_df
        }
        else if(input$Year_1a == 2011){
          ggiraph_countour = r_2011_df
        }
        else if(input$Year_1a == 2012){
          ggiraph_countour = r_2012_df
        }
        else if(input$Year_1a == 2013){
          ggiraph_countour = r_2013_df
        }
        else if(input$Year_1a == 2014){
          ggiraph_countour = r_2014_df
        }
        else if(input$Year_1a == 2015){
          ggiraph_countour = r_2015_df
        }
        else if(input$Year_1a == 2016){
          ggiraph_countour = r_2016_df
        }
        else if(input$Year_1a == 2017){
          ggiraph_countour = r_2017_df
        }
        
        
        girafe(ggobj = ggplot(ggiraph_countour,aes(x=x,y=y,z=rowSums(ggiraph_countour[,1:12]))) + theme_void() + geom_contour_filled_interactive(aes(fill = stat(level),
                                                                                                                                                     data_id = stat(level),
                                                                                                                                                     tooltip = paste("Rainfall Range", stat(level))), bins = 9)
               +  labs(fill = "Rainfall Range(in mm)")
               +  scale_fill_brewer(palette = "Blues") ,
               options = list(opts_selection(type = "single", only_shiny = TRUE)))
        
      })
      

      
  }
  )
  
  observeEvent(input$Month_Year_1a, {
    
    # Contour Map Monthly
    
    output$ggiraph_contour_monthly <- renderGirafe({
      if(isTRUE(grepl("2007",input$Month_Year_1a,fixed = TRUE))){
        ggiraph_countour = r_2007_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1a,1,3))]
      }
      else if(isTRUE(grepl("2008",input$Month_Year_1a,fixed = TRUE))){
        ggiraph_countour = r_2008_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1a,1,3))]
      }
      else if(isTRUE(grepl("2009",input$Month_Year_1a,fixed = TRUE))){
        ggiraph_countour = r_2009_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1a,1,3))]
      }
      else if(isTRUE(grepl("2010",input$Month_Year_1a,fixed = TRUE))){
        ggiraph_countour = r_2010_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1a,1,3))]
      }
      else if(isTRUE(grepl("2011",input$Month_Year_1a,fixed = TRUE))){
        ggiraph_countour = r_2011_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1a,1,3))]
      }
      else if(isTRUE(grepl("2012",input$Month_Year_1a,fixed = TRUE))){
        ggiraph_countour = r_2012_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1a,1,3))]
      }
      else if(isTRUE(grepl("2013",input$Month_Year_1a,fixed = TRUE))){
        ggiraph_countour = r_2013_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1a,1,3))]
      }
      else if(isTRUE(grepl("2014",input$Month_Year_1a,fixed = TRUE))){
        ggiraph_countour = r_2014_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1a,1,3))]
      }
      else if(isTRUE(grepl("2015",input$Month_Year_1a,fixed = TRUE))){
        ggiraph_countour = r_2015_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1a,1,3))]
      }
      else if(isTRUE(grepl("2016",input$Month_Year_1a,fixed = TRUE))){
        ggiraph_countour = r_2016_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1a,1,3))]
      }
      else if(isTRUE(grepl("2017",input$Month_Year_1a,fixed = TRUE))){
        ggiraph_countour = r_2017_df
        month = month.name[startsWith(month.name,substr(input$Month_Year_1a,1,3))]
      }
      
      
      girafe(ggobj = ggplot(ggiraph_countour,aes(x=x,y=y,z=ggiraph_countour[[month]])) + theme_void() + geom_contour_filled_interactive(aes(fill = stat(level),
                                                                                                                                                   data_id = stat(level),
                                                                                                                                                   tooltip = paste("Rainfall Range", stat(level))), bins = 9)
             +  labs(fill = "Rainfall Range(in mm)")
             + scale_fill_brewer(palette = "Blues"),
             options = list(opts_selection(type = "single", only_shiny = TRUE)))
      
    })
    
    
    
  }
  )
  
  # Reactive values for storing current season
  
  current_season <- reactiveVal()
  current_season_rainfall <- reactiveVal()
  
  season_month <- reactive({
    if (!length(current_season())) {
      return(c(1:12))
    }
    if (current_season() == 1){
      return(c(1:2,12))
    }
    else if(current_season() == 2){
      return(c(3:5))
    }
    else if(current_season() == 3){
      return(c(6:8))
    }
    else if(current_season() == 4){
      return(c(9:11))
    }
  })
  
  # Observers for changing reactive values
  observe({
    cs <- event_data("plotly_hover" , source = "season_whole")$x
    cs2 <- event_data("plotly_hover" , source = "season")$x
    if (isTRUE(cs %in% c(1:4))) current_season(cs)
    else if(isTRUE(cs2 %in% c(1:4))) current_season(cs2)
    else current_season(NULL)
  })
  
  observe({
    
    cs <- event_data("plotly_hover" , source = "season_whole")$y
    cs2 <- event_data("plotly_hover" , source = "season")$y
    if (!is.null(cs)) current_season_rainfall(cs)
    else if(!is.null(cs2)) current_season_rainfall(cs2)
    else current_season_rainfall(NULL)
    
    
  })
  
  # Season bar chart over the decade
  
  output$season_whole <- renderPlotly({
    Total_Rainfall <- c(sum(r_2007_df$Summer) + sum(r_2008_df$Summer) + sum(r_2009_df$Summer) + sum(r_2010_df$Summer) + sum(r_2011_df$Summer) + sum(r_2012_df$Summer) + sum(r_2013_df$Summer) + sum(r_2014_df$Summer) + sum(r_2015_df$Summer) + sum(r_2016_df$Summer) + sum(r_2017_df$Summer),
                               sum(r_2007_df$Autumn) + sum(r_2008_df$Autumn) + sum(r_2009_df$Autumn) + sum(r_2010_df$Autumn) + sum(r_2011_df$Autumn) + sum(r_2012_df$Autumn) + sum(r_2013_df$Autumn) + sum(r_2014_df$Autumn) + sum(r_2015_df$Autumn) + sum(r_2016_df$Autumn) + sum(r_2017_df$Autumn),
                               sum(r_2007_df$Winter) + sum(r_2008_df$Winter) + sum(r_2009_df$Winter) + sum(r_2010_df$Winter) + sum(r_2011_df$Winter) + sum(r_2012_df$Winter) + sum(r_2013_df$Winter) + sum(r_2014_df$Winter) + sum(r_2015_df$Winter) + sum(r_2016_df$Winter) + sum(r_2017_df$Winter),
                               sum(r_2007_df$Spring) + sum(r_2008_df$Spring) + sum(r_2009_df$Spring) + sum(r_2010_df$Spring) + sum(r_2011_df$Spring) + sum(r_2012_df$Spring) + sum(r_2013_df$Spring) + sum(r_2014_df$Spring) + sum(r_2015_df$Spring) + sum(r_2016_df$Spring) + sum(r_2017_df$Spring)
                              )
    ggplotly(  ggplot(as.data.frame(Total_Rainfall),aes(x=c(1,2,3,4),y = Total_Rainfall,text=paste("Season: ",c("Summer","Autumn","Winter","Spring")),group =1)) +
                 geom_bar(stat = "identity",color = "black",fill=c("#CA0020","#2da4e9","#0e1a88","#F4A582"),width = 0.2) +
                 geom_path(size = 1.5) +
                 theme(
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(fill = "transparent",colour = NA),
                   plot.background = element_rect(fill = "transparent",colour = NA),
                   panel.border = element_rect(colour = "black", fill=NA)
                 ) +
                 scale_y_continuous(labels = scales::comma) +
                 labs(title="Total rainfall per season over the decade",x="Season",y = "Total Rainfall(in mm)") +
                 scale_x_continuous(breaks=1:4,labels = c("Summer","Autumn","Winter","Spring"))
    ,source = "season_whole",tooltip = c("text","y") ) %>%  layout(height = 500,width = 650 )  %>% config(displayModeBar = FALSE)
  })
  
  # Monthly Lollipop Chart over the decade 
  
  output$monthly_total <- renderPlotly({
    month_range <- season_month()

    if(length(month_range) == 12){
      fill_value = c("#CA0020","#2da4e9","#2da4e9","#2da4e9","#0e1a88","#0e1a88","#0e1a88","#F4A582","#F4A582","#F4A582","#CA0020","#CA0020")
    }
    else if(month_range == c(3:5)){
      fill_value = "#2da4e9"
    }
    else if(month_range == c(6:8)){
      fill_value = "#0e1a88"
    }
    else if(month_range == c(9:11)){
      fill_value = "#F4A582"
    }
    else{
      fill_value = "#CA0020"
    }
    df <- data.frame()
    decade_df <- r_2007_df[,month_range]
    for(df in list(r_2008_df,r_2009_df,r_2010_df,r_2011_df,r_2012_df,r_2013_df,r_2014_df,r_2015_df,r_2016_df,r_2017_df)){
      decade_df <- decade_df + df[,month_range]
    }
    Total_Rainfall = c(colSums(decade_df))
    ggplotly(  ggplot(as.data.frame(c(colSums(decade_df))),aes(x=month_range,y = Total_Rainfall,text = paste("Month: ",month.name[season_month()]),group =1)) +
                 geom_segment( aes(x=month_range, xend=month_range, y=0, yend=c(colSums(decade_df))), color="grey") +
                 geom_point(color=fill_value, size=4) + 
                 geom_path(size = 1.5) +
                 theme(
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(fill = "transparent",colour = NA),
                   plot.background = element_rect(fill = "transparent",colour = NA),
                   panel.border = element_rect(colour = "black", fill=NA),
                   axis.text.x = element_text(angle = 45)
                 ) +
                 scale_y_continuous(labels = scales::comma) +
                 labs(title="Total rainfall per month over the decade",x="Month",y = "Total Rainfall(in mm)") +
                 scale_x_continuous(breaks=month_range,labels = as.vector(colnames(r_2007_df[,month_range])))
    ,tooltip = c("text","y")) %>% layout(height = 500,width = 650 )  %>% config(displayModeBar = FALSE)
   
  })
}



# Calling the shinyApp function
shinyApp(ui,server)