library(DBI)
library(RSQLite)
library(shinydashboard)
options(shiny.sanitize.errors = FALSE)
library(ggplot2)
library(tibble)
### library(readr)## probably won't be used
library(magrittr)
library(stringr)
library(DT)
library(shiny)
library(plyr)
library(dplyr)
library(dbplyr)
library(tidyverse)
library(janitor)
library(tidyr)
library(RCurl)
library(readxl)
library(gridExtra)
library(zoo)
library(padr)
library(tibbletime)




#### HEADER ####
header = dashboardHeader(title = "COVID19-England")

#do not display data older than 10th March 2020
maxweeks <- as.numeric(ceiling(difftime(Sys.Date(),as.Date("2020-03-10"),units='weeks')))

data <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv") %>% 
  clean_names()

last_update_date <- format(as.Date(max(data$specimen_date)),"%d %b %y")
#### SIDEBAR ####
sidebar =  dashboardSidebar(width = 230,
                            sidebarMenu(id = "tabs",
                                        menuItem("Info", tabName = "info", icon = icon("info-circle", lib = "font-awesome")),
                                        menuItem("Regions", tabName = "regions", icon = icon("city", lib = "font-awesome")),
                                        #menuItem("UTLA", tabName = "utla", icon = icon("city", lib = "font-awesome")),
                                        #menuItem("LTLA", tabName = "ltla", icon = icon("city", lib = "font-awesome")),
                                        menuItem("Local Authorities", tabName = "locauth", icon = icon("building", lib = "font-awesome")),
                                        menuItem("Map of Weekly Cases", tabName = "map_weekly_cases", icon = icon("map", lib = "font-awesome")),
                                        menuItem("Map of Change in Weekly Cases", tabName = "map_change_weekly_cases", icon = icon("map", lib = "font-awesome")),
                                        conditionalPanel(condition = "['regions'].includes(input.tabs)",
                                                         selectInput("selRegionNames", "Regions - pick up to 8:", choices ="", multiple = TRUE, selectize = TRUE),
                                                         checkboxInput("selRegionAdj", "Show positives per 100,000 population", value = TRUE),
                                                         sliderInput("selRegionTime", "Number of weeks of data to include:", ticks = FALSE, min = 2, max = maxweeks, value = 4, step = 1)),
                                        # conditionalPanel(condition = "['utla'].includes(input.tabs)",
                                        #                  selectInput("selUtlaNames", "UTLA", choices ="", multiple = TRUE, selectize = TRUE),
                                        #                  checkboxInput("selUtlaAdj", "Show positives per 100k in UTLA", value = TRUE),
                                        #                  sliderInput("selUtlaTime", "Last N Weeks", ticks = FALSE, min = 2, max = maxweeks, value = 2, step = 1)),
                                        # conditionalPanel(condition = "['ltla'].includes(input.tabs)",
                                        #                  selectInput("selLtlaNames", "LTLA", choices ="", multiple = TRUE, selectize = TRUE),
                                        #                  checkboxInput("selLtlaAdj", "Show positives per 100k in LTLA", value = TRUE),
                                        #                  sliderInput("selLtlaTime", "Last N Weeks", ticks = FALSE, min = 2, max = maxweeks, value = 2, step = 1)),
                                        conditionalPanel(condition = "['locauth'].includes(input.tabs)",
                                                         selectInput("selLocAuthNames", "Local Authorities - pick up to 8:", choices ="", multiple = TRUE, selectize = TRUE),
                                                         checkboxInput("selLocAuthAdj", "Show positives per 100,000 population", value = TRUE),
                                                         sliderInput("selLocAuthTime", "Number of weeks of data to include:", ticks = FALSE, min = 2, max = maxweeks, value = 4, step = 1)),
                                        conditionalPanel(condition = "['map_weekly_cases'].includes(input.tabs)",
                                                         dateInput("selWeekEnding", "Show total cases for week ending on", min = '2020-03-10', max = Sys.Date()-4),default = ''),
                                        conditionalPanel(condition = "['map_change_weekly_cases'].includes(input.tabs)",
                                                         dateInput("selChangeWeekEnding", "Show change in total cases for week ending on and previous week", min = '2020-03-10', max = Sys.Date()-4),default = '')
                                                       
                                        )
                            )

                            
                            
                                                         
                                                         
                                        
                                        

#### BODY ####
body  =  dashboardBody(tabItems(tabItem(tabName = "regions",
                                        fluidRow(box(width = 12,
                                                     title = "COVID-19 cases in regions in England",
                                                     plotOutput("RegionPlot")))),
                                                     #br()))),
                                tabItem(tabName = "info",
                                        fluidPage(strong('Coronavirus cases in England'),
                                                  br(),
                                                  'Data from Public Health England',
                                                  br(),
                                                  'Last updated on ',strong(last_update_date),
                                                  br(),
                                                  em('*Cases from last 4 days not included to correct for delay between test and report'),
                                                  br(),
                                                  br(),
                                                  br(),
                                                  'Scripts available on ',
                                                  tags$a(href='https://github.com/javierigea/covid19_eng_dashboard', "Github"))),
                                                  
                                
                                                  
                                                     
                                # tabItem(tabName = "utla",
                                #         fluidRow(box(width = 12,
                                #                      title = "utla",
                                #                      plotOutput("UtlaPlot"),
                                #                      br()))),
                                # tabItem(tabName = "ltla",
                                #         fluidRow(box(width = 12,
                                #                      title = "ltla",
                                #                      plotOutput("LtlaPlot"),
                                #                      br()))),
                                tabItem(tabName = "locauth",
                                        fluidRow(box(width = 12,
                                                     title = "COVID-19 cases in local authorities in England",
                                                     plotOutput("LocAuthPlot"),
                                                     br()))),
                                tabItem(tabName = "map_weekly_cases",
                                        fluidRow(box(width = 12,
                                                     title = "Total cases per week in England",
                                                     plotOutput("MapWeeklyCases"),
                                                     br()))),
                                tabItem(tabName = "map_change_weekly_cases",
                                        fluidRow(box(width = 12,
                                                     title = "Total change in cases per week in England",
                                                     plotOutput("MapChangeWeeklyCases"),
                                                     br())))
                                
))


                           

#### Shiny UI ####
dashboardPage(header, sidebar, body)
