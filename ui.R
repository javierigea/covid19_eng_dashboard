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
header = dashboardHeader(title = 'Coronavirus in England')

#do not display data older than 10th March 2020
maxweeks <- as.numeric(ceiling(difftime(Sys.Date(),as.Date("2020-03-10"),units='weeks')))
#### SIDEBAR ####
sidebar =  dashboardSidebar(width = 230,
                            sidebarMenu(id = "tabs",
                                        menuItem("start", tabName = "start", icon = icon("dashboard"), selected = T),
                                        menuItem("Regions", tabName = "regions", icon = icon("map", lib = "font-awesome")),
                                        #menuItem("UTLA", tabName = "utla", icon = icon("city", lib = "font-awesome")),
                                        #menuItem("LTLA", tabName = "ltla", icon = icon("city", lib = "font-awesome")),
                                        menuItem("Local Authorities", tabName = "locauth", icon = icon("city", lib = "font-awesome")),
                                        conditionalPanel(condition = "['regions'].includes(input.tabs)",
                                                         selectInput("selRegionNames", "Regions", choices ="", multiple = TRUE, selectize = TRUE),
                                                         checkboxInput("selRegionAdj", "Show positives per 100k in Region", value = TRUE),
                                                         sliderInput("selRegionTime", "Last N Weeks", ticks = FALSE, min = 2, max = maxweeks, value = 2, step = 1)),
                                        # conditionalPanel(condition = "['utla'].includes(input.tabs)",
                                        #                  selectInput("selUtlaNames", "UTLA", choices ="", multiple = TRUE, selectize = TRUE),
                                        #                  checkboxInput("selUtlaAdj", "Show positives per 100k in UTLA", value = TRUE),
                                        #                  sliderInput("selUtlaTime", "Last N Weeks", ticks = FALSE, min = 2, max = maxweeks, value = 2, step = 1)),
                                        # conditionalPanel(condition = "['ltla'].includes(input.tabs)",
                                        #                  selectInput("selLtlaNames", "LTLA", choices ="", multiple = TRUE, selectize = TRUE),
                                        #                  checkboxInput("selLtlaAdj", "Show positives per 100k in LTLA", value = TRUE),
                                        #                  sliderInput("selLtlaTime", "Last N Weeks", ticks = FALSE, min = 2, max = maxweeks, value = 2, step = 1)),
                                        conditionalPanel(condition = "['locauth'].includes(input.tabs)",
                                                         selectInput("selLocAuthNames", "Local Authorities", choices ="", multiple = TRUE, selectize = TRUE),
                                                         checkboxInput("selLocAuthAdj", "Show positives per 100k in Local Authority", value = TRUE),
                                                         sliderInput("selLocAuthTime", "Last N Weeks", ticks = FALSE, min = 2, max = maxweeks, value = 2, step = 1)
                                                         
                                                         ),
                                        )
                            )

                            
                            
                                                         
                                                         
                                        
                                        

#### BODY ####
body  =  dashboardBody(tabItems(tabItem(tabName = "start",
                                        fluidRow(box("Introduction",
                                                      "blablabla"))),
                                tabItem(tabName = "regions",
                                        fluidRow(box(width = 12,
                                                     title = "Regions",
                                                     plotOutput("RegionPlot"),
                                                     br()))),
                                                     
                                tabItem(tabName = "utla",
                                        fluidRow(box(width = 12,
                                                     title = "utla",
                                                     plotOutput("UtlaPlot"),
                                                     br()))),
                                tabItem(tabName = "ltla",
                                        fluidRow(box(width = 12,
                                                     title = "ltla",
                                                     plotOutput("LtlaPlot"),
                                                     br()))),
                                tabItem(tabName = "locauth",
                                        fluidRow(box(width = 12,
                                                     title = "COVID-19 Cases in Local Authorities in England",
                                                     plotOutput("LocAuthPlot"),
                                                     br())))
                                
))


                           

#### Shiny UI ####
dashboardPage(header, sidebar, body)
