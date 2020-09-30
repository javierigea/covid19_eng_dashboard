
library(shiny)
library(shinydashboard)
library(dplyr)
library(DBI)
library(RSQLite)
library(visNetwork)
library(DT)
library(magrittr)
library(dbplyr)
library(stringr)
library(tibbletime)
library(RColorBrewer)
library(directlabels)




function(input, output, session) {
  latest_csv_path <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
  
  pop_xls_path <- '~/Downloads/ukmidyearestimates20192020ladcodes.xls'
  
  pop_data <- read_excel(pop_xls_path, sheet = 6, skip = 4)
  pop_data <- pop_data %>%
    clean_names() %>%
    select(code, all_ages)
  
  rolling_mean <- rollify(mean, window = 7)
  
  ############ FUNCTIONS BLOCK#############
  
  #get data processes the latest csv with cases and calculates rolling means
    get_cases_data <- function(latest_csv_path){
      #get pop data
      pop_xls_path <- '~/Downloads/ukmidyearestimates20192020ladcodes.xls'
      pop_data <- read_excel(pop_xls_path, sheet = 6, skip = 4)
      pop_data <- pop_data %>%
        clean_names() %>%
        select(code, all_ages)
      rolling_mean <- rollify(mean, window = 7)
      #get covid cases data
      #clean column names, select entity and arrange by date
      data <- read_csv(latest_csv_path) %>%
        clean_names() %>% 
        filter(area_type == 'utla' | area_type == 'ltla' ) %>% 
        #drop rows that are utla and ltla
        select(-area_type) %>% 
        distinct() %>% 
        mutate(specimen_date = as.Date(specimen_date))
      #
      #add a column with pop size
      data <- merge(data,
                    pop_data,
                    by.x = 'area_code',
                    by.y = 'code',
                    all.x =T)
      data <- data %>%
        arrange(specimen_date)
      #split the dataframe by area name to fill in days with 0 cases + rejoin it
      list_df <- split(data,
                       as.character(data$area_name))
      list_df <- lapply(list_df, function(x) x %>% 
                          pad %>% fill_by_value(as.Date(specimen_date)) %>%
                          mutate(daily_lab_confirmed_cases = replace_na(daily_lab_confirmed_cases, 0)) %>%
                          fill(colnames(x)))
      data <- do.call("rbind",
                      list_df)
      data <- data %>%
        arrange(specimen_date)
      #add 7 day rolling mean
      rolling_mean <- rollify(mean, window = 7)
      data <- ddply(
        data, "area_name",
        mutate,
        mean_week = rolling_mean(daily_lab_confirmed_cases)
        
      )
      data <-  mutate(data,
                      day_of_week = weekdays(as.Date(specimen_date)),
                      specimen_date = as.Date(specimen_date))
      #add population adjusted variables
      data <-  mutate(data,
                      mean_week_pop = ((mean_week / all_ages) * 100000),
                      daily_cases_pop = ((daily_lab_confirmed_cases / all_ages) * 100000))
      return(data)
 
  }
  ####plot_cases_entity plots a barplot with daily cases + rolling averages####
  plot_cases_entity <- function(entity_names, 
                                timeframe = time, 
                                mode = c('raw', 'adj'), 
                                scale = c('nation','region','utla','ltla')) {
    
    #if (scale == 'utla'){
    #  scale = 'Upper tier local authority'
    #}
    #if (scale == 'ltla'){
    #  scale = 'Lower tier local authority'
    #}
    
    #clean column names, select entity and arrange by date
    data <- read_csv(latest_csv_path) %>%
      clean_names()
    #check if all entities are in the correct category scale
    #entity_match = entity_names[!entity_names %in% unique(data[data$area_type == scale, 'area_name'])]
    entity_match = entity_names[!entity_names %in% unique(pull(data[data$area_type == scale, 'area_name']))]
    #print wrong entities and exit
    if (length(entity_match) > 0){
      return(paste0(entity_match, ' - not valid ', scale, ' name(s)!'))
    }
    
    
    entity_data <- data %>%
      clean_names() %>%
      filter(area_name %in% entity_names) %>%
      filter(area_type %in% scale) %>%
      mutate(specimen_date = as.Date(specimen_date))
    #add a column with pop size
    entity_data <- merge(entity_data,
                         pop_data,
                         by.x = 'area_code',
                         by.y = 'code',
                         all.x =T)
    entity_data <- entity_data %>%
      arrange(specimen_date)
    #split the dataframe by area name to fill in days with 0 cases + rejoin it
    list_df <- split(entity_data,
                     as.character(entity_data$area_name))
    list_df <- lapply(list_df, function(x) x %>% 
                        pad %>% fill_by_value(as.Date(specimen_date)) %>%
                        mutate(daily_lab_confirmed_cases = replace_na(daily_lab_confirmed_cases, 0)) %>%
                        fill(colnames(x)))
    entity_data <- do.call("rbind",
                           list_df)
    entity_data <- entity_data %>%
      arrange(specimen_date)
    #add 7 day rolling mean
    rolling_mean <- rollify(mean, window = 7)
    
    entity_data <- ddply(
      entity_data, "area_name",
      mutate,
      mean_week = rolling_mean(daily_lab_confirmed_cases)
    )
    #rolling_mean <- rollify(mean, window = 7)
    entity_data <-  mutate(entity_data, 
                           #mean_week2 = rolling_mean(daily_lab_confirmed_cases),
                           day_of_week = weekdays(as.Date(specimen_date)),
                           specimen_date = as.Date(specimen_date))
    #add population adjusted variables
    entity_data <-  mutate(entity_data,
                           mean_week_pop = ((mean_week / all_ages) * 100000),
                           daily_cases_pop = ((daily_lab_confirmed_cases / all_ages) * 100000))
    
    
    #subset data for timeframe
    #if(timeframe == 'historic'){
    #  entity_data <- entity_data
    #}else if (timeframe == 'month'){
    #  entity_data <- entity_data %>%
    #    filter(specimen_date >= (max(as.Date(entity_data$specimen_date))-30))
    #}else if (timeframe == 'fortnight'){
    #  entity_data <- entity_data %>%
    #    filter(specimen_date >= (max(as.Date(entity_data$specimen_date))-15))
    #}
    entity_data <- entity_data %>%
      filter(specimen_date >= (max(as.Date(entity_data$specimen_date))-(7*timeframe)))
      
    if (mode == 'raw') {
      # barplot + 
      ggplot(entity_data,
             aes(x = specimen_date,
                 y = daily_lab_confirmed_cases)) +
        #ggtitle(entity_name) +
        geom_bar(stat = 'identity', colour = 'grey', fill = 'grey') +
        geom_line(aes(x=specimen_date,
                      y = mean_week), group = 1, colour="steelblue", lwd = 2) +
        ylab('number of daily COVID-19 positives') +
        xlab('date of test') +
        theme_classic() +
        theme(strip.background  = element_blank(),
              strip.text.x = element_text(size = 12),
              axis.text.x = element_text(size=8),
              axis.text.y = element_text(size=8)) + 
        #scale_colour_manual(name="7-day mean:", values=c('mean_week' = 'blue')) +
        #theme(legend.position="bottom") +
        scale_y_continuous(expand = c(0,0)) +
        facet_wrap(. ~ area_name)
    }else if (mode == 'adj') {
      # barplot + 
      ggplot(entity_data,
             aes(x = specimen_date,
                 y = daily_cases_pop)) +
        #ggtitle(entity_name) +
        geom_bar(stat = 'identity', colour = 'grey', fill = 'grey') +
        geom_line(aes(x=specimen_date,
                      y = mean_week_pop), group = 1, colour="steelblue", lwd = 2) +
        ylab('rate of daily COVID-19 positives per 100k') +
        xlab('date of test') +
        theme_classic() +
        theme(strip.background  = element_blank(),
              strip.text.x = element_text(size = 12),
              axis.text.x = element_text(size=8),
              axis.text.y = element_text(size=8)) + 
        scale_y_continuous(expand = c(0,0)) +
        facet_wrap(. ~ area_name)
      
    }
  }
  
  
  
  
  ####plot_cases_local plots lines of rolling averages for up to 8 local authorities####
  plot_cases_local <- function(entity_names, 
                               timeframe = time, 
                               mode = c('raw', 'adj'))
    
    
  {
    #set a limit of 8 entities to check
    if (length(entity_names) > 8){
      return ('max limit of 8 places to check, sorry!')
    }
    
   
    #clean column names, select entity and arrange by date
    data <- read_csv(latest_csv_path) %>%
      clean_names() %>% 
      filter(area_type == 'utla' | area_type == 'ltla' ) %>% 
      #drop rows that are utla and ltla
      select(-area_type) %>% 
      distinct() %>% 
      mutate(specimen_date = as.Date(specimen_date))
    #check if all entities are in the correct category scale
    #entity_match = entity_names[!entity_names %in% unique(data[data$area_type == scale, 'area_name'])]
    entity_match = entity_names[!entity_names %in% unique(data$area_name)]
    #print wrong entities and exit
    if (length(entity_match) > 0){
      return(paste0(entity_match, ' - not valid name(s)!'))
    }
    
    #
    #add a column with pop size
    data <- merge(data,
                  pop_data,
                  by.x = 'area_code',
                  by.y = 'code',
                  all.x =T)
    
    
    data <- data %>%
      arrange(specimen_date)
    
    #split the dataframe by area name to fill in days with 0 cases + rejoin it
    list_df <- split(data,
                     as.character(data$area_name))
    list_df <- lapply(list_df, function(x) x %>% 
                        pad %>% fill_by_value(as.Date(specimen_date)) %>%
                        mutate(daily_lab_confirmed_cases = replace_na(daily_lab_confirmed_cases, 0)) %>%
                        fill(colnames(x)))
    data <- do.call("rbind",
                    list_df)
    data <- data %>%
      arrange(specimen_date)
    #add 7 day rolling mean
    rolling_mean <- rollify(mean, window = 7)
    
    data <- ddply(
      data, "area_name",
      mutate,
      mean_week = rolling_mean(daily_lab_confirmed_cases)
      
    )

    data <-  mutate(data,
                    day_of_week = weekdays(as.Date(specimen_date)),
                    specimen_date = as.Date(specimen_date))
    #add population adjusted variables
    data <-  mutate(data,
                    mean_week_pop = ((mean_week / all_ages) * 100000),
                    daily_cases_pop = ((daily_lab_confirmed_cases / all_ages) * 100000))
    #subset to timeframe
    data <- data %>%
      filter(specimen_date >= (max(as.Date(data$specimen_date))-(7*timeframe)))
    #divide tibble into entity_data and rest_data
    entity_data <- data %>%
      filter(area_name %in% entity_names)
    if (mode == 'raw') {
      #plot mean_week
      ymax = max(entity_data$mean_week)*1.10
      ggplot() +
        geom_line(data = data, aes(x = specimen_date,
                                   y = mean_week,
                                   group = area_name),
                  size = 0.25,
                  color = 'grey') +
        
        geom_line(data = entity_data, aes(x = specimen_date,
                                          y = mean_week,
                                          group = area_name,
                                          color = area_name),
                  size = 2) +
        #color = entity_data$linecolour) +
        scale_color_brewer(palette="Set2") +
        #scale_colour_discrete(guide = 'none') +
        #scale_x_discrete(expand=c(0, 3)) +
        #scale_y_continuous(trans = 'log10') + 
        geom_dl(data = entity_data, aes(x = specimen_date,
                                        y = mean_week,
                                        label = area_name,color = area_name), method = list(dl.combine("last.points"),dl.trans(x=x+0.1),fontface = "bold"), cex = 0.8) +
        ylim(0,ymax) +
        #scale_y_continuous(trans = 'log10') + 
        ylab('number of daily COVID-19 positives') +
        xlab('date of test') +
        #geom_dl(aes(label = area_name), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
        theme_classic() +
        theme(strip.background  = element_blank(),
              strip.text.x = element_text(size = 12),
              axis.text.x = element_text(size=12),
              axis.title.x = element_text(size=14),
              axis.text.y = element_text(size=12),
              axis.title.y = element_text(size=14),
              legend.position = 'none')
      #legend.title = element_blank())
      
      
    } else if (mode == 'adj') {
      #plot mean_week
      ymax = max(entity_data$mean_week_pop)*1.10
      ggplot() +
        geom_line(data = data, aes(x = specimen_date,
                                   y = mean_week_pop,
                                   group = area_name),
                  size = 0.25,
                  color = 'grey') +
        
        geom_line(data = entity_data, aes(x = specimen_date,
                                          y = mean_week_pop,
                                          group = area_name,
                                          color = area_name),
                  size = 2) +
        #color = entity_data$linecolour) +
        scale_color_brewer(palette="Set2") +
        #scale_colour_discrete(guide = 'none') +
        #scale_x_discrete(expand=c(0, 3)) +
        geom_dl(data = entity_data, aes(x = specimen_date,
                                        y = mean_week_pop,
                                        label = area_name,color = area_name), method = list(dl.combine("last.points"),dl.trans(x=x+0.1),fontface = "bold"), cex = 0.8) +
        ylim(0,ymax) +
        ylab('number of daily COVID-19 positives pero 100k') +
        xlab('date of test') +
        #geom_dl(aes(label = area_name), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
        theme_classic() +
        theme(strip.background  = element_blank(),
              strip.text.x = element_text(size = 12),
              axis.text.x = element_text(size=12),
              axis.title.x = element_text(size=14),
              axis.text.y = element_text(size=12),
              axis.title.y = element_text(size=14),
              legend.position = 'none')
      #legend.title = element_blank())
    }
  }

    plot_cases_local_data <- function(data = data,
                                    entity_names,
                                    timeframe = time, 
                                    mode = c('raw', 'adj'))
                                    
                               
    
    
  {
    #set a limit of 8 entities to check
    if (length(entity_names) > 8){
      return ('max limit of 8 places to check, sorry!')
    }
    
    
    #clean column names, select entity and arrange by date
    data <- data
    #check if all entities are in the correct category scale
    #entity_match = entity_names[!entity_names %in% unique(data[data$area_type == scale, 'area_name'])]
    entity_match = entity_names[!entity_names %in% unique(data$area_name)]
    #print wrong entities and exit
    if (length(entity_match) > 0){
      return(paste0(entity_match, ' - not valid name(s)!'))
    }
    
    #subset to timeframe
    data <- data %>%
      filter(specimen_date >= (max(as.Date(data$specimen_date))-(7*timeframe)))
    #divide tibble into entity_data and rest_data
    entity_data <- data %>%
      filter(area_name %in% entity_names)
    if (mode == 'raw') {
      #plot mean_week
      ymax = max(entity_data$mean_week)*1.10
      ggplot() +
        geom_line(data = data, aes(x = specimen_date,
                                   y = mean_week,
                                   group = area_name),
                  size = 0.25,
                  color = 'grey') +
        
        geom_line(data = entity_data, aes(x = specimen_date,
                                          y = mean_week,
                                          group = area_name,
                                          color = area_name),
                  size = 2) +
        #color = entity_data$linecolour) +
        scale_color_brewer(palette="Set2") +
        #scale_colour_discrete(guide = 'none') +
        #scale_x_discrete(expand=c(0, 3)) +
        #scale_y_continuous(trans = 'log10') + 
        geom_dl(data = entity_data, aes(x = specimen_date,
                                        y = mean_week,
                                        label = area_name,color = area_name), method = list(dl.combine("last.points"),dl.trans(x=x+0.1),fontface = "bold"), cex = 0.8) +
        ylim(0,ymax) +
        #scale_y_continuous(trans = 'log10') + 
        ylab('number of daily COVID-19 positives') +
        xlab('date of test') +
        #geom_dl(aes(label = area_name), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
        theme_classic() +
        theme(strip.background  = element_blank(),
              strip.text.x = element_text(size = 12),
              axis.text.x = element_text(size=12),
              axis.title.x = element_text(size=14),
              axis.text.y = element_text(size=12),
              axis.title.y = element_text(size=14),
              legend.position = 'none')
      #legend.title = element_blank())
      
      
    } else if (mode == 'adj') {
      #plot mean_week
      ymax = max(entity_data$mean_week_pop)*1.10
      ggplot() +
        geom_line(data = data, aes(x = specimen_date,
                                   y = mean_week_pop,
                                   group = area_name),
                  size = 0.25,
                  color = 'grey') +
        
        geom_line(data = entity_data, aes(x = specimen_date,
                                          y = mean_week_pop,
                                          group = area_name,
                                          color = area_name),
                  size = 2) +
        #color = entity_data$linecolour) +
        scale_color_brewer(palette="Set2") +
        #scale_colour_discrete(guide = 'none') +
        #scale_x_discrete(expand=c(0, 3)) +
        geom_dl(data = entity_data, aes(x = specimen_date,
                                        y = mean_week_pop,
                                        label = area_name,color = area_name), method = list(dl.combine("last.points"),dl.trans(x=x+0.1),fontface = "bold"), cex = 0.8) +
        ylim(0,ymax) +
        ylab('number of daily COVID-19 positives pero 100k') +
        xlab('date of test') +
        #geom_dl(aes(label = area_name), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
        theme_classic() +
        theme(strip.background  = element_blank(),
              strip.text.x = element_text(size = 12),
              axis.text.x = element_text(size=12),
              axis.title.x = element_text(size=14),
              axis.text.y = element_text(size=12),
              axis.title.y = element_text(size=14),
              legend.position = 'none')
      #legend.title = element_blank())
    }
  }
  
    
  ##################END OF FUNCTIONS BLOCK##################
    cases_data <- get_cases_data(latest_csv_path = latest_csv_path)
    data <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv") %>% 
      clean_names()
    
  ############## get names of regions for selection ##############
  RegionOptions <- data %>%
    filter(area_type == 'region') %>%
    as.data.frame() %$%
    unique(area_name)
  updateSelectInput(session, "selRegionNames", choices = RegionOptions)
  
  ############## get names of utla for selection ##############
  UtlaOptions <- data %>%
    filter(area_type == 'utla') %>%
    as.data.frame() %$%
    unique(area_name)
  updateSelectInput(session, "selUtlaNames", choices = UtlaOptions)
  
  ############## get names of ltla for selection ##############
  LtlaOptions <- data %>%
    filter(area_type == 'ltla') %>%
    as.data.frame() %$%
    unique(area_name)
  updateSelectInput(session, "selLtlaNames", choices = LtlaOptions)
  
  ############## get names of localauthorities for selection ##############
  LocAuthOptions <- data %>%
    filter(area_type == 'ltla' | area_type == 'utla') %>%
    as.data.frame() %$%
    unique(area_name)
  updateSelectInput(session, "selLocAuthNames", choices = LocAuthOptions)
  
  #rolling_mean <- rollify(mean, window = 7)
  
  output$RegionPlot <- renderPlot({
    selRegionAdj = input$selRegionAdj
    if(selRegionAdj) {RegionMode = 'adj'}else{RegionMode = 'raw'}
    RegionTime = as.numeric(input$selRegionTime)
    plot_cases_entity(entity_names = input$selRegionNames,
                                                  timeframe = RegionTime,
                                                  mode = RegionMode,
                                                  scale = 'region')})
  output$UtlaPlot <- renderPlot({
    selUtlaAdj = input$selUtlaAdj
    if(selUtlaAdj) {UtlaMode = 'adj'}else{UtlaMode = 'raw'}
    UtlaTime = as.numeric(input$selUtlaTime)
    plot_cases_entity(entity_names = input$selUtlaNames,
                      timeframe = UtlaTime,
                      mode = UtlaMode,
                      scale = 'utla')})
  
  output$LtlaPlot <- renderPlot({
    selLtlaAdj = input$selLtlaAdj
    if(selLtlaAdj) {LtlaMode = 'adj'}else{LtlaMode = 'raw'}
    LtlaTime = as.numeric(input$selLtlaTime)
    plot_cases_entity(entity_names = input$selLtlaNames,
                      timeframe = LtlaTime,
                      mode = LtlaMode,
                      scale = 'ltla')})
  #LocAuthNames_d <- debounce(LocAuthNames, 2000)
  output$LocAuthPlot <- renderPlot({
    
    if(!is.null(input$selLocAuthNames)){
      #LocAuthNames <- reactive({
      #  input$selLocAuthNames
      #}) %>% debounce(2000)
      
      selLocAuthAdj = input$selLocAuthAdj
      if(selLocAuthAdj) {LocAuthMode = 'adj'}else{LocAuthMode = 'raw'}
      LocAuthTime = as.numeric(input$selLocAuthTime)
      #LocAuthNames <- input$selLocAuthNames
      #LocAuthNames_d <- debounce(LocAuthNames, 2000)
        
      plot_cases_local_data(data = cases_data,
                       entity_names = input$selLocAuthNames,
                       timeframe = LocAuthTime,
                       mode = LocAuthMode)
    
    }})
      
    
}

    

  
