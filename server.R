
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



function(input, output, session) {
  latest_csv_path <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
  data <- read_csv(latest_csv_path) %>%
    clean_names()
    
  
  pop_xls_path <- '~/Downloads/ukmidyearestimates20192020ladcodes.xls'
  pop_data <- read_excel(pop_xls_path, sheet = 6, skip = 4)
  pop_data <- pop_data %>%
    clean_names() %>%
    select(code, all_ages)
  
  rolling_mean <- rollify(mean, window = 7)
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
    print(entity_match)
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
  
  rolling_mean <- rollify(mean, window = 7)
  
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
}

    

  
