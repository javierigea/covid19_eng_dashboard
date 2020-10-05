library(gridExtra)
library(grid)
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
library(viridis)
library(sf)
library(grDevices)


function(input, output, session) {
  latest_csv_path <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
  
  pop_xls_path <- './data/ukmidyearestimates20192020ladcodes.xls'
  
  pop_data <- read_excel(pop_xls_path, sheet = 6, skip = 4)
  pop_data <- pop_data %>%
    clean_names() %>%
    select(code, all_ages)
  
  rolling_mean <- rollify(mean, window = 7)
  #read map
  map <-readRDS('./data/data_counties_simplified.RDS')
  ############ FUNCTIONS BLOCK#############
  
  #get data processes the latest csv with cases and calculates rolling means
    get_cases_data <- function(latest_csv_path,
                               mode = c('local','region')){
      #get pop data
      pop_xls_path <- './data/ukmidyearestimates20192020ladcodes.xls'
      pop_data <- read_excel(pop_xls_path, sheet = 6, skip = 4)
      pop_data <- pop_data %>%
        clean_names() %>%
        select(code, all_ages)
      rolling_mean <- rollify(mean, window = 7)
      #get covid cases data
      #clean column names, select entity and arrange by date
      if(mode == 'local'){
        data <- read_csv(latest_csv_path) %>%
        clean_names() %>% 
        filter(area_type == 'utla' | area_type == 'ltla' ) %>% 
        #drop rows that are utla and ltla
        select(-area_type) %>% 
        distinct() %>% 
        mutate(specimen_date = as.Date(specimen_date))
      }else if (mode  == 'region'){
        data <- read_csv(latest_csv_path) %>%
          clean_names() %>% 
          filter(area_type == 'region' ) %>% 
          #drop rows that are utla and ltla
          select(-area_type) %>% 
          distinct() %>% 
          mutate(specimen_date = as.Date(specimen_date))
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
      # #this was only needed with a previous format of the csv
      # #split the dataframe by area name to fill in days with 0 cases + rejoin it
      # list_df <- split(data,
      #                  as.character(data$area_name))
      # list_df <- lapply(list_df, function(x) x %>% 
      #                     pad %>% fill_by_value(as.Date(specimen_date)) %>%
      #                     mutate(daily_lab_confirmed_cases = replace_na(daily_lab_confirmed_cases, 0)) %>%
      #                     fill(colnames(x)))
      # data <- do.call("rbind",
      #                 list_df)
      # data <- data %>%
      #   arrange(specimen_date)
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
  # ####plot_cases_entity plots a barplot with daily cases + rolling averages####
  # plot_cases_entity <- function(entity_names, 
  #                               timeframe = time, 
  #                               mode = c('raw', 'adj'), 
  #                               scale = c('nation','region','utla','ltla')) {
  #   
  #   #if (scale == 'utla'){
  #   #  scale = 'Upper tier local authority'
  #   #}
  #   #if (scale == 'ltla'){
  #   #  scale = 'Lower tier local authority'
  #   #}
  #   
  #   #clean column names, select entity and arrange by date
  #   data <- read_csv(latest_csv_path) %>%
  #     clean_names()
  #   #check if all entities are in the correct category scale
  #   #entity_match = entity_names[!entity_names %in% unique(data[data$area_type == scale, 'area_name'])]
  #   entity_match = entity_names[!entity_names %in% unique(pull(data[data$area_type == scale, 'area_name']))]
  #   #print wrong entities and exit
  #   if (length(entity_match) > 0){
  #     return(paste0(entity_match, ' - not valid ', scale, ' name(s)!'))
  #   }
  #   
  #   
  #   entity_data <- data %>%
  #     clean_names() %>%
  #     filter(area_name %in% entity_names) %>%
  #     filter(area_type %in% scale) %>%
  #     mutate(specimen_date = as.Date(specimen_date))
  #   #add a column with pop size
  #   entity_data <- merge(entity_data,
  #                        pop_data,
  #                        by.x = 'area_code',
  #                        by.y = 'code',
  #                        all.x =T)
  #   entity_data <- entity_data %>%
  #     arrange(specimen_date)
  #   #split the dataframe by area name to fill in days with 0 cases + rejoin it
  #   list_df <- split(entity_data,
  #                    as.character(entity_data$area_name))
  #   list_df <- lapply(list_df, function(x) x %>% 
  #                       pad %>% fill_by_value(as.Date(specimen_date)) %>%
  #                       mutate(daily_lab_confirmed_cases = replace_na(daily_lab_confirmed_cases, 0)) %>%
  #                       fill(colnames(x)))
  #   entity_data <- do.call("rbind",
  #                          list_df)
  #   entity_data <- entity_data %>%
  #     arrange(specimen_date)
  #   #add 7 day rolling mean
  #   rolling_mean <- rollify(mean, window = 7)
  #   
  #   entity_data <- ddply(
  #     entity_data, "area_name",
  #     mutate,
  #     mean_week = rolling_mean(daily_lab_confirmed_cases)
  #   )
  #   #rolling_mean <- rollify(mean, window = 7)
  #   entity_data <-  mutate(entity_data, 
  #                          #mean_week2 = rolling_mean(daily_lab_confirmed_cases),
  #                          day_of_week = weekdays(as.Date(specimen_date)),
  #                          specimen_date = as.Date(specimen_date))
  #   #add population adjusted variables
  #   entity_data <-  mutate(entity_data,
  #                          mean_week_pop = ((mean_week / all_ages) * 100000),
  #                          daily_cases_pop = ((daily_lab_confirmed_cases / all_ages) * 100000))
  #   
  #   
  #   #subset data for timeframe
  #   #if(timeframe == 'historic'){
  #   #  entity_data <- entity_data
  #   #}else if (timeframe == 'month'){
  #   #  entity_data <- entity_data %>%
  #   #    filter(specimen_date >= (max(as.Date(entity_data$specimen_date))-30))
  #   #}else if (timeframe == 'fortnight'){
  #   #  entity_data <- entity_data %>%
  #   #    filter(specimen_date >= (max(as.Date(entity_data$specimen_date))-15))
  #   #}
  #   entity_data <- entity_data %>%
  #     filter(specimen_date >= (max(as.Date(entity_data$specimen_date))-(7*timeframe)))
  #     
  #   if (mode == 'raw') {
  #     # barplot + 
  #     ggplot(entity_data,
  #            aes(x = specimen_date,
  #                y = daily_lab_confirmed_cases)) +
  #       #ggtitle(entity_name) +
  #       geom_bar(stat = 'identity', colour = 'grey', fill = 'grey') +
  #       geom_line(aes(x=specimen_date,
  #                     y = mean_week), group = 1, colour="steelblue", lwd = 2) +
  #       ylab('number of daily COVID-19 positives') +
  #       xlab('date of test') +
  #       theme_classic() +
  #       theme(strip.background  = element_blank(),
  #             strip.text.x = element_text(size = 12),
  #             axis.text.x = element_text(size=8),
  #             axis.text.y = element_text(size=8)) + 
  #       #scale_colour_manual(name="7-day mean:", values=c('mean_week' = 'blue')) +
  #       #theme(legend.position="bottom") +
  #       scale_y_continuous(expand = c(0,0)) +
  #       facet_wrap(. ~ area_name)
  #   }else if (mode == 'adj') {
  #     # barplot + 
  #     ggplot(entity_data,
  #            aes(x = specimen_date,
  #                y = daily_cases_pop)) +
  #       #ggtitle(entity_name) +
  #       geom_bar(stat = 'identity', colour = 'grey', fill = 'grey') +
  #       geom_line(aes(x=specimen_date,
  #                     y = mean_week_pop), group = 1, colour="steelblue", lwd = 2) +
  #       ylab('rate of daily COVID-19 positives per 100k') +
  #       xlab('date of test') +
  #       theme_classic() +
  #       theme(strip.background  = element_blank(),
  #             strip.text.x = element_text(size = 12),
  #             axis.text.x = element_text(size=8),
  #             axis.text.y = element_text(size=8)) + 
  #       scale_y_continuous(expand = c(0,0)) +
  #       facet_wrap(. ~ area_name)
  #     
  #   }
  # }
  # 
  
  
  
  # ####plot_cases_local plots lines of rolling averages for up to 8 local authorities####
  # plot_cases_local <- function(entity_names, 
  #                              timeframe = time, 
  #                              mode = c('raw', 'adj'))
  #   
  #   
  # {
  #   #set a limit of 8 entities to check
  #   if (length(entity_names) > 8){
  #     return ('max limit of 8 places to check, sorry!')
  #   }
  #   
  #  
  #   #clean column names, select entity and arrange by date
  #   data <- read_csv(latest_csv_path) %>%
  #     clean_names() %>% 
  #     filter(area_type == 'utla' | area_type == 'ltla' ) %>% 
  #     #drop rows that are utla and ltla
  #     select(-area_type) %>% 
  #     distinct() %>% 
  #     mutate(specimen_date = as.Date(specimen_date))
  #   #check if all entities are in the correct category scale
  #   #entity_match = entity_names[!entity_names %in% unique(data[data$area_type == scale, 'area_name'])]
  #   entity_match = entity_names[!entity_names %in% unique(data$area_name)]
  #   #print wrong entities and exit
  #   if (length(entity_match) > 0){
  #     return(paste0(entity_match, ' - not valid name(s)!'))
  #   }
  #   
  #   #
  #   #add a column with pop size
  #   data <- merge(data,
  #                 pop_data,
  #                 by.x = 'area_code',
  #                 by.y = 'code',
  #                 all.x =T)
  #   
  #   
  #   data <- data %>%
  #     arrange(specimen_date)
  #   
  #   #split the dataframe by area name to fill in days with 0 cases + rejoin it
  #   list_df <- split(data,
  #                    as.character(data$area_name))
  #   list_df <- lapply(list_df, function(x) x %>% 
  #                       pad %>% fill_by_value(as.Date(specimen_date)) %>%
  #                       mutate(daily_lab_confirmed_cases = replace_na(daily_lab_confirmed_cases, 0)) %>%
  #                       fill(colnames(x)))
  #   data <- do.call("rbind",
  #                   list_df)
  #   data <- data %>%
  #     arrange(specimen_date)
  #   #add 7 day rolling mean
  #   rolling_mean <- rollify(mean, window = 7)
  #   
  #   data <- ddply(
  #     data, "area_name",
  #     mutate,
  #     mean_week = rolling_mean(daily_lab_confirmed_cases)
  #     
  #   )
  # 
  #   data <-  mutate(data,
  #                   day_of_week = weekdays(as.Date(specimen_date)),
  #                   specimen_date = as.Date(specimen_date))
  #   #add population adjusted variables
  #   data <-  mutate(data,
  #                   mean_week_pop = ((mean_week / all_ages) * 100000),
  #                   daily_cases_pop = ((daily_lab_confirmed_cases / all_ages) * 100000))
  #   #subset to timeframe
  #   data <- data %>%
  #     filter(specimen_date >= (max(as.Date(data$specimen_date))-(7*timeframe)))
  #   #divide tibble into entity_data and rest_data
  #   entity_data <- data %>%
  #     filter(area_name %in% entity_names)
  #   if (mode == 'raw') {
  #     #plot mean_week
  #     ymax = max(entity_data$mean_week)*1.10
  #     ggplot() +
  #       geom_line(data = data, aes(x = specimen_date,
  #                                  y = mean_week,
  #                                  group = area_name),
  #                 size = 0.25,
  #                 color = 'grey') +
  #       
  #       geom_line(data = entity_data, aes(x = specimen_date,
  #                                         y = mean_week,
  #                                         group = area_name,
  #                                         color = area_name),
  #                 size = 2) +
  #       #color = entity_data$linecolour) +
  #       scale_color_brewer(palette="Set2") +
  #       #scale_colour_discrete(guide = 'none') +
  #       #scale_x_discrete(expand=c(0, 3)) +
  #       #scale_y_continuous(trans = 'log10') + 
  #       geom_dl(data = entity_data, aes(x = specimen_date,
  #                                       y = mean_week,
  #                                       label = area_name,color = area_name), method = list(dl.combine("last.points"),dl.trans(x=x+0.1),fontface = "bold"), cex = 0.8) +
  #       ylim(0,ymax) +
  #       #scale_y_continuous(trans = 'log10') + 
  #       ylab('number of daily COVID-19 positives') +
  #       xlab('date of test') +
  #       #geom_dl(aes(label = area_name), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
  #       theme_classic() +
  #       theme(strip.background  = element_blank(),
  #             strip.text.x = element_text(size = 12),
  #             axis.text.x = element_text(size=12),
  #             axis.title.x = element_text(size=14),
  #             axis.text.y = element_text(size=12),
  #             axis.title.y = element_text(size=14),
  #             legend.position = 'none')
  #     #legend.title = element_blank())
  #     
  #     
  #   } else if (mode == 'adj') {
  #     #plot mean_week
  #     ymax = max(entity_data$mean_week_pop)*1.10
  #     ggplot() +
  #       geom_line(data = data, aes(x = specimen_date,
  #                                  y = mean_week_pop,
  #                                  group = area_name),
  #                 size = 0.25,
  #                 color = 'grey') +
  #       
  #       geom_line(data = entity_data, aes(x = specimen_date,
  #                                         y = mean_week_pop,
  #                                         group = area_name,
  #                                         color = area_name),
  #                 size = 2) +
  #       #color = entity_data$linecolour) +
  #       scale_color_brewer(palette="Set2") +
  #       #scale_colour_discrete(guide = 'none') +
  #       #scale_x_discrete(expand=c(0, 3)) +
  #       geom_dl(data = entity_data, aes(x = specimen_date,
  #                                       y = mean_week_pop,
  #                                       label = area_name,color = area_name), method = list(dl.combine("last.points"),dl.trans(x=x+0.1),fontface = "bold"), cex = 0.8) +
  #       ylim(0,ymax) +
  #       ylab('number of daily COVID-19 positives pero 100k') +
  #       xlab('date of test') +
  #       #geom_dl(aes(label = area_name), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
  #       theme_classic() +
  #       theme(strip.background  = element_blank(),
  #             strip.text.x = element_text(size = 12),
  #             axis.text.x = element_text(size=12),
  #             axis.title.x = element_text(size=14),
  #             axis.text.y = element_text(size=12),
  #             axis.title.y = element_text(size=14),
  #             legend.position = 'none')
  #     #legend.title = element_blank())
  #   }
  # }

  #plotting multiple lines (up to 8) with preloaded data
    plot_cases_multiple_data <- function(data = data,
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
    last_update_date <- as.character(max(data$specimen_date))
    data <- data %>%
      filter(specimen_date >= (max(as.Date(data$specimen_date))-(7*timeframe)))
    
    #remove last 4 days in timeframe
    date_threshold <- 4
    data <- data %>%
      filter(specimen_date <= (max(as.Date(data$specimen_date))-date_threshold))
    #divide tibble into entity_data and rest_data
    entity_data <- data %>%
      filter(area_name %in% entity_names)
    #get maxdate
    maxdate <- as.Date(max(data[['specimen_date']]))
    if (mode == 'raw') {
      #plot mean_week
      #caption <- expression(paste('Example map with ', bold(last_update_date), aaaa))
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
                  size = 2,
                  show.legend = F) +
        scale_color_brewer(palette="Set2") +
        scale_y_continuous(expand=c(0, 0),limits = c(0,ymax)) +
        scale_x_date(expand=c(0,0),
                     date_labels = "%d %b",
                     breaks=seq(min(data[['specimen_date']]),max(data[['specimen_date']]),length.out = 6)) +
        geom_dl(data = entity_data, aes(x = specimen_date,
                                        y = mean_week,
                                        label = area_name,color = area_name), method = list(dl.combine("last.points"),dl.trans(x=x+0.1),fontface = "bold"), cex = 0.8) +
        
        #scale_y_continuous(trans = 'log10') + 
        ylab('daily positive cases') +
        xlab('date of test') +
        theme_classic() +
        #labs(subtitle='Seven day rolling average of new cases\nData from PH England', hjust = 1)+
        coord_cartesian(clip = "off")+
        labs(caption = bquote('\nLast updated at'~bold(.(last_update_date))),
             title = bquote('Seven day rolling average of new daily cases*\nData from PH England\n*Cases from last 4 days not included to correct for delay between test and report')) +
        theme(strip.background  = element_blank(),
              plot.caption = element_text(hjust = 1),
              plot.title = element_text(hjust = 1,size = 8),
              strip.text.x = element_text(size = 12),
              axis.text.x = element_text(size=12),
              axis.title.x = element_text(size=14),
              axis.text.y = element_text(size=12),
              axis.title.y = element_text(size=14),
              plot.margin=unit(c(3,3,3.5,3.2),"cm")) 
      #legend.title = element_blank())
      
      
    } else if (mode == 'adj') {
      #plot mean_week
      ymax = max(entity_data$mean_week_pop)*1.10
      ggplot() +
        geom_line(data = data, aes(x = specimen_date,
                                   y = mean_week_pop,
                                   group = area_name),
                  size = 0.25,
                  show.legend = F,
                  color = 'grey') +
        
        geom_line(data = entity_data, aes(x = specimen_date,
                                          y = mean_week_pop,
                                          group = area_name,
                                          color = area_name),
                  show.legend = F,
                  size = 2) +
        scale_color_brewer(palette="Set2") +
        scale_y_continuous(expand=c(0, 0),limits = c(0,ymax)) +
        scale_x_date(expand=c(0,0),
                     date_labels = "%d %b",
                     breaks=seq(min(data[['specimen_date']]),max(data[['specimen_date']]),length.out = 6)) +
        geom_dl(data = entity_data, aes(x = specimen_date,
                                        y = mean_week_pop,
                                        label = area_name,color = area_name), method = list(dl.combine("last.points"),dl.trans(x=x+0.1),fontface = "bold"), cex = 0.8) +
        ylab('daily positive cases per 100k') +
        xlab('date of test') +
        coord_cartesian(clip = "off")+
        labs(caption = bquote('\nLast updated at'~bold(.(last_update_date))),
             title = bquote('Seven day rolling average of new daily cases*\nData from PH England\n*Cases from last 4 days not included to correct for delay between test and report')) +
        theme_classic() +
        theme(strip.background  = element_blank(),
              plot.caption = element_text(hjust = 1),
              plot.title = element_text(hjust = 1,size = 8),
              strip.text.x = element_text(size = 12),
              axis.text.x = element_text(size=12),
              axis.title.x = element_text(size=14),
              axis.text.y = element_text(size=12),
              axis.title.y = element_text(size=14),
              plot.margin=unit(c(3,3,3.5,3.2),"cm"))
      #legend.title = element_blank())
    }
  }
  
  
  ###plot weekly totals of infection in a map
  plot_map_cases_week <- function(map, local_cases_data, week_ending_date){
    
    #select data to week ending on week_ending_date
    local_cases_data_sum <- as_tibble(local_cases_data) %>%
      filter(specimen_date <= as.Date(week_ending_date), specimen_date >= as.Date(week_ending_date)-7) %>% 
      dplyr::group_by(area_code) %>% 
      dplyr::summarize(weekly_cases_pop=sum(daily_cases_pop,na.rm = T))
    
    #add column of weekly_cases_pop to dataframe
    data_counties_simplified <- merge(map,
                                      local_cases_data_sum,
                                      by.x = 'LAD19CD',
                                      by.y = 'area_code',
                                      all.x = T)
    #plot the map
    plot(data_counties_simplified["weekly_cases_pop"],
         main = paste0('Total Cases (per 100k population) in week ending ',week_ending_date),
         #breaks = "quantile", nbreaks = 12,
         nbreaks = 20,
         pal = plasma,
         border = NA,
         bg = 'lightgrey')
    
  }
  
  ###plot weekly changes in totals of infection in a map
  plot_map_change_cases_week <- function(map, local_cases_data, week_ending_date){
    
    #select data to week ending on week_ending_date
    local_change_cases_data_week <- as_tibble(local_cases_data) %>%
      filter(specimen_date <= as.Date(week_ending_date), specimen_date >= as.Date(week_ending_date)-7) %>% 
      dplyr::group_by(area_code) %>% 
      dplyr::summarize(weekly_cases_pop=sum(daily_cases_pop,na.rm = T))
    
    local_cases_data_week1 <- as_tibble(local_cases_data) %>%
      filter(specimen_date <= as.Date(week_ending_date)-7, specimen_date >= as.Date(week_ending_date)-14) %>% 
      dplyr::group_by(area_code) %>% 
      dplyr::summarize(weekly_cases_pop_1=sum(daily_cases_pop,na.rm = T))
    
    local_change_cases_data_week <- merge(local_change_cases_data_week,
                                          local_cases_data_week1)
    local_change_cases_data_week$weekly_cases_change_pop <- local_change_cases_data_week$weekly_cases_pop-local_change_cases_data_week$weekly_cases_pop_1
    #add column of weekly_cases_pop to dataframe
    data_counties_simplified <- merge(map,
                                      local_change_cases_data_week,
                                      by.x = 'LAD19CD',
                                      by.y = 'area_code',
                                      all.x = T)
    #plot the map
    plot(data_counties_simplified["weekly_cases_change_pop"],
         main = paste0('Change in total cases (per 100k population) between week ending ',week_ending_date,' and week ending ',as.Date(week_ending_date)-7),
         #breaks = "quantile", nbreaks = 12,
         nbreaks = 20,
         #modifying wesanderson colour palette because I cannot make a coloramp with the package
         #pal = colorRampPalette(c("#3C9AB2","#78B7C5","#EBCC2A","#E1AF00","#F12400")),
         pal = colorRampPalette(c("blue","white","red")),
         #pal = inferno,
         border = NA,
         bg = 'lightgrey')
  }
  
  ##################END OF FUNCTIONS BLOCK##################
    region_cases_data <- get_cases_data(latest_csv_path = latest_csv_path,
                                       mode = 'region')
    
    local_cases_data <- get_cases_data(latest_csv_path = latest_csv_path,
                                             mode = 'local')
    data <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv") %>% 
      clean_names()
    
  ############## get names of regions for selection ##############
  RegionOptions <- data %>%
    filter(area_type == 'region') %>%
    filter(area_code %in% pop_data[['code']]) %>% 
    as.data.frame() %$%
    unique(area_name)
  updateSelectInput(session, "selRegionNames", choices = RegionOptions)
  
  # ############## get names of utla for selection ##############
  # UtlaOptions <- data %>%
  #   filter(area_type == 'utla') %>%
  #   as.data.frame() %$%
  #   unique(area_name)
  # updateSelectInput(session, "selUtlaNames", choices = UtlaOptions)
  # 
  # ############## get names of ltla for selection ##############
  # LtlaOptions <- data %>%
  #   filter(area_type == 'ltla') %>%
  #   as.data.frame() %$%
  #   unique(area_name)
  # updateSelectInput(session, "selLtlaNames", choices = LtlaOptions)
  # 
  ############## get names of localauthorities for selection ##############
  LocAuthOptions <- data %>%
    filter(area_type == 'ltla' | area_type == 'utla') %>%
    filter(area_code %in% pop_data[['code']]) %>% 
    as.data.frame() %$%
    unique(area_name)
  updateSelectInput(session, "selLocAuthNames", choices = LocAuthOptions)
  
  #rolling_mean <- rollify(mean, window = 7)
  output$RegionPlot <- renderPlot({
    
    if(!is.null(input$selRegionNames)){
      #LocAuthNames <- reactive({
      #  input$selLocAuthNames
      #}) %>% debounce(2000)
      
      selRegionAdj = input$selRegionAdj
      if(selRegionAdj) {RegionMode = 'adj'}else{RegionMode = 'raw'}
      RegionTime = as.numeric(input$selRegionTime)
      #LocAuthNames <- input$selLocAuthNames
      #LocAuthNames_d <- debounce(LocAuthNames, 2000)
      
      plot_cases_multiple_data(data = region_cases_data,
                            entity_names = input$selRegionNames,
                            timeframe = RegionTime,
                            mode = RegionMode)
      
    }})
  # output$RegionPlot <- renderPlot({
  #   selRegionAdj = input$selRegionAdj
  #   if(selRegionAdj) {RegionMode = 'adj'}else{RegionMode = 'raw'}
  #   RegionTime = as.numeric(input$selRegionTime)
  #   plot_cases_entity(entity_names = input$selRegionNames,
  #                                                 timeframe = RegionTime,
  #                                                 mode = RegionMode,
  #                                                 scale = 'region')})
  # output$UtlaPlot <- renderPlot({
  #   selUtlaAdj = input$selUtlaAdj
  #   if(selUtlaAdj) {UtlaMode = 'adj'}else{UtlaMode = 'raw'}
  #   UtlaTime = as.numeric(input$selUtlaTime)
  #   plot_cases_entity(entity_names = input$selUtlaNames,
  #                     timeframe = UtlaTime,
  #                     mode = UtlaMode,
  #                     scale = 'utla')})
  # 
  # output$LtlaPlot <- renderPlot({
  #   selLtlaAdj = input$selLtlaAdj
  #   if(selLtlaAdj) {LtlaMode = 'adj'}else{LtlaMode = 'raw'}
  #   LtlaTime = as.numeric(input$selLtlaTime)
  #   plot_cases_entity(entity_names = input$selLtlaNames,
  #                     timeframe = LtlaTime,
  #                     mode = LtlaMode,
  #                     scale = 'ltla')})
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
        
      plot_cases_multiple_data(data = local_cases_data,
                       entity_names = input$selLocAuthNames,
                       timeframe = LocAuthTime,
                       mode = LocAuthMode)
    
    }})
  output$MapWeeklyCases <- renderPlot({
    
    if(!is.null(input$selWeekEnding)){
      week_ending_date <- as.Date(input$selWeekEnding)
      plot_map_cases_week(map = map,
                          local_cases_data = local_cases_data,
                          week_ending_date = week_ending_date)
      
    }})
  
  output$MapChangeWeeklyCases <- renderPlot({
    
    if(!is.null(input$selChangeWeekEnding)){
      week_ending_date <- as.Date(input$selChangeWeekEnding)
      plot_map_change_cases_week(map = map,
                          local_cases_data = local_cases_data,
                          week_ending_date = week_ending_date)
      
    }})
    
    
}

    

  
