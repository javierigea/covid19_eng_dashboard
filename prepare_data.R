library(rgdal)
library(rmapshaper)


#data preparation

####generate map
#from https://geoportal.statistics.gov.uk/datasets/1d78d47c87df4212b79fe2323aae8e08_0
uk_countries <- readOGR(dsn = "~/Downloads/Local_Authority_Districts__December_2019__Boundaries_UK_BFC-shp/", layer = "Local_Authority_Districts__December_2019__Boundaries_UK_BFC")

#subset to area codes with data
latest_csv_path <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
data <- read_csv(latest_csv_path) %>%
  clean_names()

data_counties <- uk_countries[as.character(uk_countries$lad19cd) %in% data$area_code,]

#simplify shapes to plot quicker
data_counties_simplified <- ms_simplify(input = data_counties)
#save to data folder
data_counties_simplified <- st_as_sf(data_counties_simplified)
saveRDS(data_counties_simplified,'./data/data_counties_simplified.RDS')

####get pop size estimates
#open 
pop_xls_path <- './data/ukmidyearestimates20192020ladcodes.xls'
pop_data <- read_excel(pop_xls_path, sheet = 6, skip = 4)
pop_data <- pop_data %>%
  clean_names() %>% 
  select(code, all_ages)
  
  
#have to add Aylesbury Vale, SouthBucks and Wycombe which were are now Buckinghamshire
pop_csv_new <- read_csv('~/Downloads/1ec4bbfe-6a89-4c5f-84b0-ed021a072037.csv') %>% 
  clean_names() %>% 
  filter(time == 2018 & age == 'Total' & sex == 'All') %>% 
  select(admin_geography, v4_0) %>% 
  dplyr::rename(code = admin_geography, all_ages = v4_0)
#add missing rows to pop_data
pop_data <- bind_rows(pop_data,pop_csv_new[!pop_csv_new$code%in%pop_data$code,])
write_csv(pop_data,'./data/pop_data.csv')
  
  
