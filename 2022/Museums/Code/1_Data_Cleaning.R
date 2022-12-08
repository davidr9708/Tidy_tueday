# Libraries ####
library(pacman)
p_load(tidytuesdayR,
       tidyverse   ,
       broom       ,
       lmtest
       )

# Get data ####
raw_data <- tidytuesdayR::tt_load('2022-11-22')[[1]]

# Clean data to explore ####
exploratory_data <- raw_data %>%
     ## Separating columns
     separate(Year_closed, 
              into = c('low_closing', 
                       'high_closing')) %>%
     separate(Year_opened, 
              into = c('low_opening', 
                       'high_opening')) %>%  
     separate(Admin_area, 
              into  = c('Nothing', 
                        'Country'), 
              sep   = '/', 
              extra = 'drop') %>%
     separate(Subject_Matter, 
              into  = c('Subject'), 
              sep   = '-', 
              extra = 'drop') %>%
     separate(Governance, 
              into  = c('Governance'), 
              sep   = '-', 
              extra = 'drop') %>%
     ## Filtering to have the population of interest
     filter(high_closing > 2018, 
            Size != 'unknown', 
            Country == 'England') %>%
     mutate(Size = ifelse(Size == 'small', 
                          1, 
                          0))

# Save data
if(!dir.exists('Data')){
  dir.create('Data')
}
save(file = 'Data/Exploratory_data.RData', exploratory_data)
rm(list = ls())