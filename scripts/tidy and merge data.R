library(tidyverse)
library(readxl)
library(openxlsx)
library(zoo)

#read in datasets
df_Adzuna_import <- read_csv("data/Adzuna_data_v6.csv")
df_IVI_import <- read_xlsx("data/IVI_DATA_regional - May 2010 onwards.xlsx", sheet = 2, header = )

df_IVI <- df_IVI_import %>%
  #select major occupational groups
  filter(ANZSCO_CODE %in% c(1, 2, 3, 4, 5, 6, 7, 8)) %>%
  #convert location to GCCSA equivalent
  mutate(GCCSA = case_when(Region %in% c("Blue Mountains, Bathurst & Central West NSW", 
                                         "Dubbo & Western NSW", 
                                         "Gosford & Central Coast", 
                                         "Illawarra & South Coast", 
                                         "NSW North Coast", 
                                         "Newcastle & Hunter", 
                                         "Riverina & Murray", 
                                         "Southern Highlands & Snowy", 
                                         "Tamworth and North West NSW") ~ "Rest of NSW",
                           Region == "Sydney" ~ "Greater Sydney",
                           Region %in% c("Ballarat & Central Highlands", 
                                         "Bendigo & High Country", 
                                         "Geelong & Surf Coast", 
                                         "Gippsland",
                                         "Wimmera & Western") ~ "Rest of Vic.",
                           Region == "Melbourne" ~ "Greater Melbourne",
                           Region %in% c("Central Queensland",
                                         "Far North Queensland",
                                         "Gold Coast",
                                         "Outback Queensland",
                                         "Sunshine Coast",
                                         "Toowoomba and South West QLD") ~ "Rest of Qld",
                           Region == "Brisbane" ~ "Greater Brisbane",
                           Region %in% c("Fleurieu Peninsula & Murray Mallee",
                                         "Port Augusta & Eyre Peninsula",
                                         "Yorke Peninsula & Clare Valley") ~ "Rest of SA",
                           Region == "Adelaide" ~ "Greater Adelaide",
                           Region %in% c("Goldfields & Southern WA",
                                         "Pilbara & Kimberley",
                                         "South West WA") ~ "Rest of WA",
                           Region == "Perth" ~ "Greater Perth",
                           Region == "Regional Northern Territory" ~ "Rest of NT",
                           Region == "Darwin" ~ "Greater Darwin",
                           Region %in% c("Launceston and Northeast Tasmania",
                                         "North West Tasmania") ~ "Rest of Tas.",
                           Region == "Hobart & Southeast Tasmania" ~ "Greater Hobart",
                           Region == "Canberra & ACT" ~ "Australian Capital Territory"
                           )) %>%
  select(-c(Level, State, Region)) %>%
  #turn into long format  
  pivot_longer(-c(ANZSCO_CODE, ANZSCO_TITLE, GCCSA), names_to = "Month", values_to = "IVI_count") %>%
  #convert excel date number to R date
  mutate(Month = convertToDate(Month)) %>%
  #turn occupation in factor and change labes (they are mices upper and lower case)
  mutate(ANZSCO_TITLE = factor(ANZSCO_TITLE ,levels = c("MANAGERS", "PROFESSIONALS", "TECHNICIANS AND TRADES WORKERS", 
                                                        "COMMUNITY AND PERSONAL SERVICE WORKERS", "CLERICAL AND ADMINISTRATIVE WORKERS",
                                                        "SALES WORKERS", "MACHINERY OPERATORS AND DRIVERS", "LABOURERS"), labels =
                                 c("Managers", "Professionals", "Technicians and Trades Workers",
                                   "Community and Personal Service Workers",  "Clerical and Administrative Workers",
                                   "Sales Workers", "Machinery Operators and Drivers", "Labourers")))

df_Adzuna <- df_Adzuna_import %>%
  #merge year and month columns and turn into date
  unite(Month, c(Year,Month), sep = "-") %>%
  mutate(Month = as.Date(as.yearmon(Month))) %>%
  #rename occupation column and turn occupation in factor
  rename(ANZSCO_TITLE = occupation) %>%
  mutate(ANZSCO_TITLE = factor(ANZSCO_TITLE ,levels = c("Managers", "Professionals", "Technicians and Trades Workers",
                                                        "Community and Personal Service Workers",  "Clerical and Administrative Workers",
                                                        "Sales Workers", "Machinery Operators and Drivers", "Labourers")))
    
df <- df_Adzuna %>%
  left_join(df_IVI, by = c("Month", "ANZSCO_TITLE", "GCCSA"))
    
