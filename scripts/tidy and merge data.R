library(tidyverse)
library(readxl)
library(openxlsx)
library(zoo)

#read in datasets
df_Adzuna_import <- read_csv("data/Adzuna_data_v6.csv")
df_Adzuna_occupation_GCCSA_import <- read_csv("data/Adzuna_data_v6_occupation_gccsa.csv")
df_IVI_import <- read_xlsx("data/IVI_DATA_regional - May 2010 onwards.xlsx", sheet = 2)

df_IVI_occupation_GCCSA <- df_IVI_import %>%
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
                                   "Sales Workers", "Machinery Operators and Drivers", "Labourers"))) %>%
  #group by all variables except count
  group_by_at(vars(-IVI_count)) %>%
  summarise(IVI_count = sum(IVI_count))
  
df_IVI <- df_IVI_occupation_GCCSA %>%
  group_by(Month) %>%
  summarise(IVI_count = sum(IVI_count))


df_Adzuna_occupation_GCCSA <- df_Adzuna_occupation_GCCSA_import %>%
  #merge year and month columns and turn into date
  unite(Month, c(Year,Month), sep = "-") %>%
  mutate(Month = as.Date(as.yearmon(Month))) %>%
  #rename occupation column and turn occupation in factor
  rename(ANZSCO_TITLE = occupation) %>%
  mutate(ANZSCO_TITLE = factor(ANZSCO_TITLE ,levels = c("Managers", "Professionals", "Technicians and Trades Workers",
                                                        "Community and Personal Service Workers",  "Clerical and Administrative Workers",
                                                        "Sales Workers", "Machinery Operators and Drivers", "Labourers"))) %>%
  #group by all variables except count
  group_by(Month, GCCSA, ANZSCO_TITLE) %>%
  summarise(Adzuna_count = sum(Adzuna_count))


df_Adzuna <- df_Adzuna_import %>%
  #merge year and month columns and turn into date
  unite(Month, c(Year,Month), sep = "-") %>%
  mutate(Month = as.Date(as.yearmon(Month)))


#calculate moving average
mav <- function(x,n=3){
  stats::filter(x,rep(1/n,n), sides=2)
  }

df_Adzuna_mav_occupation_GCCSA <- df_Adzuna_occupation_GCCSA %>%
  group_by(GCCSA, ANZSCO_TITLE) %>%
  drop_na()%>%
  mutate(Adzuna_mav_count = mav(Adzuna_count))

df_Adzuna_mav <- df_Adzuna %>%
  mutate(Adzuna_mav_count = mav(Adzuna_count))

    
df_merged_occupation_GCCSA <- df_Adzuna_mav_occupation_GCCSA %>%
  left_join(df_IVI_occupation_GCCSA, by = c("Month", "ANZSCO_TITLE", "GCCSA")) %>%
  drop_na() %>%
  ungroup()%>%
  mutate(GCCSA = factor(GCCSA ,levels = c("Greater Sydney", "Rest of NSW", "Greater Melbourne", "Rest of Vic.", "Greater Brisbane",
                                        "Rest of Qld", "Greater Adelaide", "Rest of SA", "Greater Perth", "Rest of WA", "Greater Hobart", "Rest of Tas.", "Greater Darwin", "Rest of NT", "Australian Capital Territory"), 
                       labels = c("Greater Sydney", "Rest of NSW", "Greater Melbourne", "Rest of Vic.", "Greater Brisbane", "Rest of Qld", "Greater Adelaide", 
                                  "Rest of SA", "Greater Perth", "Rest of WA", "Greater Hobart", "Rest of Tas.", "Greater Darwin", "Rest of NT", "Australian Capital Territory")))

df_merged_long_occupation_GCCSA <- df_merged_occupation_GCCSA %>%
  pivot_longer(c(Adzuna_count, IVI_count, Adzuna_mav_count), names_to = "source", values_to = "count") %>%
  mutate(source = factor(source))

#####
df_merged <- df_Adzuna_mav %>%
  left_join(df_IVI, by = c("Month")) %>%
  drop_na() %>%
  ungroup()

df_merged_long<- df_merged %>%
  pivot_longer(c(Adzuna_count, IVI_count, Adzuna_mav_count), names_to = "source", values_to = "count") %>%
  mutate(source = factor(source))


write_csv(df_merged, "data/jobs_data_merged.csv")
write_csv(df_merged, "data/jobs_data_merged_long.csv")
write_csv(df_merged_occupation_GCCSA, "data/jobs_data_merged_occupation_GCCSA.csv")
write_csv(df_merged_long_occupation_GCCSA, "data/jobs_data_merged_long_occupation_GCCSA.csv")


save(df_merged, df_merged_long, df_merged_occupation_GCCSA, df_merged_long_occupation_GCCSA, file="data/jobs_data_merged.Rdata")
