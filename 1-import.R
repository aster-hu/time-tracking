# Source: https://github.com/Epsian/ics2csv/
# library(devtools)
# devtools::install_github("Epsian/ics2csv")

# Import data -------------------------------------------------------------

library(tidyverse)
library(ics2csv)

# Import gcal and row bind all files
gcal <- list.files(path = "gcal", pattern = "\\.ics$", full.names = TRUE) %>%
  map(ics2csv) %>%
  bind_rows() # dplyr::bind_rows do not require all data frames have the same number of columns

# Import ATracker and row bind all files
atracker <- list.files(path = "atracker", pattern = "\\.csv$", full.names = TRUE) %>%
  map(read.csv) %>%
  bind_rows() # dplyr::bind_rows do not require all data frames have the same number of columns


# Data exploration and cleansing ------------------------------------------

colnames(atracker)
colnames(gcal)

str(atracker)
str(gcal)

# atracker 
# Task.name, Start.time, End.time
# the start and end time is chr.
# 2023-06-23

atracker_dt <- atracker %>%
  select(Start.time, End.time, Task.name) %>% 
  # Convert start and end date to POSIXct date time format
  mutate(
    DTSTART = mdy_hms(Start.time, tz = "America/Toronto"),
    DTEND = mdy_hms(End.time, tz = "America/Toronto")) %>% 
  # Convert the naming convention to gcal tracking
  mutate(activity = case_match(Task.name,
                              "Casual reading" ~ "CasReading",
                              "Good times" ~ "GoodTimes",
                              "Personal development " ~ "Flourish",
                              "Professional development " ~ "Mastery",
                              "Social media" ~ "SocialMedia",
                              "System maintenance " ~ "SysMaint",
                              .default = Task.name)) %>% 
  # Extract the time tracking archetypes
  mutate(archetype = case_match(activity,
                                 c("Survival", "Home") ~ "Downtime",
                                 c("SysMaint", "GoodTimes", "CasReading") ~ "Sanity",
                                 c("Relationship", "Health") ~ "Wellness",
                                 c("SocialMedia", "Idle") ~ "Distraction",
                                 .default = activity)) %>% 
  # Do not count 2023-06-23
  filter(floor_date(DTSTART, unit = "day") <= as.POSIXct("2023-06-22", tzone = "America/Toronto"))

  
  unique(atracker_dt$SUMMARY) %>% sort()
  unique(atracker_dt$activity) %>% sort()
  # [1] "Casual reading"            "Good times"                "Health"                   
  # [4] "Home"                      "Idle"                      "Personal development "    
  # [7] "Professional development " "Relationship"              "Social media"             
  # [10] "Survival"                  "System maintenance "       "Work" 
  unique(atracker_dt$archetype) %>% sort()
  
gcal_dt <- gcal %>% 
  select(DTSTART, DTEND, SUMMARY) %>% 
  # Convert to local timezone
  mutate(
    DTSTART = lubridate::with_tz(DTSTART, tzone = "America/Toronto"),
    DTEND = lubridate::with_tz(DTEND, tzone = "America/Toronto")
  ) %>% 
  # Get the activity without detailed description
  mutate(activity = case_when(grepl("Blogging", SUMMARY) ~ "Blogging",
                              grepl("GoodTimes", SUMMARY) ~ "GoodTimes",
                              grepl("Home", SUMMARY) ~ "Home",
                              grepl("WebDev", SUMMARY) ~ "WebDev",
                               TRUE ~ SUMMARY)) %>% 
  # Extract time tracking archetypes
  mutate(archetype = case_match(activity,
                                 c("Survival", "Home", "Sleep") ~ "Downtime",
                                 c("SysMaint", "GoodTimes", "CasReading") ~ "Sanity",
                                 c("Relationship", "Strength", "Stamina") ~ "Wellness",
                                 c("WebDev", "Blogging", "Knitting") ~ "Flourish",
                                 c("DataViz", "R", "Python", "Stats") ~ "Mastery",
                                 c("SocialMedia", "Idle") ~ "Distraction",
                                 .default = activity)) %>% 
  # Do not count 2023-06-23, and do not count 2023-08-14
  filter(DTSTART >= "2023-06-24",
         DTSTART <= "2023-08-14")

unique(gcal_dt$SUMMARY) %>% sort()
unique(gcal_dt$activity) %>% sort()
unique(gcal_dt$archetype) %>% sort()
# [1] "Blogging"                                           
# [2] "Blogging - R and SQL"                               
# [3] "Blogging - SQL and R"                               
# [4] "CasReading"                                         
# [5] "DataViz"                                            
# [6] "GoodTimes"                                          
# [7] "GoodTimes - CoC"                                    
# [8] "GoodTimes - Keyboard"                               
# [9] "Home"                                               
# [10] "Home - Voting"                                      
# [11] "Idle"                                               
# [12] "Knitting"                                           
# [13] "Python"                                             
# [14] "R"                                                  
# [15] "Relationship"                                       
# [16] "Sleep"                                              
# [17] "SocialMedia"                                        
# [18] "Stamina"                                            
# [19] "Strength"                                           
# [20] "Survival"                                           
# [21] "SysMaint"                                           
# [22] "WebDev"                                             
# [23] "WebDev - also blogging (hugo stack renovation post)"
# [24] "WebDev - Learn org-mode"                            
# [25] "WebDev - Org-mode"                                  
# [26] "WebDev - orgmode"                                   
# [27] "Work" 

all_data <- bind_rows(gcal_dt, atracker_dt) %>% 
  mutate(dtduration = DTEND - DTSTART) %>%
  mutate(duration = as.duration(dtduration)) %>%
  mutate(dtdate = as.Date(DTSTART, tz = "America/Toronto")) %>%
  distinct()

str(all_data)
summary(all_data)

total <- all_data %>% 
  group_by(dtdate) %>% 
  summarize(total_hours = sum(duration)/3600)
View(total)

# Find date entries that are less than 10 total hours
# 2023-05-12, 2023-02-09, 2023-05-03

dates_to_filter <- as.Date(c("2023-05-12", "2023-02-09", "2023-05-03"))

all_data <- all_data %>%
  filter(!dtdate %in% dates_to_filter) %>% # Remove entries that are less than 10 total hours
  complete(dtdate = seq.Date(min(dtdate), max(dtdate), by = "day")) # Fill these entries with blank to avoid NA. source https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5 
# %>% 
  # group_by(dtdate) %>% 
  # mutate(day_hours = sum(duration/3600)) # Add total daily hours (it may exceed 24 because of the sleep hours overlapped)

rm(list = setdiff(ls(), "all_data")) # remove all objects except the main
