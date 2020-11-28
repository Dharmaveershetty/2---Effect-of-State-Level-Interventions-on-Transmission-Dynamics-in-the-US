# DESCRIPTIVE VISUALIZATION: USA STATE NPI'S
# WSU DATABASE

# General Libraries
library(readr)
library (tidyverse)


# Database: WSA COVID19 State NPI database
NPI_State_WSU <- read.csv("ConstituentDatabases/NPI_State_WSU.csv")
NPI_County_JHU <- read_csv("ConstituentDatabases/NPI_County_JHU.csv")
# (The JHU database also has rows corresponding to a whole state...)
# (...whose FIP numbers end in '000')
NPI_County_Hikma <- read_csv("ConstituentDatabases/NPI_County_Hikma.csv")
# (The Hikma database has no rows corresponding to a whole state)
NPI_County_Keystone <- read_csv("ConstituentDatabases/NPI_County_Keystone.csv")
# (The Keystone database also has rows corresponding to a whole state...)
# (...whose FIP numbers are restricted to either single or double digits)


# Filtering out the state-level and county-level rows in all the datasets
NPI_County_JHU$FIPS <- as.numeric (NPI_County_JHU$FIPS)
JHU_state <- NPI_County_JHU %>% filter ((FIPS/1000) == round(FIPS/1000))
JHU_county <- NPI_County_JHU %>% filter (!((FIPS/1000) == round(FIPS/1000)))
Hikma_county <- NPI_County_Hikma
Keystone_state <- NPI_County_Keystone %>% filter (fips < 1000)
Keystone_county <- NPI_County_Keystone %>% filter (fips > 1000)
WSU_state <- NPI_State_WSU


# Finding the number of unique counties in each database
nrow(JHU_county)
JHU_county %>% filter (grepl('county', AREA_NAME)) %>% distinct (STATE, AREA_NAME) %>% count()
Hikma_county %>% distinct (county_name) %>% count ()
Keystone_county %>% distinct (county, state) %>% count ()


# Compare common county FIPS across the three databases
Intersect <- as.data.frame(intersect (intersect (JHU_county$FIPS, Hikma_county$fips), Keystone_county$fips)) %>% rename (FIPS=1)
nrow (Intersect)
# (Check if there are any duplicates)
Intersect %>% group_by(FIPS) %>% filter(n()>1)        
# (No duplicates were found)


# Number of unique county FIPS in the individual databases 
UniqueJHU <- as.data.frame (setdiff (setdiff (JHU_county$FIPS, Hikma_county$fips), Keystone_county$fips)) %>% rename (FIPS=1)
nrow (UniqueJHU)   
UniqueHikma <- as.data.frame (setdiff (setdiff (Hikma_county$fips, Keystone_county$fips), JHU_county$FIPS)) %>% rename (FIPS=1)
nrow (UniqueHikma)
UniqueKeystone <- as.data.frame (setdiff (setdiff (Keystone_county$fips, Hikma_county$fips), JHU_county$FIPS)) %>% rename (FIPS=1)
nrow (UniqueKeystone)

# Determining state-wise similarity between the databases that records state-wise data
## Databases tested: WSU, JHU, and Keystone
## NPI category used for testing: school closures
WSU_state$FIPS <- (WSU_state$StateFIPS)*1000
Keystone_state <- Keystone_state %>% rename (FIPS = fips) %>% FIPS*1000

WSU_state$FIPS <- (WSU_state$StateFIPS)*1000
State_School <- as.data.frame(intersect )

  
Intersect <- as.data.frame(intersect (intersect (JHU_county$FIPS, Hikma_county$fips), Keystone_county$fips)) %>% rename (FIPS=1)
nrow (Intersect)
