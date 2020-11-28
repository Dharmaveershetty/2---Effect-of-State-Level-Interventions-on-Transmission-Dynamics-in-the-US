# DESCRIPTIVE VISUALIZATION: USA STATE NPI'S
# WSU DATABASE

# Library
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


# Finding the number of unique counties in each database
nrow(JHU_county)
JHU_county %>% filter (grepl('county', AREA_NAME)) %>% distinct (STATE, AREA_NAME) %>% count()
Hikma_county %>% distinct (county_name) %>% count ()
Keystone_county %>% distinct (county, state) %>% count ()


# Compare common county FIPS across the three databases
Intersect1 <- as.data.frame (intersect (NPI_County_JHU$FIPS, NPI_County_Hikma$fips))
names (Intersect1) <- c("FIPS")
Intersect <- as.data.frame (intersect (Intersect1$FIPS, NPI_County_Keystone$fips))
names (Intersect) <- c("FIPS")
# (Check if there are any duplicates)
Intersect %>% group_by(FIPS) %>% filter(n()>1)        
# (No duplicates were found)
# (These only pertain to county FIPS because the state FIPS were...)
# (... designated with different values in the JHU and Keystone database...)
# (... and thus, not selected in the intersection values)
# (Common counties across the 3 databases = 544)


# Number of unique county FIPS in the JHU database (not found in the other databases)
UniqueJHU1 <- as.data.frame (setdiff (NPI_County_JHU$FIPS, NPI_County_Hikma$fips))
names (UniqueJHU1) <- c("FIPS")
UniqueJHU2 <- as.data.frame (setdiff (UniqueJHU1$FIPS, NPI_County_Keystone$fips))
names (UniqueJHU2) <- c("FIPS")
# (Removing the state level rows which have '000' as their leading digits (FIPS))
UniqueJHU2$FIPS2 <- UniqueJHU2$FIPS/1000
UniqueJHU2$FIPS3 <- round(UniqueJHU2$FIPS2)
UniqueJHU <- UniqueJHU2 %>% filter (!(FIPS3 == FIPS2))
# (Unique counties in the JHU database = )
nrow (UniqueJHU)

# Number of unique county FIPS in the Hikma database (not found in the other databases)
UniqueHikma1 <- as.data.frame (setdiff (NPI_County_Hikma$fips, NPI_County_JHU$FIPS))
names (UniqueHikma1) <- c("FIPS")
UniqueHikma <- as.data.frame (setdiff (UniqueHikma1$FIPS, NPI_County_Keystone$fips))
names (UniqueHikma) <- c("FIPS")



UniqueKeystone1 <- as.data.frame (setdiff (NPI_County_Keystone$fips, NPI_County_JHU$FIPS))
names (UniqueKeystone1) <- c("FIPS")
UniqueKeystone2 <- as.data.frame (setdiff (UniqueKeystone1$FIPS, NPI_County_Hikma$fips))
names (UniqueKeystone2) <- c("FIPS")
UniqueKeystone <- UniqueKeystone2 %>% filter (FIPS > 1000)


