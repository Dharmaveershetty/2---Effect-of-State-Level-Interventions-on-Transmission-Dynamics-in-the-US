# DESCRIPTIVE VISUALIZATION: USA STATE NPI'S
# WSU DATABASE

# Library
library (tidyverse)
library(readr)

# Database: WSA COVID19 State NPI database
NPI_County_JHU <- read_csv("ConstituentDatabases/NPI_County_JHU.csv")
NPI_County_Hikma <- read_csv("ConstituentDatabases/NPI_County_Hikma.csv")
NPI_County_Keystone <- read_csv("ConstituentDatabases/NPI_County_Keystone.csv")

# Compare common FIPS across the three databases
Intersect1 <- as.data.frame (intersect (NPI_County_JHU$FIPS, NPI_County_Hikma$fips))
names (Intersect1) <- c("FIPS")
Intersect <- as.data.frame (intersect (Intersect1$FIPS, NPI_County_Keystone$fips))
names (Intersect) <- c("FIPS")

# Compare unique FIPS in each of the three databases
UniqueJHU1 <- as.data.frame (setdiff (NPI_County_JHU$FIPS, NPI_County_Hikma$fips))
names (UniqueJHU1) <- c("FIPS")
UniqueJHU <- as.data.frame (setdiff (UniqueJHU1$FIPS, NPI_County_Keystone$fips))
names (UniqueJHU1) <- c("FIPS")
UniqueHikma1 <- as.data.frame (setdiff (NPI_County_Hikma$fips, NPI_County_JHU$FIPS))
names (UniqueHikma1) <- c("FIPS")
UniqueHikma <- as.data.frame (setdiff (UniqueHikma1$FIPS, NPI_County_Keystone$fips))
names (UniqueHikma) <- c("FIPS")
UniqueKeystone1 <- as.data.frame (setdiff (NPI_County_Keystone$fips, NPI_County_JHU$FIPS))
names (UniqueKeystone1) <- c("FIPS")
UniqueKeystone2 <- as.data.frame (setdiff (UniqueKeystone1$FIPS, NPI_County_Hikma$fips))
names (UniqueKeystone2) <- c("FIPS")
UniqueKeystone <- UniqueKeystone2 %>% filter (FIPS > 1000)

