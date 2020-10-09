# DESCRIPTIVE VISUALIZATION: USA COUNTY NPI'S
# HIKMA DATABASE

# Library
library (tidyverse)
library (readr)

# Database: Hikma COVID19 County NPI database
NPI_County_Hikma <- read_csv("ConstituentDatabases/NPI_County_Hikma.csv")

# Create a subset of only Shelter and School variables 
# (concurrent variables with State NPI database)
Hikma_subset <- NPI_County_Hikma %>% select (-c(6,7,8,9,10,11,15,16,17,18,19,20,21,22,23))

# Number of secondary school closure policies per county in the database
NPISchoolDuplicates <- Hikma_subset %>% group_by(county_name, school) %>% 
  filter (school==TRUE) %>% filter ((n() > 1)) %>% distinct (county_name)
NumberSchoolDuplicates <- nrow (NPISchoolDuplicates)

# Number of secondary shelter policies per county in the database
NPIShelterDuplicates <- Hikma_subset %>% group_by(county_name, shelter) %>% 
  filter (shelter==TRUE) %>% filter ((n() > 1)) %>% distinct (county_name)
NumberShelterDuplicates <- nrow (NPIShelterDuplicates)

# Printing the number of secondary school and shelter policies per county 
a <- c("School Duplicates", "Shelter Duplicates")
b <- c (NumberSchoolDuplicates, NumberShelterDuplicates)
NPIDuplicates <- data.frame (a,b)
write.csv (NPIDuplicates, "Plots/CountyNPI_Hikma/Duplicate_NPIs.csv")

# Printing univariate NPIs in descending order
d <- c ("School Count", "Shelter Count")
e <- c (length (Hikma_subset$school [Hikma_subset$school == TRUE]), length (Hikma_subset$shelter [Hikma_subset$shelter == TRUE]))
NPIUnivariate <- data.frame (d,e)
write.csv (NPIUnivariate, "Plots/CountyNPI_Hikma/Univariate_NPIs.csv")


  
