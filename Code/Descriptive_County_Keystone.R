# DESCRIPTIVE VISUALIZATION: USA COUNTY NPI'S
# KEYSTONE DATABASE

# Library
library (tidyverse)
library(readr)

# Database: Keystone COVID19 State NPI database
NPI_County_Keystone <- read_csv("ConstituentDatabases/NPI_County_Keystone.csv")

# Find and plot the number of duplicate NPIs in every cpunty
DuplicateCountyNPI <- NPI_County_Keystone %>% filter (!is.na(county)) %>% 
  group_by(state, county, npi) %>% filter(n() > 1)
DuplicateCountyNpiDf <- as.data.frame (sort(table(DuplicateCountyNPI$npi), decreasing = TRUE))
names (DuplicateCountyNpiDf) <- c("CountyPolicy", "Freq")  
ggplot (DuplicateCountyNpiDf, aes (x=CountyPolicy, y=Freq)) +
  geom_bar(stat="identity") + ggtitle ("Duplicate NPIs") + 
  ggsave ("DuplicateCountyNPI.pdf", path = "Plots/CountyNPI_Keystone", width = 20, height = 10)

# The duplicate NPI's are not true duplicates. 
# They are just NPI's that are re-issued again 

# Ploting and printing univariate NPIs in descending order
CountyNPI <- NPI_County_Keystone %>% filter (!is.na(county))
UnivariateNPI <- as.data.frame (sort(table(CountyNPI$npi), decreasing = TRUE))
names (UnivariateNPI) <- c("CountyPolicy", "Freq")             
ggplot (UnivariateNPI, aes (x=CountyPolicy, y=Freq)) + 
  geom_bar(stat="identity") + ggtitle ("NPI Frequency") + 
  ggsave ("Univariate_NPI.pdf", path = "Plots/CountyNPI_Keystone", width = 20, height = 10)


