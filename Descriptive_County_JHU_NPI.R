# DESCRIPTIVE VISUALIZATION: USA COUNTY NPI'S
# JHU DATABASE

# Library
library (tidyverse)
library (folderfun)
library (readr)

# Designating folder functions with paths 
setff ("plots", "/Dharma/Ideas&Projects/Academic/Projects/COVID19_StateInterventions/Code/Github_DVRN/
       2---Effect-of-State-Level-Interventions-on-Transmission-Dynamics-in-the-US/CountyNPI_JHU/Plots")

# Database: JHU COVID19 County NPI database
NPI_County_JHU <- read_csv("ConstituentDatabases/NPI_County_JHU.csv")

# Find, plot, and print the number of duplicate NPIs in every state/county
DuplicateCounty <- NPI_County_JHU %>% group_by(AREA_NAME) %>% filter(n() > 1)
ggplot (DuplicateCounty, aes (x=StatePolicy)) +
  geom_bar() + ggtitle ("Duplicate NPIs") + 
  ggsave ("DuplicateStateNPI.pdf", path = "plots", width = 10, height = 10)

# The duplicate NPI's are not true duplicates. 
# They are just different NPI's under that category. 
# E.g.: Gath Rest may be mentioned more than once because of different group sizes. 

# Ploting and printing univariate NPIs in ascending order
UnivariateNPI <- as.data.frame (sort(table(MandatedStatewideNPI$StatePolicy), decreasing = TRUE))
names (UnivariateNPI) <- c("StatePolicy", "Freq")             
ggplot (UnivariateNPI, aes (x=StatePolicy, y=Freq)) + 
  geom_bar(stat="identity") + ggtitle ("NPI Frequency") + 
  ggsave ("Univariate_NPI.pdf", path = "plots", width = 20, height = 10)


