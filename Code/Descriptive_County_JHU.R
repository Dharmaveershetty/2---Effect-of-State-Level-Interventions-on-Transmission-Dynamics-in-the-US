# DESCRIPTIVE VISUALIZATION: USA COUNTY NPI'S
# JHU DATABASE

# Library
library (tidyverse)
library (readr)

# Database: JHU COVID19 County NPI database
NPI_County_JHU <- read_csv("ConstituentDatabases/NPI_County_JHU.csv")

# Find and print the number of duplicate rows in the database
DuplicateCounty <- NPI_County_JHU %>% group_by(STATE, AREA_NAME) %>% filter(n() > 1)
write.csv (DuplicateCounty, "Plots/CountyNPI_JHU/Duplicate_rows.csv")
# There are two duplicate rows in the database. 

# Ploting and printing univariate NPIs in descending order
UnivariateNPI_county_JHU <- NPI_County_JHU %>% summarise_all(funs(sum(!is.na(.)))) %>%
  t() %>% as.data.frame () %>% rownames_to_column("NPI") %>% 
  slice (-1, -2, -3) 
ggplot (UnivariateNPI_county_JHU, aes (x=reorder (NPI, -V1), y=V1)) + 
  geom_bar(stat="Identity") + ggtitle ("NPI Frequency") + 
  ggsave ("Univariate_NPI.pdf", path = "Plots/CountyNPI_JHU", width = 20, height = 10)


