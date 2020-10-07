# DESCRIPTIVE VISUALIZATION: USA STATE NPI'S
# WSU DATABASE

# Library
library (tidyverse)
library (folderfun)
library(readr)

# Designating folder functions with paths 
setff ("plots", "/Dharma/Ideas&Projects/Academic/Projects/COVID19_StateInterventions/Code/Github_DVRN/
       2---Effect-of-State-Level-Interventions-on-Transmission-Dynamics-in-the-US/Plots/NPIStateWSU")

# Database: WSA COVID19 State NPI database
NPI_State_WSU <- read_csv("ConstituentDatabases/NPI_State_WSU.csv")

# Subset database with mandated and statewide policies
MandatedStatewideNPI <- NPI_State_WSU %>% filter (Mandate>0.1, StateWide>0.1)

# Find, plot, and print the number of duplicate NPIs in every state
DuplicateState <- MandatedStatewideNPI %>% group_by(StateName, StatePolicy) %>% filter(n() > 1)
DuplicateStateNPI <- as.data.frame (sort(table(DuplicateState$StatePolicy), decreasing = TRUE))
names (DuplicateStateNPI) <- c("StatePolicy", "Freq")  
ggplot (DuplicateStateNPI, aes (x=StatePolicy, y=Freq)) +
  geom_bar(stat="identity") + ggtitle ("Duplicate NPIs") + 
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








# Libraries
library (arsenal)
library (magrittr)

table1 <- tableby(interaction (StateName, StatePolicy) ~., data = MandatedStatewideNPI)
summary (table1, title = "MandatedStateWideNPI")
write2html(table1, "table1.html")



