# DESCRIPTIVE VISUALIZATION: USA COUNTY NPI'S
# JHU DATABASE

# Library
library (tidyverse)
library (folderfun)

# Designating folder functions with paths 
setff ("plots", "/Dharma/Ideas&Projects/Academic/Projects/COVID19_StateInterventions/Code/Github_DVRN/
       2---Effect-of-State-Level-Interventions-on-Transmission-Dynamics-in-the-US/CountyNPI_JHU/Plots")

# Database: JHU COVID19 County NPI database


# Subset database with mandated and statewide policies
MandatedStatewideNPI <- USstatesCov19distancingpolicy %>% filter (Mandate>0.1, StateWide>0.1)

# Find, plot, and print the number of duplicate NPIs in every state
DuplicateState <- MandatedStatewideNPI %>% group_by(StateName, StatePolicy) %>% filter(n() > 1)
ggplot (DuplicateState, aes (x=StatePolicy)) +
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


