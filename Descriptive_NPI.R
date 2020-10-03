# DESCRIPTIVE VISUALIZATION: USA STATE NPI'S

# Database: WSA COVID19 State NPI database
USstatesCov19distancingpolicy <- read_csv("USstatesCov19distancingpolicy.csv")

# Library
library (tidyverse)

# Subset database with mandated and statewide policies
MandatedStatewideNPI <- USstatesCov19distancingpolicy %>% filter (Mandate>0.1, StateWide>0.1)

# Find, plot, and print the number of duplicate NPIs in every state
DuplicateState <- MandatedStatewideNPI %>% group_by(StateName, StatePolicy) %>% filter(n() > 1)
ggplot (DuplicateState, aes (x=StatePolicy)) +
  geom_bar() + ggtitle ("Duplicate NPIs") + 
  ggsave ("DuplicateStateNPI.pdf", width = 10, height = 10)

# The duplicate NPI's are not true duplicates. 
# They are just different NPI's under that category. 
# E.g.: Gath Rest may be mentioned more than once because of different group sizes. 

# Ploting and printing univariate NPIs in ascending order
UnivariateNPI <- as.data.frame (sort(table(MandatedStatewideNPI$StatePolicy)))
names (UnivariateNPI) <- c("StatePolicy", "Freq")             #Changing column heading names
ggplot (UnivariateNPI, aes (x=StatePolicy, y=Freq)) + 
  geom_bar(stat="identity") + ggtitle ("NPI Frequency") + 
  ggsave ("Univariate_NPI.pdf", width = 20, height = 10)


