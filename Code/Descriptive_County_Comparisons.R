# DESCRIPTIVE VISUALIZATION: USA STATE NPI'S
# WSU DATABASE

# General Libraries
library(readr)
library (tidyverse)
library (lubridate)

# Database: WSA COVID19 State NPI database
NPI_State_WSU <- read.csv("ConstituentDatabases/NPI_State_WSU.csv")
NPI_County_JHU <- read_csv("ConstituentDatabases/NPI_County_JHU_dateconverted.csv")
# (The JHU database also has rows corresponding to a whole state...)
# (...whose FIP numbers end in '000')
# (The dates have already been converted from ordinal values to dates using Python)
NPI_County_Hikma <- read_csv("ConstituentDatabases/NPI_County_Hikma.csv")
# (The Hikma database has no rows corresponding to a whole state)
NPI_County_Keystone <- read_csv("ConstituentDatabases/NPI_County_Keystone.csv")
# (The Keystone database also has rows corresponding to a whole state...)
# (...whose FIP numbers are restricted to either single or double digits)

# Converting the date format into a common format across databases
NPI_State_WSU <- NPI_State_WSU %>%
  mutate (DateIssued = ymd(DateIssued),
          DateEnacted = ymd(DateEnacted),
          DateExpiry = ymd(DateExpiry),
          DateEased = ymd(DateEased),
          DateEnded = ymd(DateEnded),
          DateReexpanded1 = ymd(DateReexpanded1),
          DateReeased1 = ymd(DateReeased1),
          LastUpdated = ymd(LastUpdated))
NPI_County_JHU <- NPI_County_JHU [-1,]
NPI_County_JHU <- NPI_County_JHU %>%
  mutate (`stay at home` = as.Date(`stay at home`),
          `>50 gatherings` = as.Date(`>50 gatherings`),
          `>500 gatherings` = as.Date(`>500 gatherings`),
          `public schools` = as.Date(`public schools`),
          `restaurant dine-in` = as.Date (`restaurant dine-in`),
          `entertainment/gym` = as.Date(`entertainment/gym`),
          `federal guidelines` = as.Date(`federal guidelines`),
          `foreign travel ban` = as.Date(`foreign travel ban`),
          `stay at home rollback` = as.Date(`stay at home rollback`),
          `>50 gatherings rollback` = as.Date(`>50 gatherings rollback`),
          `>500 gatherings rollback` = as.Date(`>500 gatherings rollback`),
          `restaurant dine-in rollback` = as.Date(`restaurant dine-in rollback`),
          `entertainment/gym rollback` = as.Date(`entertainment/gym rollback`)
          )
#NPI_County_Hikma are already in the YYYY-MM-DD format
NPI_County_Keystone <- NPI_County_Keystone %>%
  mutate (start_date = mdy(start_date),
          end_date = mdy(end_date))


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

#-----------------------------------
# NPI CATEGORY: (1) SCHOOL CLOSURES
#-----------------------------------

# Selecting only 'school closure' policies for the individual databases
  # State-level
library (tidyverse)
WSU_state$FIPS <- (WSU_state$StateFIPS)*1000
Keystone_state <- Keystone_state %>% rename (FIPS = fips) %>% mutate (FIPS = FIPS*1000)
WSU_st_school <- WSU_state %>% filter (StatePolicy == "SchoolClose") %>% 
  select (StatePolicy, StateName, DateEnacted, FIPS) %>%
  rename_all(paste0, "_WSU") %>% rename (FIPS = FIPS_WSU) %>%
  distinct (FIPS, .keep_all = TRUE)
KS_st_school <- Keystone_state %>% filter (grepl("school", npi)) %>% 
  select (FIPS, npi, start_date) %>%
  rename_all(paste0, "_KS") %>% rename (FIPS = FIPS_KS)
JHU_st_school <- JHU_state %>% select (FIPS, `public schools`) %>% 
  rename_all(paste0, "_JHU") %>% rename (FIPS = FIPS_JHU)
  # County-level
Keystone_county <- Keystone_county %>% rename (FIPS = fips)
Hikma_county <- Hikma_county %>% rename (FIPS = fips)
JHU_c_school <- JHU_county %>% select (FIPS, `public schools`) %>% 
  rename_all(paste0, "_JHU") %>% rename (FIPS = FIPS_JHU)
KS_c_school <- Keystone_county %>% filter (grepl("school", npi)) %>% 
  select (FIPS, npi, start_date) %>%
  rename_all(paste0, "_KS") %>% rename (FIPS = FIPS_KS)
Hikma_c_school <- Hikma_county %>% select (FIPS, school_date) %>% distinct (FIPS, .keep_all = TRUE) %>%
  rename_all(paste0, "_H") %>% rename (FIPS = FIPS_H)


# Counting the number of 'school closure' date values in each database
 # State-level
colSums(!is.na(WSU_st_school))
colSums(!is.na(JHU_st_school))
colSums(!is.na(KS_st_school))
  # County-level
colSums(!is.na(JHU_c_school))
colSums(!is.na(Hikma_c_school))
colSums(!is.na(KS_c_school))


# Merging the databases for 'School Closures' by 'FIP' values
  # State-level
State_school <- as.data.frame (merge (merge (WSU_st_school, JHU_st_school, by="FIPS"), KS_st_school, by="FIPS")) 
State_school <- State_school[,-6] %>% rename(WSU = "DateEnacted_WSU",
                                        JHU = `public schools_JHU`, 
                                        KS = start_date_KS) 
  # County-level
County_school <- as.data.frame (merge (merge (merge (Intersect, JHU_c_school, by="FIPS"), 
                                              Hikma_c_school, by="FIPS"),
                                       KS_c_school, by="FIPS"))
County_school <- County_school %>% 
  rename (JHU = `public schools_JHU`, H = school_date_H, NPI = npi_KS, KS = start_date_KS) %>%
  select (FIPS, NPI, JHU, H, KS)



# Determining similarities in dates between the databases for 'School Closures'
  # State-wise
State_school <- State_school %>% mutate (SameDate = if_else(WSU-JHU==0 & WSU-KS==0, 1, 0))
sum(State_school$SameDate)
  # County-wise
County_school <- County_school %>% mutate (SameDate = if_else(JHU-H==0 & JHU-KS==0, 1, 0))
sum(County_school$SameDate, na.rm = TRUE)
  

# Determining intra-database dissimilaries (between state and county dates) for 'School Closures'
  # (As the presence of similarity could imply imputation of county values from state dates)
  # JHU Database
JHU_st_school <- JHU_state %>% select (FIPS, STATE, `public schools`) %>%
  rename (date = `public schools`) %>% rename_all(paste0, "_st") %>% rename (STATE = STATE_st)
JHU_c_school <- JHU_county %>% select (FIPS, STATE, `public schools`) %>%
  rename (date = `public schools`) %>% rename_all(paste0, "_ct") %>% rename (STATE = STATE_ct)
JHU_school <- as.data.frame (merge (JHU_c_school, JHU_st_school, by="STATE"))
JHU_school <- JHU_school %>% mutate (SameDate = if_else (date_st-date_ct==0,0,1))
sum(JHU_school$SameDate)
  # Keystone Database
KS_st_school <- Keystone_state %>% filter (grepl("school", npi)) %>% 
  select (FIPS, state, npi, start_date) %>%
  rename (date = start_date) %>%
  rename_all(paste0, "_st") %>% rename (state = state_st)
KS_c_school <- Keystone_county %>% filter (grepl("school", npi)) %>% 
  select (FIPS, state, npi, start_date) %>%
  rename (date = start_date) %>%
  rename_all(paste0, "_ct") %>% rename (state = state_ct)
KS_school <- as.data.frame (merge (KS_c_school, KS_st_school, by="state"))
KS_school <- KS_school %>% mutate (SameDate = if_else (date_st-date_ct==0,0,1))
sum(KS_school$SameDate)


# Determining dissimilaries between county Hikma dates and State WSU dates for 'School Closures'
  # (As the presence of similarity could imply imputation of county values from state dates)
WSU_state$FIPS <- (WSU_state$StateFIPS)*1000
WSU_st_school <- WSU_state %>% filter (StatePolicy == "SchoolClose") %>% 
  select (StatePolicy, StatePostal, DateEnacted, FIPS) %>%
  rename (State = StatePostal) %>%
  rename_all(paste0, "_WSU_st") %>% rename (State = State_WSU_st) %>%
  distinct (State, .keep_all = TRUE)
  # Adding the state acronyms to the Hikma database (using the JHU database FIPS values)
county_FIPS <- JHU_county %>% select (FIPS, STATE)
Hikma_c_school <- Hikma_county %>% select (FIPS, school_date) %>% distinct (FIPS, .keep_all = TRUE) %>%
  rename_all(paste0, "_H_ct") %>% rename (FIPS = FIPS_H_ct)
Hikma_c_school <- as.data.frame (merge (Hikma_c_school, county_FIPS, by="FIPS")) %>%
  rename (FIPS_H_ct = FIPS, State = STATE)
  # Merging the WST_st and Hikma_county school dataframes
WST_Hikma_school <- as.data.frame (merge(Hikma_c_school, WSU_st_school, by="State")) %>%
  rename (date_ct = school_date_H_ct, date_st = DateEnacted_WSU_st)
  # Comparing the dates
WST_Hikma_school <- WST_Hikma_school %>% mutate (SameDate = if_else (date_ct-date_st==0,0,1))
sum(WST_Hikma_school$SameDate, na.rm = TRUE)



#-----------------------------------
# NPI CATEGORY: (2) SHELTER-IN-PLACE
#-----------------------------------

# Selecting only 'shelter-in-place' policies for the individual databases
  # State-level
    # JHU: Shelter-in-place variable doesn't exist
    # WSU: Shelter-in-place variable doesn't exist
    # Hikma: State-level data not present
KS_st_shelter <- Keystone_state %>% filter (grepl("shelter_in_place", npi)) %>% 
  select (FIPS, npi, start_date) %>%
  rename_all(paste0, "_KS") %>% rename (FIPS = FIPS_KS)
  # County-level
    # WSU: County-level data doesnt exist
    # JHU: Shelter-in-place variable doesn't exist
Hikma_c_shelter <- Hikma_county %>% select (FIPS, shelter_enforcement_date, shelter_date) %>% distinct (FIPS, .keep_all = TRUE) %>%
  rename_all(paste0, "_H") %>% rename (FIPS = FIPS_H)
KS_c_shelter <- Keystone_county %>% filter (grepl("shelter_in_place", npi)) %>% 
  select (FIPS, npi, start_date) %>%
  rename_all(paste0, "_KS") %>% rename (FIPS = FIPS_KS)


# Counting the number of 'shelter-in-place' date values in each database
  # State-level
colSums(!is.na(KS_st_shelter))
  # County-level
colSums(!is.na(Hikma_c_shelter))
colSums(!is.na(KS_c_shelter))
colSums(!is.na(KS_c_school))


# Merging the databases for 'Shelter-in-place' by 'FIP' values
  # State-level
    # Only one db with state 'shelter-in-place' variable, so merge not applicable 
  # County-level
County_shelter <- as.data.frame (merge (merge (Intersect, Hikma_c_shelter, by="FIPS"), 
                                              KS_c_shelter, by="FIPS"))
County_shelter <- County_shelter %>% 
  rename (H_shelter_enforcement = 'shelter_enforcement_date_H', H_shelter = 'shelter_date_H', KS_shelter = 'start_date_KS') %>%
  select (H_shelter_enforcement, H_shelter, KS_shelter)



# Determining similarities in dates between the databases for 'Shelter Enforcements'
  # State-wise
    # Only one db with state 'shelter-in-place' variable, so merge not applicable 
  # County-wise
County_shelter <- County_shelter %>% mutate (SameDate = if_else(H_shelter-KS_shelter==0, 1, 0))
sum(County_shelter$SameDate, na.rm = TRUE)


# Determining intra-database dissimilaries (between state and county dates) for 'Shelter-in-Place'
  # (As the presence of similarity could imply imputation of county values from state dates)
  # Keystone Database
KS_st_shelter <- Keystone_state %>% filter (grepl("shelter_in_place", npi)) %>% 
  select (FIPS, state, npi, start_date) %>%
  rename (date = start_date) %>%
  rename_all(paste0, "_st") %>% rename (state = state_st)
KS_c_shelter <- Keystone_county %>% filter (grepl("shelter_in_place", npi)) %>% 
  select (FIPS, state, npi, start_date) %>%
  rename (date = start_date) %>%
  rename_all(paste0, "_ct") %>% rename (state = state_ct)
KS_shelter <- as.data.frame (merge (KS_c_shelter, KS_st_shelter, by="state"))
KS_shelter1 <- KS_shelter %>% mutate (SameDate = if_else (date_st-date_ct==0,0,1))
sum(KS_shelter1$SameDate, na.rm = TRUE)




#-----------------------------------
# NPI CATEGORY: (3) STAY-AT-HOME
#-----------------------------------

# Selecting only 'stay-at-home' policies for the individual databases
  # State-level
    # Hikma: State-level data and 'Stay at home' variable not present
    # Keystone: 'Stay-at-home' variable not present 
WSU_st_home <- WSU_state %>% filter (StatePolicy == "StayAtHome") %>% 
  select (StatePolicy, StateName, DateEnacted, FIPS) %>%
  rename_all(paste0, "_WSU") %>% rename (FIPS = FIPS_WSU) %>%
  distinct (FIPS, .keep_all = TRUE)
JHU_st_home <- JHU_state %>% select (FIPS, `stay at home`) %>% 
  rename_all(paste0, "_JHU") %>% rename (FIPS = FIPS_JHU)
  # County-level
    # WSU: County-level data doesnt exist
    # Keystone: Stay-at-home variable doesn't exist 
    # Hikma: Stay-at-home variable doesnt exist
JHU_c_home <- JHU_county %>% select (FIPS, `stay at home`) %>% 
  rename_all(paste0, "_JHU") %>% rename (FIPS = FIPS_JHU)


# Counting the number of 'shelter-in-place' date values in each database
  # State-level
colSums(!is.na(WSU_st_home))
colSums(!is.na(JHU_st_home))
  # County-level
colSums(!is.na(JHU_c_home))



# Determining intra-database dissimilaries (between state and county dates) for 'Stay-at-home'
  # (As the presence of similarity could imply imputation of county values from state dates)
  # JHU Database
JHU_st_home <- JHU_state %>% select (FIPS, STATE, `stay at home`) %>%
  rename (date = `stay at home`) %>% rename_all(paste0, "_st") %>% rename (STATE = STATE_st)
JHU_c_home <- JHU_county %>% select (FIPS, STATE, `stay at home`) %>%
  rename (date = `stay at home`) %>% rename_all(paste0, "_ct") %>% rename (STATE = STATE_ct)
JHU_home <- as.data.frame (merge (JHU_c_home, JHU_st_home, by="STATE"))
JHU_home <- JHU_home %>% mutate (SameDate = if_else (date_st-date_ct==0,0,1))
sum(JHU_home$SameDate)





