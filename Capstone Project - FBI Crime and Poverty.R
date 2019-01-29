library(tidyverse)

##Uploaded data table containing crime rates for state and regions
FBICrime <- read_csv("FBI Crime Data - State and Region (Original).csv")

#Removed extra, unneeded rows and empty columns
#Removed regions from Area so that the data only included the 50 states plus District of Columbia
FBICrime <- FBICrime %>% fill(Area) %>% filter(Year != "Percent change", Year != "2016", !is.na(Year)) %>% 
  filter(!(Area %in% c("United States Total5, 6", "Puerto Rico", "Pacific", "Mountain", "West", "West South Central", "East South Central", "South Atlantic5,6", "South5,6", "West North Central", "East North Central", "Midwest", "Middle Atlantic", "Northeast", "New England"))) %>% 
  arrange(Area)


#Isolated the crime rates by removing the columns containing the number of offenses and renamed column names
column_names <- make.names(c("State", "Population", "Violent Crime", "Murder", "Rape", "Robbery", "Aggravated Assault", "Property Crime", "Burglary", "Larceny", "Motor Vehicle Theft"))
FBICrime <- FBICrime %>% select(Area, Population2, starts_with("X"), -X22, -X23, -X24) %>% setNames(column_names)

FBICrime$State[FBICrime$State %in% "District of Columbia5"] <- c("District of Columbia")
FBICrime$State[FBICrime$State %in% "North Carolina6"] <- c("North Carolina")

#Gathered the crimes into one column and rate into another
FBICrime <- gather(FBICrime, "Offense", "Rate", -State, -Population)
FBICrime$Rate <- gsub(",", "", FBICrime$Rate) %>% as.numeric(FBICrime$Rate)
FBICrime$Population <- gsub(",", "", FBICrime$Population) %>% as.numeric(FBICrime$Population)
FBICrime <- FBICrime %>% group_by(Offense) %>% mutate(region = tolower(State))

#Arranged the data frame in alphabetical order by state and replaced with abbreviations of the states (including District of Columbia as DC)
abbreviations <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
FBICrime$State <- abbreviations

#Filtered out Violent Crime and Property Crime into a separate data frame and removed Violent Crime and Property Crime
#from the original crime data frame
Violent_Crime <- filter(FBICrime, Offense == c("Violent.Crime"))
Property_Crime <- filter(FBICrime, Offense == c("Property.Crime"))
FBICrime <- FBICrime %>% filter(Offense != c("Violent.Crime")) %>% filter(Offense != c("Property.Crime"))
FBICrime2 <- bind_rows(Violent_Crime, Property_Crime) %>% arrange(State)



#Saved new data frame to CSV file
write.csv(FBICrime2, file = "Violent&PropertyCrime.csv")

#Created a new column within the original CrimeDF data frame to label the offenses as violent or property crime
Murder <- FBICrime %>% filter(Offense == c("Murder")) %>% mutate(Crime = "Violent")
Rape <- FBICrime %>% filter(Offense == c("Rape")) %>% mutate(Crime = "Violent")
Assault <- FBICrime %>% filter(Offense == c("Aggravated.Assault")) %>% mutate(Crime = "Violent")
Robbery <- FBICrime %>% filter(Offense == c("Robbery")) %>% mutate(Crime = "Violent")
Burglary <- FBICrime %>% filter(Offense == c("Burglary")) %>% mutate(Crime = "Property")
Larceny <- FBICrime %>% filter(Offense == c("Larceny")) %>% mutate(Crime = "Property")
Theft <- FBICrime %>% filter(Offense == c("Motor.Vehicle.Theft")) %>% mutate(Crime = "Property")

FBICrime <- bind_rows(Murder, Rape, Assault, Robbery, Burglary, Larceny, Theft) %>% arrange(State)

#Saved to CSV file
write.csv(FBICrime, file = "CrimeByState&Offenses.csv")

##Created a separate data frame containing an average total crime rate per state
AvgStateRate <- FBICrime %>% group_by(State) %>% summarise(Average.Crime.Rate = mean(Rate))

#Loaded poverty data, created a data frame, and filtered out columns containing the states, poverty count, percentage of povery for all ages, and confidence interval
Poverty_State2017 <- read_csv("Poverty_State2017 (Original).csv")
poverty_names <- make.names(c("State", "Poverty Count", "Poverty Count 90% Confidence Interval", "Poverty Rate", "Poverty Rate 90% Confidence Interval"))
Poverty <- Poverty_State2017[c(4, 6, 9, 10, 13)] %>% setNames(poverty_names)
Poverty$State <- abbreviations

#Joined the crime rate per state data with the poverty data, adjusted decimal places, and saved as a csv file
CrimePoverty <- left_join(AvgStateRate, Poverty, by = "State")
is.num <- sapply(CrimePoverty, is.numeric)
CrimePoverty[is.num] <- lapply(CrimePoverty[is.num], round, 1)
write.csv(CrimePoverty, file = "Crime&PovertyByState.csv")
