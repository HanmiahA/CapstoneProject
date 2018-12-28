library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

##Uploaded data table containing crime rates for state and regions
FBICrime_State <- read_csv("FBI Crime Data - State and Region (Original).csv")
CrimeDF <- data.frame(FBICrime_State)

#Removed extra, unneeded rows and empty columns
CrimeDF <- CrimeDF[-c(1,2), -c(22:24)]

#Isolated area names from the Area column, filtered out the 2017 data, and 
#filled in the NA spaces in the Area column for 2017
area_df <- filter(CrimeDF, !is.na(Area))
area_names <- pull(area_df, Area)
NA2017 <- filter(CrimeDF, Year == 2017)
NA2017$Area <- area_names
CrimeDF <- NA2017

#Removed regions from Area so that the data only included the 50 states plus District of Columbia
CrimeDF <- CrimeDF %>% filter(!(Area %in% c("United States Total5, 6", "Puerto Rico", "Pacific", "Mountain", "West", "West South Central", "East South Central", "South Atlantic5,6", "South5,6", "West North Central", "East North Central", "Midwest", "Middle Atlantic", "Northeast", "New England")))
CrimeDF$Area[CrimeDF$Area == "District of Columbia5"] <- c("District of Columbia")

#Isolated the crime rates by removing the columns containing the number of offenses
CrimeDF <- CrimeDF[, -c(4,6,8,10,12,14,16,18,20)]

#Renamed Column Names
column_names <- c("State", "Year", "Population", "Violent Crime", "Murder/Nonnegligent Manslaughter", "Rape", "Robbery", "Aggravated Assault", "Property Crime", "Burglary", "Larceny", "Motor Vehicle Theft")
names(CrimeDF) <- column_names

#Gathered the crimes into one column and rate into another
#Arranged the data frame in alphabetical order by state and changed rate from character to numeric
CrimeDF <- gather(CrimeDF, "Offense", "Rate", 4:12)
CrimeDF <- arrange(CrimeDF, State)
CrimeDF$Rate <- gsub(",", "", CrimeDF$Rate)
class(CrimeDF$Rate) <- "numeric"

#Saved to CSV file
write.csv(CrimeDF, file = "CrimeByState&Offenses.csv")

##Created a separate data frame containing an average total crime rate per state
AvgStateRate <- CrimeDF %>% group_by(State) %>% summarise(Average_Rate = mean(Rate))
AvgStateRate <- data.frame(AvgStateRate)
colnames(AvgStateRate)[2] <- "Average Crime Rate"

#Loaded poverty data, created a data frame, and filtered out columns containing the states, poverty count, percentage of povery for all ages, and confidence interval
Poverty_State2017 <- read_csv("Poverty_State2017 (Original).csv")
PovertyDF <- data.frame(Poverty_State2017)
PovertyDF <- PovertyDF[c(4, 6, 9, 10, 13)]
names(PovertyDF) <- c("State", "All Ages Poverty Count", "Poverty Count 90% Confidence Interval", "All Ages Poverty Percent", "Poverty Percent 90% Confidence Interval")

#Joined the crime rate per state data with the poverty data, adjusted decimal places, and saved as a csv file
CrimePovertyDF <- left_join(AvgStateRate, PovertyDF, by = "State")
is.num <- sapply(CrimePovertyDF, is.numeric)
CrimePovertyDF[is.num] <- lapply(CrimePovertyDF[is.num], round, 1)
write.csv(CrimePovertyDF, file = "Crime&PovertyByState.csv")

