# IST 687
# Group 2 Final Project
# Student names: Sidney Lanier, Ryan Reed, Abhiram Gopal, Renjie Zhu, Scott Kessel
# Association Rules for Final Project 

dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

# Set working directory *edit to reflect your computer's local directory)
# Change to the folder containing your 
setwd("~/Documents/Pip's")

install.packages("RCurl") 
install.packages("bitops")
install.packages("jsonlite") 
install.packages("tidyverse") 

library(tidyverse)
library(bitops)
library(RCurl)
library(jsonlite)

# Import dataset from URL and input into dataframe "df" *update url if necessary)
df <- getURL("https://s3.us-east-1.amazonaws.com/blackboard.learn.xythos.prod/5956621d575cd/8614406?response-content-disposition=inline%3B%20filename%2A%3DUTF-8%27%27fall2019-survey-M02%25281%2529.json&response-content-type=application%2Fjson&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20191208T212813Z&X-Amz-SignedHeaders=host&X-Amz-Expires=21600&X-Amz-Credential=AKIAIL7WQYDOOHAZJGWQ%2F20191208%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=f69ae50b3fae8c453f135537d2bd29ecc01b417e8b2aac813ceeddf803077883")

df <- jsonlite::fromJSON(df) # Convert downloaded data to JSON format
View(df)
sum(is.na(df$Flight.time.in.minutes))

#Replace the NAs value in the column "Flight.time.in.minutes" by the mean of the flight minutes 
#of flights where the origin city and destination city are the same
list_na_flight_time <- which(is.na(df$Flight.time.in.minutes)==TRUE)
no_value <- df[list_na_flight_time,]
no_value1 <- data.frame(no_value)
for (i in seq(1,length(list_na_flight_time))){
  no_value1$Flight.time.in.minutes[i]<- floor(mean(df$Flight.time.in.minutes[df$Flight.time.in.minutes[df$Destination.City ==df$Destination.City[i]  & df$Origin.City==df$Origin.City[i]]],na.rm=TRUE))
}

#After that, still get one missing value
#One flight from "Milwaukee, WI" to"Minneapolis, MN"
still_missing <-which(is.na(no_value1[,25]))
no_value1$Destination.City[still_missing]
no_value1$Origin.City[still_missing]

#I searched the google fight, find the flight minutes is about 82minutes.
#so I replaced the na value by 82
no_value1[103,25] <- 82

df$Flight.time.in.minutes[which(is.na(df$Flight.time.in.minutes))] <- no_value1[,25]

# Remove NAs from Arrival.Delay.in.Minutes $ Departure.Delay.in.Minutes 
#create two data frame to store the NA value of two columns
list_na_arrive_delay <- which(is.na(df$Arrival.Delay.in.Minutes)==TRUE)
list_na_departure_delay <-which(is.na(df$Departure.Delay.in.Minutes)==TRUE)
no_value_arrive <- df[list_na_arrive_delay,]
no_value_departure <- df[list_na_departure_delay,]

#Replace the NAs value in the columns "Departure.Delay.in.Minutes"  and "Arrival.Delay.in.Minutes" 
#by the mean of the flight minutes of flights where the origin city and destination city are the same
for (i in seq(1,length(list_na_arrive_delay))){
  no_value_arrive$Arrival.Delay.in.Minutes[i]<- floor(mean(df$Arrival.Delay.in.Minutes
                                                           [df$Arrival.Delay.in.Minutes
                                                             [df$Destination.City ==df$Destination.City[i]                                                                                                                   & df$Origin.City==df$Origin.City[i]]],na.rm=TRUE))
}
for (i in seq(1,length(list_na_departure_delay))){
  no_value_departure$Departure.Delay.in.Minutes[i]<- floor(mean(df$Departure.Delay.in.Minutes[df$Departure.Delay.in.Minutes[df$Destination.City ==df$Destination.City[i] 
                                                                                                                            & df$Origin.City==df$Origin.City[i]]],na.rm=TRUE))
  
}

#Replace the remaining NAs value by the mean of delay minutes of whole data set
no_value_arrive[which(is.na(no_value_arrive[,23])),23] <- mean(df$Arrival.Delay.in.Minutes,na.rm=TRUE)
no_value_departure[which(is.na(no_value_departure[,22])),22] <- mean(df$Departure.Delay.in.Minutes,na.rm=TRUE)

#Replace the NAs value in df using these two data frame which has replaced all NAs values
df$Arrival.Delay.in.Minutes[which(is.na(df$Arrival.Delay.in.Minutes))]<-
  no_value_arrive[,23]
df$Departure.Delay.in.Minutes[which(is.na(df$Departure.Delay.in.Minutes))]<- no_value_departure[,22]

sum(is.na(df$Arrival.Delay.in.Minutes))
sum(is.na(df$Departure.Delay.in.Minutes))


# Convert year.of.First.Flight column to year/date format
df$Year.of.First.Flight <-as.Date(as.character(df$Year.of.First.Flight), format = "%Y")
df$Year.of.First.Flight <- format(as.Date(df$Year.of.First.Flight), "%Y")

############## Data Cleanup for Association Rules ##################

# Convert Type.of.Travel to a three-level factor 
df$Type.of.Travel <- as.factor(df$Type.of.Travel)

# Convert Flight.cancelled to factor
df$Flight.cancelled <- as.factor(df$Flight.cancelled)

# Convert Airline.Status to factor 
df$Airline.Status <- as.factor(df$Airline.Status)

# Convert Gender to factor 
df$Gender <- as.factor(df$Gender)

# Conver Class to a factor 
df$Class <- as.factor(df$Class)

# Convert Likelihood.to.recommend to a scale from low to bad to passive to awesome 
df$Likelihood.to.recommend <- as.character(df$Likelihood.to.recommend, 0-4)
df$Likelihood.to.recommend = 
  ifelse(df$Likelihood.to.recommend >=9,"promotor",
         ifelse(df$Likelihood.to.recommend >=7, "passive",
                ifelse(df$Likelihood.to.recommend >=0, "detractor")))

# Convert new likelihood.to.recommend to a factor
df$Likelihood.to.recommend <- as.factor(df$Likelihood.to.recommend)

# Review to make sure it was converted to a 3-level factor
View(df$Likelihood.to.recommend)
str(df$Likelihood.to.recommend)
 
# Convert Age to a scale from youth to adult to sen ior 
df$Age <- as.character(df$Age,0-4)
df$Age =
  ifelse(df$Age>=50, "senior",
  ifelse(df$Age>=18, "adult",
  ifelse(df$Age>=1, "youth")))

# Convert new age levels to a factor 
df$Age <- as.factor(df$Age)

# Review to make sure it was converted to a 3-level factor
str(df$Age)
View(df$Age)

#################################### Association Rules ##########################
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

# Run association rules functions on 6 airlines that were determined to have significant 
# amount of valuable responses (Cheapseats Airlines Inc.,FlyFast Airways Inc.,
# Northwest Business Airlines Inc.,Oursin Airlines Inc.,Sigma Airlines Inc.,
# Southeast Airlines Co.). Include variables like age, status, and type of travel that 
# had significant outcomes from the linear analysis. 



################ Run Apriori() on each individual airline to test for trends #########
########### EnjoyFlying Air Services ###################
# Select columns to be input into new df and filter by airline name 
dfEnjoy <- df %>% 
  select(Type.of.Travel,Flight.cancelled,Airline.Status,Gender, Class, Likelihood.to.recommend) %>%
  filter(df$Partner.Name == "EnjoyFlying Air Services") 

# Convert the dataframe to a matrix 
dfEnjoyX <- as(dfEnjoy,"transactions")

# Run apriori() to find variables related to detractors
ruleset <- apriori(dfEnjoyX, 
                   #Give parameters for the list: 
                   #use a low support of .005 and a confidence of .5
                   parameter=list(support=0.08,confidence=0.5),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is detractor 
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=detractor")))

inspectDT(ruleset)

# Run again for "passive" 
ruleset <- apriori(dfEnjoyX, 
                   #Give parameters for the list: 
                   #use a low support of .005 and a confidence of .5
                   parameter=list(support=0.005,confidence=0.5),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is passive
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=passive")))

inspectDT(ruleset)

########### FlyHere Airways ###################
# Select columns to be input into new df and filter by airline name 
dfFlyHere <- df %>% 
  select(Type.of.Travel,Flight.cancelled,Airline.Status,Gender, Class, Likelihood.to.recommend) %>% 
  filter(df$Partner.Name == "FlyHere Airways") 

# Convert the dataframe to a matrix 
dfFlyHereX <- as(dfFlyHere,"transactions")

# Run apriori() to find variables related to detractors
ruleset <- apriori(dfFlyHereX, 
                   #Give parameters for the list: 
                   #use a low support of .005 and a confidence of .5
                   parameter=list(support=0.08,confidence=0.6),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is detractor
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=detractor")))

inspectDT(ruleset)


# Run again for "passive" 
ruleset <- apriori(dfFlyHereX, 
                   #Give parameters for the list: 
                   #use a low support of .002 and a confidence of .38
                   parameter=list(support=0.02,confidence=0.38),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is passive
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=passive")))

inspectDT(ruleset)

# Convert the dataframe to a matrix 
dfFlyHereX <- as(dfFlyHere,"transactions")

# Run apriori() to find variables related to detractors
ruleset <- apriori(dfFlyHereX, 
                   #Give parameters for the list: 
                   #use a low support of .1 and a confidence of .6
                   parameter=list(support=0.1,confidence=0.6),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is promotor
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=promotor")))

inspectDT(ruleset)

###########  GoingNorth Airlines Inc. ###################

# Select columns to be input into new df and filter by airline name 
dfGoingNorth <- df %>% 
  select(Type.of.Travel,Flight.cancelled,Airline.Status,Gender, Class, Likelihood.to.recommend) %>% 
  filter(df$Partner.Name == "GoingNorth Airlines Inc.") 

# Convert the dataframe to a matrix 
dfGoingNorthX <- as(dfGoingNorth,"transactions")

# Run apriori() to find variables related to detractors
ruleset <- apriori(dfGoingNorthX, 
                   #Give parameters for the list: 
                   #use a low support of .08 and a confidence of .7
                   parameter=list(support=0.08,confidence=0.7),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is detractor
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=detractor")))

inspectDT(ruleset)

# Run again for passive
ruleset <- apriori(dfGoingNorthX, 
                   #Give parameters for the list: 
                   #use a low support of .05 and a confidence of .45
                   parameter=list(support=0.05,confidence=0.45),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is passive
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=passive")))

inspectDT(ruleset)

###########  OnlyJets Airlines Inc. ###################

# Select columns to be input into new df and filter by airline name 
dfOnlyJets <- df %>% 
  select(Type.of.Travel,Flight.cancelled,Airline.Status,Gender, Class, Likelihood.to.recommend) %>% 
  filter(df$Partner.Name == "OnlyJets Airlines Inc.") 

# Convert the dataframe to a matrix 
dfOnlyJetsX <- as(dfOnlyJets,"transactions")

# Run apriori() to find variables related to detractors
ruleset <- apriori(dfOnlyJetsX, 
                   #Give parameters for the list: 
                   #use a low support of .1 and a confidence of .6
                   parameter=list(support=0.1,confidence=0.6),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is detractor
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=detractor")))

inspectDT(ruleset)

# Run again for passive 
ruleset <- apriori(dfOnlyJetsX, 
                   #Give parameters for the list: 
                   #use a low support of .02 and a confidence of .5
                   parameter=list(support=0.02,confidence=0.5),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is passive
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=passive")))

inspectDT(ruleset)

########### Paul Smith Airlines Inc. ###################

# Select columns to be input into new df and filter by airline name 
dfPaulSmith <- df %>% 
  select(Type.of.Travel,Flight.cancelled,Airline.Status,Gender, Class, Likelihood.to.recommend) %>%
  filter(df$Partner.Name == "Paul Smith Airlines Inc.") 

# Convert the dataframe to a matrix 
dfPaulSmithX <- as(dfPaulSmith, "transactions")

# # Run apriori() to find variables related to detractors
ruleset <- apriori(dfPaulSmithX, 
                   #Give parameters for the list: 
                   #use a low support of .06 and a confidence of .6
                   parameter=list(support=0.06,confidence=0.6),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is detractor 
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=detractor")))

inspectDT(ruleset)

# Run again for "passive"
ruleset <- apriori(dfPaulSmithX, 
                   #Give parameters for the list: 
                   #use a low support of .005 and a confidence of .6
                   parameter=list(support=0.005,confidence=0.6),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is passive 
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=passive")))

inspectDT(ruleset)


###########  Southeast Airlines Co. ###################

# Select columns to be input into new df and filter by airline name 
dfSoutheast <- df %>% 
  select(Type.of.Travel,Flight.cancelled,Airline.Status,Gender, Class, Likelihood.to.recommend) %>%
  filter(df$Partner.Name == "Southeast Airlines Co.") 

# Convert the dataframe to a matrix 
dfSoutheastX <- as(dfSoutheast,"transactions")

# Run apriori() to find variables related to detractors
ruleset <- apriori(dfSoutheastX, 
                   #Give parameters for the list: 
                   #use a low support of .1 and a confidence of .6
                   parameter=list(support=0.1,confidence=0.6),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is bad (can test for passive or awesome too) 
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=detractor")))

inspectDT(ruleset)

# Run again for "passive"
ruleset <- apriori(dfSoutheastX, 
                   #Give parameters for the list: 
                   #use a low support of .005 and a confidence of .4
                   parameter=list(support=0.005,confidence=0.4),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is passive 
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=passive")))

inspectDT(ruleset)


###########  Cool&Young Airlines Inc. ###################

# Select columns to be input into new df and filter by airline name 
dfCoolYoung <- df %>% 
  select(Type.of.Travel,Flight.cancelled,Airline.Status,Gender, Class, Likelihood.to.recommend) %>%
  filter(df$Partner.Name == "Cool&Young Airlines Inc.") 

# Convert the dataframe to a matrix 
dfCoolYoungX <- as(dfCoolYoung,"transactions")

# Run apriori() to find variables related to detractors
ruleset <- apriori(dfCoolYoungX, 
                   #Give parameters for the list: 
                   #use a low support of .005 and a confidence of .5
                   parameter=list(support=0.1,confidence=0.5),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is detractor 
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=detractor")))

inspectDT(ruleset)

# Run again for "passive" 
ruleset <- apriori(dfCoolYoungX, 
                   #Give parameters for the list: 
                   #use a low support of .005 and a confidence of .5
                   parameter=list(support=0.02,confidence=0.5),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is passive 
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=passive")))

inspectDT(ruleset)

###########  FlyToSun Airlines Inc. ###################

# Select columns to be input into new df and filter by airline name 
dfFlyToSun <- df %>% 
  select(Type.of.Travel,Flight.cancelled,Airline.Status,Gender, Class, Likelihood.to.recommend) %>% 
  filter(df$Partner.Name == "FlyToSun Airlines Inc.") 

# Convert the dataframe to a matrix 
dfFlyToSunX <- as(dfFlyToSun,"transactions")

# Run apriori() to find variables related to detractors
ruleset <- apriori(dfFlyToSunX, 
                   #Give parameters for the list: 
                   #use a low support of .005 and a confidence of .5
                   parameter=list(support=0.1,confidence=0.65),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is detractor 
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=detractor")))

inspectDT(ruleset)

# Run again for "passive"
ruleset <- apriori(dfFlyToSunX, 
                   #Give parameters for the list: 
                   #use a low support of .005 and a confidence of .5
                   parameter=list(support=0.08,confidence=0.3),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is passive 
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=passive")))

inspectDT(ruleset)

########### Northwest Business Airlines Inc. ###################

# Select columns to be input into new df and filter by airline name 
dfNorthwestBusiness <- df %>% 
  select(Type.of.Travel,Flight.cancelled,Airline.Status,Gender, Class, Likelihood.to.recommend) %>%
  filter(df$Partner.Name == "Northwest Business Airlines Inc.") 

# Convert the dataframe to a matrix 
dfNorthwestBusinessX <- as(dfNorthwestBusiness,"transactions")

# Run apriori() to find variables related to detractors
ruleset <- apriori(dfNorthwestBusinessX, 
                   #Give parameters for the list: 
                   #use a low support of .005 and a confidence of .5
                   parameter=list(support=0.08,confidence=0.5),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is detractor 
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=detractor")))

inspectDT(ruleset)

# Run again for "passive"
ruleset <- apriori(dfNorthwestBusinessX, 
                   #Give parameters for the list: 
                   #use a low support of .005 and a confidence of .5
                   parameter=list(support=0.03,confidence=0.35),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is passive 
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=passive")))

inspectDT(ruleset)

###########  Oursin Airlines Inc. ###################

# Select columns to be input into new df and filter by airline name 
dfOursin <- df %>% 
  select(Type.of.Travel,Flight.cancelled,Airline.Status,Gender, Class, Likelihood.to.recommend) %>%
  filter(df$Partner.Name == "Oursin Airlines Inc.") 

# Convert the dataframe to a matrix 
dfOursinX <- as(dfOursin,"transactions")

# Run apriori() to find variables related to detractors
ruleset <- apriori(dfOursinX, 
                   #Give parameters for the list: 
                   #use a low support of .005 and a confidence of .5
                   parameter=list(support=0.005,confidence=0.5),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is detractor 
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=detractor")))

inspectDT(ruleset)


# Run again for "passive"
ruleset <- apriori(dfOursinX, 
                   #Give parameters for the list: 
                   #use a low support of .008 and a confidence of .35
                   parameter=list(support=0.008,confidence=0.35),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is passive
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=passive")))

inspectDT(ruleset)

########### Sigma Airlines Inc. ###################

# Select columns to be input into new df and filter by airline name 
dfSigma <- df %>% 
  select(Type.of.Travel,Flight.cancelled,Airline.Status,Gender, Class, Likelihood.to.recommend) %>%
  filter(df$Partner.Name == "Sigma Airlines Inc.") 

# Convert the dataframe to a matrix 
dfSigmaX <- as(dfSigma,"transactions")

# Run apriori() to find variables related to detractors
ruleset <- apriori(dfSigmaX, 
                   #Give parameters for the list: 
                   #use a low support of .005 and a confidence of .5
                   parameter=list(support=0.005,confidence=0.5),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is detractor 
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=detractor")))

inspectDT(ruleset)

# Run again for "passive" 
ruleset <- apriori(dfSigmaX, 
                   #Give parameters for the list: 
                   #use a low support of .005 and a confidence of .5
                   parameter=list(support=0.005,confidence=0.5),
                   #Set appearance of list to show: a default lhs and 
                   #the rhs if liklihood.to.recommend is passive  
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=passive")))

inspectDT(ruleset)

# The rule trends come out very similar across all six airlines, so combining them into one matrix
# to test individual variables makes sense. 


################ More Granular Analysis on Most Common Variables ##################
# Create dataframe containing only the important airlines and variables 
dfAirlines <- df %>% 
  select(Type.of.Travel,Airline.Status,Gender, Class, Likelihood.to.recommend,Age) %>% 
  filter(df$Partner.Name == c("Cheapseats Airlines Inc.", "FlyFast Airways Inc.","Northwest Business Airlines Inc.", "Oursin Airlines Inc.","Sigma Airlines Inc.","Southeast Airlines Co.")) 
dfAirlines$Partner.Name <- as.factor(dfAirlines$Partner.Name)
dfAirlinesX <- as(dfAirlines,"transactions")



# Check plot to see item frequencies just to get a better idea of what support to use 
itemFrequencyPlot(dfAirlinesX, cex.names=.66)

# Create ruleset based on support from frequencies for detractors 
rulesetAirDet <- apriori(dfAirlinesX, 
                           #Give parameters for the list: 
                           parameter=list(support=0.05,confidence=.3),
                           #Set appearance of list to show: a default lhs and 
                           # Test the rhs if liklihood.to.recommend is bad or awesome  
                           appearance = list(default="lhs", rhs=("Likelihood.to.recommend=detractor")))

# Plot the rules to get a better idea of the support, confidence, and lift levels 
plot(rulesetAirDet) 

# Create new rules with higher lift considered and vie new ruleset
goodrulesAirDet <- rulesetAirDet[quality(rulesetAirDet)$lift > 1.2]
plot(goodrulesAirDet)
inspectDT(goodrulesAirDet)


# Create ruleset based on support from frequencies for passive customers 
rulesetAirPass <- apriori(dfAirlinesX, 
                           #Give parameters for the list: 
                           parameter=list(support=0.05,confidence=.3),
                           #Set appearance of list to show: a default lhs and 
                           # Test the rhs if liklihood.to.recommend is passive or detractor
                           appearance = list(default="lhs", rhs=("Likelihood.to.recommend=passive")))

# Plot the rules to get a better idea of the support, confidence, and lift levels 
plot(rulesetAirPass) 

# Create new rules with higher lift considered and vie new ruleset
goodrulesAirPass <- rulesetAirPass[quality(rulesetAirPass)$lift > 1.05]
plot(goodrulesAirPass)
inspectDT(goodrulesAirPass)

# Over six important airlines, detractors were most likely to show 
# Type.of.Travel=Personal Travel and Airline.Status=Blue as having the 
# strongest confidence and lift. Passive customers were most likely to have 
# Airline.Status=Silver and Age=Adult.


####################### Test on only Status ############################

dfAirStat <- df %>% 
  select(Airline.Status, Likelihood.to.recommend) %>% 
  filter(df$Partner.Name == c("Cheapseats Airlines Inc.", "FlyFast Airways Inc.","Northwest Business Airlines Inc.", "Oursin Airlines Inc.","Sigma Airlines Inc.","Southeast Airlines Co.")) 
dfAirStat$Partner.Name <- as.factor(dfAirStat$Partner.Name)
dfAirStatX <- as(dfAirStat,"transactions")

# Check plot to see item frequencies, not that blue status is most common
itemFrequencyPlot(dfAirStatX, cex.names=.8)

# Create ruleset based on support from frequencies for detractors 
rulesetAirStat <- apriori(dfAirStatX, 
                         #Give parameters for the list: 
                         parameter=list(support=0.05,confidence=.3),
                         #Set appearance of list to show: a default lhs and 
                         # Test the rhs if liklihood.to.recommend is passive or detracor
                         appearance = list(default="lhs", rhs=(c("Likelihood.to.recommend=detractor",
                                                                 "Likelihood.to.recommend=passive"))))
inspectDT(rulesetAirStat)
plot(rulesetAirStat)

# Detractors are most likely to have Blue Status.
# Passive customers are most likely to have Silver Status despite most customers having a Blue Status
# Be careful focusing only on these traits. While it provides a good, large group of customers to 
# focus on, come of the relationship olccurs as a result of the scarcity of customers with Gold or Platinum
# Status. However, it is a good starting point and it is worth looking into what services are preventing 
# Gold and Platium status partners from giving detractor scores. 

############################ Test on Only Age ######################

dfAirAge <- df %>% 
  select(Age,Likelihood.to.recommend) %>% 
  filter(df$Partner.Name == c("Cheapseats Airlines Inc.", "FlyFast Airways Inc.","Northwest Business Airlines Inc.", "Oursin Airlines Inc.","Sigma Airlines Inc.","Southeast Airlines Co.")) 
dfAirlines$Partner.Name <- as.factor(dfAirlines$Partner.Name)
dfAirAgeX <- as(dfAirAge,"transactions")


# Check plot to see item frequencies 
itemFrequencyPlot(dfAirAgeX, cex.names=.8)

# Create ruleset based on support from frequencies for detractors 
rulesetAirAge <- apriori(dfAirAgeX, 
                         #Give parameters for the list: 
                         parameter=list(support=0.005,confidence=.2),
                         #Set appearance of list to show: a default lhs and 
                         # Test the rhs if liklihood.to.recommend is bad or awesome  
                         appearance = list(default="lhs", rhs=(c("Likelihood.to.recommend=detractor",
                                                                 "Likelihood.to.recommend=passive"))))
inspectDT(rulesetAirAge)
plot(rulesetAirAge)

# Detractors are most likely to be Seniors.
# Passive custonmers are most likely to be Adults by a small margin, but many customers in the adult
# category also gave Passive scores. 

############################ Test on Only Type of Travel ######################

dfAirToT <- df %>% 
  select(Type.of.Travel,Likelihood.to.recommend) %>% 
  filter(df$Partner.Name == c("Cheapseats Airlines Inc.", "FlyFast Airways Inc.","Northwest Business Airlines Inc.", "Oursin Airlines Inc.","Sigma Airlines Inc.","Southeast Airlines Co.")) 
dfAirToTX <- as(dfAirToT,"transactions")


# Check plot to see item frequencies 
itemFrequencyPlot(dfAirToTX, cex.names=.8)

# Create ruleset based on support from frequencies for detractors 
rulesetAirToT <- apriori(dfAirToTX, 
                         #Give parameters for the list: 
                         parameter=list(support=0.005,confidence=.3),
                         #Set appearance of list to show: a default lhs and 
                         # Test the rhs if liklihood.to.recommend is bad or awesome  
                         appearance = list(default="lhs", rhs=(c("Likelihood.to.recommend=detractor",
                                                                 "Likelihood.to.recommend=passive",
                                                                 "Likelihood.to.recommend=promotor"))))
inspectDT(rulesetAirToT)
plot(rulesetAirToT)

# Detractors are most likely to be travelling for Personal travel.
