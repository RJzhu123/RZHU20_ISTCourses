################################################
# IST 687, M002 Group 2 Final Project
#
# Group 2 names: Ryan Reed, Abhiram Gopal, Scott Kessel, Sidney Lanier, Renjie Zhu
# Final Project
# Milestone 1 due: 11/6/2019
# Milestone 2 due:  11/20/2019
# Final Project due: 12/10/2019
#
#
#
# Attribution statement: (choose the statements that are true)
# 1. group 2 did this work with help from the book and the professor
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

# Set working directory 
# Change to the folder containing your homework data files
setwd("C:/Users/Reed/Desktop/Intro_to_Data_Science/FinalProject") # Change to the folder containing your Projects JSON data files

# Step 1: a-c
getwd() # making sure that the setwd() function set the working directory correctly

#install.packages("RCurl")
library("RCurl")
#install.packages("bitops")
library("bitops")
#install.packages("jsonlite")
library("jsonlite")
#install.packages("tidyverse")
library("tidyverse")

# 
# create a bar chart with three bars for each airline partners the airlines with most passive and detractors we analyze.

# using the jsonlite library we will use the fromJSON() function to read in the project data into the a dataframe called
# df. Note make sure that the file path below is pointing to where the JSON file you wish to read is located.
df <- jsonlite::fromJSON("C:/Users/Reed/Desktop/Intro_to_Data_Science/FinalProject/fall2019-survey-M02(1).json")
View(df) # taking a quick view of the df to see if there were any issues with reading the JSON file into a dataframe
dim(df) # finding the diminsions of the original JSON dataframe > [1] observations: 10282   variables: 32
str(df) # looking a the structure of the dataframe
summary(df) # looking at a summary of the data inside the dataframe


#Cleaning columns 1 - 6

# 1 - 2 #Converting to lower case
df$Destination.City <- tolower(df$Destination.City) # convert column to lower case
df$Origin.City <- tolower(df$Origin.City) # convert column to lower case

# 1- 2 #creating a new column with just the abbreviation state characters of the origin & destination
df$Destination.abv <- gsub("^.*?, ","",df$Destination.City) 
df$Origin.abv <- gsub("^.*?, ","", df$Origin.City)

# 1- 2 #Removing the abbreviation for the existing column(since we created a new one above)
df$Destination.City <- gsub(", ..","",df$Destination.City) # takes the state abrevation out of this column
df$Origin.City <- gsub(", ..","", df$Origin.City) # takes the state abrevation out of this column

# 5 # - Gender as factor - Male or Female
df$Gender <- as.factor(df$Gender) # convert column to type factor

# 6 # - Price sensitivity as factor
df$Price.Sensitivity <- as.factor(df$Price.Sensitivity) # convert column to type factor

#cleanign column 7 - 12

# 7 # - Convert Year.of.First.Flight to date/year & taking only the year format for analysis  
df$Year.of.First.Flight <-as.Date(as.character(df$Year.of.First.Flight), format = "%Y")
df$Year.of.First.Flight <- format(as.Date(df$Year.of.First.Flight, format="%d/%m/%Y"),"%Y")

# 10 # - # Convert Type.of.Travel to a factor
df$Type.of.Travel <- as.factor(df$Type.of.Travel)

# 19 , 20  # Origin state , destination state # COnverting all to lowercase
df$Origin.State<-tolower(df$Origin.State)
df$Destination.State<-tolower(df$Destination.State)

# 22 , 23 # -   departure delay and departure arrival - substituting NAs by the mean of the flight minutes of flights where the origin city and destination city are the same
#########################################################################
#Column Arrival.Delay.in.Minutes and Departure.Delay.in.Minutes

list_na_arrive_delay <- which(is.na(df$Arrival.Delay.in.Minutes)==TRUE)
list_na_departure_delay <-which(is.na(df$Departure.Delay.in.Minutes)==TRUE)

no_value_arrive <- df[list_na_arrive_delay,]
no_value_departure <- df[list_na_departure_delay,]
#create two data frame to store the NA value of two columns

for (i in seq(1,length(list_na_arrive_delay))){
  no_value_arrive$Arrival.Delay.in.Minutes[i]<- floor(mean(df$Arrival.Delay.in.Minutes[df$Arrival.Delay.in.Minutes[df$Destination.City ==df$Destination.City[i] 
                                                                                                                   & df$Origin.City==df$Origin.City[i]]],na.rm=TRUE))
  
}
for (i in seq(1,length(list_na_departure_delay))){
  no_value_departure$Departure.Delay.in.Minutes[i]<- floor(mean(df$Departure.Delay.in.Minutes[df$Departure.Delay.in.Minutes[df$Destination.City ==df$Destination.City[i] 
                                                                                                                            & df$Origin.City==df$Origin.City[i]]],na.rm=TRUE))
  
}

no_value_arrive[which(is.na(no_value_arrive[,23])),23] <- mean(df$Arrival.Delay.in.Minutes,na.rm=TRUE)
no_value_departure[which(is.na(no_value_departure[,22])),22] <- mean(df$Departure.Delay.in.Minutes,na.rm=TRUE)

#Replace the remaining NAs value by the mean of delay minutes of whole data set

df$Arrival.Delay.in.Minutes[which(is.na(df$Arrival.Delay.in.Minutes))]<-
  no_value_arrive[,23]
df$Departure.Delay.in.Minutes[which(is.na(df$Departure.Delay.in.Minutes))]<- no_value_departure[,22]

#Replace the NAs value in df using these two data frame which has replaced all NAs values
#########################################################################

# 24 # - Convertring into a factor
df$Flight.cancelled<-as.factor(df$Flight.cancelled)

# 25 # - Flight time in minutes - 235NAs
#########################################################################
#Replace the NAs value in the column "Flight.time.in.minutes" by the mean of the flight minutes of flights where the origin city and destination city are the same
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

# 27 # - COnvert to factor and replace 1 na with the mean of that flights likelyhood to recommend

df[635,27]<-7
df$Likelihood.to.recommend<-as.factor(df$Likelihood.to.recommend)



################################################################################
############## Determining Appropriate Dataframes to Analyze ###################
################################################################################

# for the purposes of this linear model analysis I am going to convert the dataframe df column's
# Likelihood.to.recommend, Price.Sensitivity, and Year.of.First.Flight from type factor to num
df$Likelihood.to.recommend <- as.numeric(df$Likelihood.to.recommend)
df$Price.Sensitivity <- as.numeric(df$Price.Sensitivity)
df$Year.of.First.Flight <- as.numeric(df$Year.of.First.Flight)

# Seperate the large dataframe into 3 dataframes, the Detractor dataframe 
# (lickely.hood.to.recomend variable = 0-6), 
# the Passives dataframe (lickely.hood.to.recomend variable = 7-8), 
# and the Promotors dataframe (lickely.hood.to.recomend variable = 9-10)
dfDetractor <- df[df$Likelihood.to.recommend<=6,]
View(dfDetractor) # viewed to make sure it looked correct
# ran dudiligance to make sure everything looked right
dim(dfDetractor)
str(dfDetractor)
summary(dfDetractor)

# the Passives dataframe (lickely.hood.to.recomend variable = 7-8),
dfPassive <- df[df$Likelihood.to.recommend<=8 & df$Likelihood.to.recommend>6,]
View(dfPassive)# viewed to make sure it looked correct
# ran dudiligance to make sure everything looked right
dim(dfPassive)
str(dfPassive)
summary(dfPassive)

# the Promotors dataframe (lickely.hood.to.recomend variable = 9-10)
dfPromotor <- df[df$Likelihood.to.recommend>=9,]
View(dfPromotor)# viewed to make sure it looked correct
# ran dudiligance to make sure everything looked right
dim(dfPromotor)
str(dfPromotor)
summary(dfPromotor)

# Please note that at this point we analyzed the 3 dataframes and ralized that
# in order to increase or NPS higher (i.e. NPS = Promotors - Detractors) 
# we need to worry about increasing the Detractors into Passive or Promotor
# and the Passive into promotor to increase the overall Promotor score in which
# will increase the overall NPS. Therefore, we determined that the 3755 records
# that are in the promotor category are not relizant to our analysis therefore
# the dfPromotor dataframe was neglected from any further analysis.




# At this point an analysis of the the 14 airlines sampling
# size based on Detractor and Passive Values occured in order
# to determine which of the 14 airlines had
# significant sampling size in order to influence that overall dataset 
# (i.e. all 10282 records) thus in turn influencing our NPS.

# Plotting a bar graph of the Partner Airlines vs. their 
# Likelihood.to.recommend Detractor values. 
PD <- dfDetractor %>%
  mutate(Likelihood.to.recommend=as.factor(Likelihood.to.recommend)) %>% #convert variable to factor
  group_by(Likelihood.to.recommend) %>% # grouped variable into like values
  ggplot(aes(x = Partner.Name, y = Likelihood.to.recommend)) + # defined x & y axis
  geom_col(aes(fill = Likelihood.to.recommend)) + # disignated bar chart and fill coditions
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # rotate airline names 90 degrees
  ggtitle("Distribution of Detractor Values over each Partner Airline") # Added title
PD

# Please note that the this graph represents a great example of the distribution 
# of the Likelihood.to.recommend Detractor variables this is important because
# A. the amount of sample size in each airline is important towards influencing
# the overall population and thus the NPS result, and B. the higher the sum total 
# indicates the better chance changes to one (some) airlines vs. others will
# have a greater chance to raise a detractors value higher
# which means that there is a greater chance of moving people from these airlines
# (and potentially others too!)
# closer to the Passive or even Promotor values (i.e. increasing out NPS.)

# Plotting a bar graph of the Partner Airlines vs. their 
# Likelihood.to.recommend Passive values. 
PP <- dfPassive %>%
  mutate(Likelihood.to.recommend=as.factor(Likelihood.to.recommend)) %>%
  group_by(Likelihood.to.recommend) %>%
  ggplot(aes(x = Partner.Name, y = Likelihood.to.recommend)) +
  geom_col(aes(fill = Likelihood.to.recommend)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Distribution of Passive Values over each Partner Airline")
PP

# Please note that the this graph represents a great example of the distribution 
# of the Likelihood.to.recommend Passive variables this is important because
# the amount of sample size in each airline is important towards influencing
# the overall population and thus the NPS result, and B. the higher the sum total 
# indicates the better chance changes to one (or some) airlines vs. others will
# have a greater chance to raise a PAssive value higher
# which means that there is a greater chance of moving people from these airlines
# (and potentially others too!)
# closer to the Promotor values (i.e. increasing out NPS.)

# At this point it was determined that the most lickely airline partners that have 
# a significant sampling size of both Detractor and Promotors were: 
# "Cheapseats Airlines Inc.", "FlyFast Airways Inc.", "Northwest Business Airlines Inc.", 
# "Oursin Airlines Inc.", "Sigma Airlines Inc.", and "Southeast Airlines Co.". However,
# our group felt that it was important to create a sampling percentage analysis on the
# data in question to note what percentage of data would be neglected if the 8 non-selected
# airlines were left out of further modeling analysis.

# Analyzing the Detractor and Promotor Dataframes to determine the approprate airlines to use

# Now that we have determined our airlines it is time to merdge our dataframes and sub-set
# our data into the above determined airlines to start looking at further analysis.
NewDF <- rbind(dfPassive, dfDetractor)

# Further seperating the mergedDF dataframes into airlines
# (i.e. into 14 smaller triblles) based on the 14 airline partners variable.
groupedDF <- NewDF %>%
  group_split(Partner.Name)
View(groupedDF)

# Split the airlines into 14 dataframes
# Key for the 14 datefrmes:
# dfCAI = "Cheapseats Airlines Inc."
# dfC&YAI = "Cool&Young Airlines Inc."
# dfEFAS = "EnjoyFlying Air Services"
# dfFFAI = "FlyFast Airways Inc."
# dfFHA = "FlyHere Airways"
# dfFTSAI = "FlyToSun Airlines Inc."
# dfGNAI = "GoingNorth Airlines Inc."
# dfNBAI = "Northwest Business Airlines Inc."
# dfOJAI = "OnlyJets Airlines Inc."
# dfOAI = "Oursin Airlines Inc."
# dfPSAI = "Paul Smith Airlines Inc."
# dfSAI = "Sigma Airlines Inc."
# dfSAC = "Southeast Airlines Co."
# dfWAI = "West Airways Inc."

# Taking the 14 tibles and creating 14 dataframes out of them
dfCAI <- data.frame(groupedDF[1])
dfCYAI <- data.frame(groupedDF[2])
dfEFAS <- data.frame(groupedDF[3])
dfFFAI <- data.frame(groupedDF[4])
dfFHA <- data.frame(groupedDF[5])
dfFTSAI <- data.frame(groupedDF[6])
dfGNAI <- data.frame(groupedDF[7])
dfNBAI <- data.frame(groupedDF[8])
dfOJAI <- data.frame(groupedDF[9])
dfOAI <- data.frame(groupedDF[10])
dfPSAI <- data.frame(groupedDF[11])
dfSAI <- data.frame(groupedDF[12])
dfSAC <- data.frame(groupedDF[13])
dfWAI <- data.frame(groupedDF[14])

# Finding the dimensions of all 14 airline dataframes using the dim() function
A1 <- dim(dfCAI) 
A2 <- dim(dfCYAI) 
A3 <- dim(dfEFAS)
A4 <- dim(dfFFAI)
A5 <- dim(dfFHA)
A6 <- dim(dfFTSAI) 
A7 <- dim(dfGNAI)
A8 <- dim(dfNBAI)
A9 <- dim(dfOJAI)
A10 <- dim(dfOAI)
A11 <- dim(dfPSAI) 
A12 <- dim(dfSAI)
A13 <- dim(dfSAC)
A14 <- dim(dfWAI)
AAll <- dim(NewDF)
# 
#Combined Detractor/Passive Airline Partner Ratio
#Determined by taking the number of the sum of the Detractor and Passive values
#for each airline partner and deviding by the total combined records for 
# Detractor & Passive values.
CAI_Ratio <- A1[1]/AAll[1]
CAI_Ratio
CYAI_Ratio <- A2[1]/AAll[1]
CYAI_Ratio
EFAS_Ratio <- A3[1]/AAll[1]
EFAS_Ratio
FFAI_Ratio <- A4[1]/AAll[1]
FFAI_Ratio 
FHA_Ratio <- A5[1]/AAll[1]
FHA_Ratio
FTSAI_Ratio <- A6[1]/AAll[1]
FTSAI_Ratio 
GNAI_Ratio <- A7[1]/AAll[1]
GNAI_Ratio
NBAI_Ratio <- A8[1]/AAll[1]
NBAI_Ratio
OJAI_Ratio <- A9[1]/AAll[1]
OJAI_Ratio
OAI_Ratio <- A10[1]/AAll[1]
OAI_Ratio
PSAI_Ratio <- A11[1]/AAll[1]
PSAI_Ratio
SAI_Ratio <- A12[1]/AAll[1]
SAI_Ratio
SAC_Ratio <- A13[1]/AAll[1]
SAC_Ratio
WAI_Ratio <- A14[1]/AAll[1]
WAI_Ratio

# After viewing the Detractor and Passive graphs and running a precentage sampling analysis on the 
# on the combined Passive and Detractor dataframe it was determined that the airline parners
# "Cheapseats Airlines Inc.", "FlyFast Airways Inc.", "Northwest Business Airlines Inc.", 
# "Oursin Airlines Inc.", "Sigma Airlines Inc.", "Southeast Airlines Co." had sufficiant 
# amounts of samples and potentially high values of Passive and Detractor scale values present.

# Taking the 14 tibles and creating 6 dataframes out of what we determined to 
# be our relevant dataframes. Note, this is just recalling dataframes that 
# already exist but it always good to reset a dataframe before further analysis just incase
# something code above happen to change the originalsub-dataframes (this most likely didn't
# occur but it's just good habbits).
dfCAI <- data.frame(groupedDF[1])
dfFFAI <- data.frame(groupedDF[4])
dfNBAI <- data.frame(groupedDF[8])
dfOAI <- data.frame(groupedDF[10])
dfSAI <- data.frame(groupedDF[12])
dfSAC <- data.frame(groupedDF[13])

summary(dfCAI)
summary(dfFFAI)
summary(dfNBAI)
summary(dfOAI)
summary(dfSAI)
summary(dfSAC)


################################################################################
############################# LINEAR MODEL ANALYSIS ############################
################################################################################

# Constructing a linModel function that will return a linear model analysis based 
# on the numerical type variables that are located in the dataframe that is inputed 
# as the functions argument.
# It is important to note that this function was created as a time saver so that 6 linear models
# could be constructed quickly based on the 6 Partnering airlines that are apart of the final projects
# overall data base. Analysis on how this was determined above.
# As noted the input to the function is a sub-dataframe, a linear model is determined, and a summary
# function on that linear model is run. The summary is then returned as the functions output.

# linear Model function all 6 data frames
LinModel <- function(Inputdf){
  
  m <- lm(formula = Likelihood.to.recommend ~ Airline.Status + Age + Gender + Price.Sensitivity + Year.of.First.Flight + Flights.Per.Year + Loyalty + Type.of.Travel + Total.Freq.Flyer.Accts + Shopping.Amount.at.Airport + Eating.and.Drinking.at.Airport + Class + Day.of.Month + Scheduled.Departure.Hour + Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes + Flight.cancelled + Flight.time.in.minutes + Flight.Distance, data = Inputdf)
  value <- summary(m)  
  
  
  return(value)
}

# Creating 6 linear models on each airlines partner to determine which independent variables are
# significant to the dependent variable which is lickely.hood.to.recomend. The 6 linear models
# were determined by the LinModel() function created above.

LinModel(dfCAI)
LinModel(dfFFAI)
LinModel(dfNBAI)
LinModel(dfOAI)
LinModel(dfSAI)
LinModel(dfSAC)

# Based on the analysis for the 6 linear models above it was determined that the significant variable
# identified as Airline.StatusPlatinum, Airline.StatusSilver, Age, Type.of.TravelPersonal Travel
# we classifed by all or the majority of the airlines as significant. Additionally the Adjuted R-Squard
# values were 0.277 for "Cheapseats Airlines Inc.", 0.3142 for "FlyFast Airways Inc.",
# 0.269 for "Northwest Business Airlines Inc.", 0.2502 for "Oursin Airlines Inc.", 
# 0.2581 for "Sigma Airlines Inc.", and 0.2949 for "Southeast Airlines Co." which
# noted from "An Introdication to Data Science" by J. Saltz and J. Stanton (page 203) when 
# analyzing hunman behavior (which we are in this project) a very good R-Squared values
# can be between (0.2 and 0.3) due to the fact that humans are notoriously unpredictable.


# Now that we have looked at the 6 linear models and have determined that the significant 
# variables are Airline.StatusPlatinum, Airline.StatusSilver, Age, Type.of.TravelPersonal Travel
# the only remaining thing to determine through linear modeling is which age is most benefitual
# therefore using ggplot we can graph the dependent variable (Likelihood.to.recommend) vs. Age
# and plot the linear model line to the graph
PD_Airlines <- rbind(dfCAI, dfFFAI, dfNBAI, dfOAI, dfSAI, dfSAC)
ggplot(PD_Airlines, aes(x = Age, y = Likelihood.to.recommend)) +
  geom_point() + stat_smooth(method = "lm", col = "red") + 
  ggtitle("Likely.hood.to.recommend vs. Age with lm trendline ")
# By doing this we see that the younger one is the more likely
# a higher passive or detractor score was present therefore we recomend focusing marketing campains
# to apeal to Younger (below 50), Platnum or Silver airline status members, who are looking to
# travel on personal travel in order to increase Detractor and Passive Likelihood.to.recommend scores
# in order to increase the overal NPS.


# Sanity check of the 6 linear model analysis that models by combining the 6 airline dataframes into
# one larger dataframe and running a linear model on its entirety. The results of the analysis are 
# identified in the final report under the linear model analysis section.
LinModel(PD_Airlines)



