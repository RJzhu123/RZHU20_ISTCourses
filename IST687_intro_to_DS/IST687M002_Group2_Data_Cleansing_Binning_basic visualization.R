
install.packages("RCurl")
library("RCurl")
install.packages("bitops")
library("bitops")
install.packages("jsonlite")
library("jsonlite")
install.packages("tidyverse")
library("tidyverse")


##DATA ACQUISITION AND CLEANING##

df <- jsonlite::fromJSON("survey.json")
dim(df)

##ALL cleanings of NAs##

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

#I searched the google fight, find the flight minutes is about 82minutes.
#so I replaced the na value by 82
no_value1[103,25] <- 82

df$Flight.time.in.minutes[which(is.na(df$Flight.time.in.minutes))] <- no_value1[,25]

# 27 # - COnvert to factor and replace 1 na with the mean of that flights likelyhood to recommend

df[635,27]<-7

##OUTLIEERRRSSS##



##outlier detections - Age - No worries - ##
str(df$Age)
boxplot(df$Age)$out
filter(df,df$Age>85|df$Age<15)

##year of first flight## - no worries 
df$Year.of.First.Flight<-as.numeric(df$Year.of.First.Flight)
hist(df$Year.of.First.Flight)
boxplot(df$Year.of.First.Flight)
filter(df,df$Year.of.First.Flight<2003|df$Year.of.First.Flight>2012)

##Flights per year - as per metadata between 0 and 100
boxplot(df$Flights.Per.Year)
quantile(df$Flights.Per.Year , c(.75 , .95 , .98 , .99))
nrow(filter(df,df$Flights.Per.Year>60))

##age vs flights per year 
ggplot(data = df , aes(x = df$Age , y = df$Flights.Per.Year , group = df$Age)) + geom_boxplot()
nrow(filter(df,df$Flights.Per.Year>60))
df%>%
  group_by(df$Age)%>%
  filter(Flights.Per.Year>60&Age<20)

#loyalty index - lies between 1 to -1 - no outlier values
filter(df , abs(df$Loyalty)>1)

#Type of travel - well and good
summary(df$Type.of.Travel)

##Binning and Categorizing##

#Likelyhood to recommend - converting them into detractors , passives and promoters
df$Likelyhood<-ifelse(df$Likelihood.to.recommend<7,"Detractor",ifelse(df$Likelihood.to.recommend>8,"Promoter","Passive"))
df$Likelyhood<-as.factor(df$Likelyhood)
levels(df$Likelyhood)<-c("Detractors","Passives","Promoters")

##IMPORTANT : As per our analysis we are taking only those records having a passive or detractor score
dfPasstractors<-df[df$Likelyhood=='Passives'|df$Likelyhood=='Detractors',]


#creating a binning variable for AGE
dfPasstractors$Age_bin<-ifelse(dfPasstractors$Age<18,"Under 18",
                               ifelse(dfPasstractors$Age>=18 & dfPasstractors$Age<=30,"Between 18 and 30",
                                      ifelse(dfPasstractors$Age>30 & dfPasstractors$Age<=55,"Above 30 to 55","Above 55")))

#Creating a Binning variable for Flights_bin
dfPasstractors$Number_of_flights_bin<-ifelse(dfPasstractors$Flights.Per.Year<20,"Under 20 flights",
                                             ifelse(dfPasstractors$Flights.Per.Year>=20 & dfPasstractors$Flights.Per.Year<=40,"Between 20 to 40 Flights",
                                                    ifelse(dfPasstractors$Flights.Per.Year>40 & dfPasstractors$Flights.Per.Year<60 , "Above 40 Flights and below 60","60 and above flights")))
dfPasstractors$Number_of_flights_bin<-as.factor(dfPasstractors$Number_of_flights_bin)

#Creating a Binning variable for Loyalty
dfPasstractors$loyalty_bin<-ifelse(dfPasstractors$Loyalty<=0,"-1 to 0 Loyalty Index","Above 0 to 1 Loyalty Index")
dfPasstractors$loyalty_bin<-as.factor(dfPasstractors$loyalty_bin)

#creating a binning variable for number of frequent flyer accounts
dfPasstractors$Frequent_flyer_bin<-ifelse(dfPasstractors$Total.Freq.Flyer.Accts < 1 , "Less than 1" , "More than 1")
dfPasstractors$Frequent_flyer_bin<-as.factor(dfPasstractors$Frequent_flyer_bin)

#Creating a binning variable for shopping amount
dfPasstractors$Shopping_amount_bin<-ifelse(dfPasstractors$Shopping.Amount.at.Airport<25,"Less than 25","Greater than 25")
dfPasstractors$Shopping_amount_bin<-as.factor(dfPasstractors$Shopping_amount_bin)


#Creating a binning variable for eating and drinking amount
dfPasstractors$Eating_and_drinking_bin<-ifelse(dfPasstractors$Eating.and.Drinking.at.Airport<50,"Less than 50",
                                               ifelse(dfPasstractors$Eating.and.Drinking.at.Airport>=50 & dfPasstractors$Eating.and.Drinking.at.Airport<=100,"Between 50 and 100","Greater than 100"))
dfPasstractors$Eating_and_drinking_bin<-as.factor(dfPasstractors$Eating_and_drinking_bin)


#Creating a binning variable for arrival delay more than 15min(Taken as per the mean value)
dfPasstractors$Arrival_delay_more_than_15min<-ifelse(dfPasstractors$Arrival.Delay.in.Minutes<15,"Less than 15min","More than 15min")

dfPasstractors$Arrival_delay_more_than_15min<-as.factor(dfPasstractors$Arrival_delay_more_than_15min)



#Creating a new column with Duration of the whole trip from departure to arrival
dfPasstractors$Trip_duration<-dfPasstractors$Arrival.Delay.in.Minutes+dfPasstractors$Flight.time.in.minutes
summary(dfPasstractors$Trip_duration)
#mean durration around 2hrrs
dfPasstractors$Trip_duration_bin<-ifelse(dfPasstractors$Trip_duration<120,"Less than 2hrs journey","More than 2hrs journey")
dfPasstractors$Trip_duration_bin<-as.factor(dfPasstractors$Trip_duration_bin)

#Creating a new data set with the cleaned,Binned values. & Also copying all other relevant facotr variables which shall be used in the analysis

dfBinned<-dfPasstractors[,34:44]
dfBinned<-dfBinned[,-1]
dfBinned$Partner_name<-dfPasstractors$Partner.Name
dfBinned$Likelyhood_to_recommend<-dfPasstractors$Likelihood.to.recommend
dfBinned$airline_status<-dfPasstractors$Airline.Status
dfBinned$gender<-dfPasstractors$Gender
dfBinned$class<-dfPasstractors$Class
dfBinned$Type_of_travel<-dfPasstractors$Type.of.Travel
dfBinned<-dfBinned[,-12]

#The final dataset of dfBinned is our cleaned and completed dataset. Now based upon this.. Further analysis is done below.


#Visualizations
#To find the number of detractors , promoters and passives.
ggplot(data = df , aes(x = df$Likelyhood , fill = Likelyhood))+geom_bar(color = "black")+xlab("Likelyhood to recommend factors")+ylab("Count")
table(df$Likelyhood)

#To find the mean score as per every airline - scatter plot
df1<-data.frame(df%>%
                  group_by(Partner.Name)%>%
                  summarise(Mean_Likelyhood = mean(Likelihood.to.recommend) , Number_of_records = n()))
ggplot(data = df1 , aes(x = df1$Partner.Name, y = df1$Mean_Likelyhood , color = df1$Partner.Name , size = df1$Number_of_records))+geom_point()+xlab("Partner Name")+ylab("Mean Score")+theme(axis.text.x = element_text(angle = 90, hjust = 1))



