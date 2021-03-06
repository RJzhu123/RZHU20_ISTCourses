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

#Set working directory 
# Change to the folder containing your homework data files
#setwd("C:/Users/Reed/Desktop/Intro_to_Data_Science/FinalProject") # Change to the folder containing your Projects JSON data files

# Step 1: a-c
#getwd() # making sure that the setwd() function set the working directory correctly

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
df <- fromJSON("/Users/renjiezhu/Desktop/fall2019-survey-M02.json")

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
df$Likelihood.to.recommend <- as.numeric(df$Likelihood.to.recommend)
df$Price.Sensitivity <- as.numeric(df$Price.Sensitivity)
df$Year.of.First.Flight <- as.numeric(df$Year.of.First.Flight)

################################################################################
############################# DECISION TREE MODEL ANALYSIS ############################
################################################################################

prac_df <- data.frame(df) 
view(prac_df)

#import the rapart library
library(rpart)

#I will devide the likelihood.to.recommend column into 3 parts:1-6,7-8,9-10

#convert the likelihood column into 3 parts, bad, passive, awesome
for(i in seq(1,length(prac_df[,1]))){
  if(prac_df$Likelihood.to.recommend[i] <=6){
    prac_df$Likelihood.to.recommend[i] <- 'bad'
  }else if(prac_df$Likelihood.to.recommend[i] >=9){
    prac_df$Likelihood.to.recommend[i] <- 'awesome'
  }else{
    prac_df$Likelihood.to.recommend[i] <- 'passive'
  }
}

# Convert the data type to the factor
prac_df$Likelihood.to.recommend <- as.factor(prac_df$Likelihood.to.recommend)

#install.packages('caret')
library(caret)
#use caret package to create training and test sets

#makes the sampling predictable
set.seed(111) 
str(prac_df)
#Use the columns in the df to build the tree model 
#except Destination.City, Origin.City, Flight.Date, Partner.Name, olong, olat, dlong, dlat, freetext
prac_df <- prac_df[,-28:-34]
prac_df <- prac_df[,-1:-2]
prac_df <- prac_df[,-14]
prac_df <- prac_df[,-15]
prac_df <- prac_df[,-15:-16]
#View(prac_df)

# Randomly sample elements to go into a training data set
trainList <- createDataPartition(y=prac_df$Likelihood.to.recommend,p=.70,list=FALSE)

# Include all of those elements in the training set
trainSet <- prac_df[trainList,]

# Construct test set from everything that didn’t go into the training
testSet <- prac_df[-trainList,]

# Use the trainSet
like_tree <- rpart(Likelihood.to.recommend~.,data = trainSet)
control = rpart.control(minsplit = 8)
like_tree

#install.packages('rpart.plot')
library(rpart.plot)

# Use prp() function to explore the decision tree
prp(like_tree, faclen = 0, cex = 0.8, extra = 1)

# Use the testSet to evaluate the like_tree
predictValues <- predict(like_tree,newdata=testSet,type = "class")
#simpler to do confusion matrix
#install.packages('e1071')
library(e1071)
confusion <- confusionMatrix(predictValues,testSet$Likelihood.to.recommend)
confusion

# Explore the variable importance
vi <-varImp(like_tree)
vi$variables <- rownames(vi)
vi<- vi[vi[,1]>0,]
plt_variance <- ggplot(data =vi, aes(reorder(x=vi[,2],-vi[,1]),y=vi[,1])) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plt_variance

##################################################################
#optimize the decision tree
#prune the minimum error tree
opt <- which.min(like_tree$cptable[,"xerror"])
cp <- like_tree$cptable[opt, "CP"]
like_tree <- prune(like_tree, cp = cp)

# Use the testSet to evaluate the like_tree
predictValues <- predict(like_tree,newdata=testSet,type = "class")
#simpler to do confusion matrix
#install.packages('e1071')
library(e1071)
confusion <- confusionMatrix(predictValues,testSet$Likelihood.to.recommend)
confusion

##################################################################
#randomForest
#install.packages('randomForest')
library(randomForest)
library(dplyr)
#convert all the character columns to factor
trainSet=trainSet %>% mutate_if(is.character, as.factor)
testSet=testSet %>% mutate_if(is.character, as.factor)

#build a randomforest model
like_rm_tree <- randomForest(Likelihood.to.recommend~.,data = trainSet,ntree=300)

#plot
plot(like_rm_tree)
like_rm_tree

#see the importance of the variables
importance(like_rm_tree)

#plot the importance 
varImpPlot(like_rm_tree)

# treesize：compute the number of nodes in each tree
head(treesize(like_rm_tree,terminal = TRUE))

count_data <- as.data.frame(plyr::count(treesize(like_rm_tree,terminal = TRUE)))
head(count_data,5)

#predict the testSet
rf_pre <- predict(like_rm_tree,newdata=testSet,type = "class")

#simpler to do confusion matrix
confusion <- confusionMatrix(rf_pre,testSet$Likelihood.to.recommend)
confusion


###############################################################
ProcessDF <- function(Inputdf){
  for(i in seq(1,length(Inputdf[,1]))){
    if(Inputdf$Likelihood.to.recommend[i] <=6){
      Inputdf$Likelihood.to.recommend[i] <- 'bad'
    }else if(Inputdf$Likelihood.to.recommend[i] >=9){
      Inputdf$Likelihood.to.recommend[i] <- 'awesome'
    }else{
      Inputdf$Likelihood.to.recommend[i] <- 'passive'
    }
  }
  
  # Convert the data type to the factor
  Inputdf$Likelihood.to.recommend <- as.factor(Inputdf$Likelihood.to.recommend)
  Inputdf <- Inputdf[,-28:-34]
  Inputdf <- Inputdf[,-1:-2]
  Inputdf <- Inputdf[,-14]
  Inputdf <- Inputdf[,-15]
  Inputdf <- Inputdf[,-15:-16]
}

DecisionTreeModel <- function(Inputdf){
  trainList <- createDataPartition(y=Inputdf$Likelihood.to.recommend,p=.70,list=FALSE)
  trainSet <- Inputdf[trainList,]
  testSet <- Inputdf[-trainList,]
  like_tree <- rpart(Likelihood.to.recommend~.,data = trainSet)
  control = rpart.control(minsplit = 8)  
  prp(like_tree, faclen = 0, cex = 0.8, extra = 1)
  predictValues <- predict(like_tree,newdata=testSet,type = "class")
  confusion <- confusionMatrix(predictValues,testSet$Likelihood.to.recommend)
  print(confusion)
  # Explore the variable importance
  vi <-varImp(like_tree)
  vi$variables <- rownames(vi)
  vi<- vi[vi[,1]>0,]
  plt_variance <- ggplot(data =vi, aes(reorder(x=vi[,2],-vi[,1]),y=vi[,1])) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  plt_variance  
}

groupedDF <- df %>%
  group_split(Partner.Name)
#view(groupedDF)

dfCAI <- data.frame(groupedDF[1])
dfCAI <- ProcessDF(dfCAI)
  
dfFFAI <- data.frame(groupedDF[4])
dfFFAI <- ProcessDF(dfFFAI)

dfNBAI <- data.frame(groupedDF[8])
dfNBAI <- ProcessDF(dfNBAI)

dfOAI <- data.frame(groupedDF[10])
dfOAI <- ProcessDF(dfOAI)

dfSAI <- data.frame(groupedDF[12])
dfSAI <- ProcessDF(dfSAI)

dfSAC <- data.frame(groupedDF[13])
dfSAC <- ProcessDF(dfSAC)

DecisionTreeModel(dfCAI)
DecisionTreeModel(dfFFAI)
DecisionTreeModel(dfNBAI)
DecisionTreeModel(dfOAI)
DecisionTreeModel(dfSAI)
DecisionTreeModel(dfSAC)

######################################################################
# Next step 
# age
df_old <- df[df$Age >= 65,]
df_young <- df[df$Age < 65,]
df_old <- ProcessDF(df_old)
df_old <- df_old[,-14]
DecisionTreeModel(df_old)
df_young <- ProcessDF(df_young)
df_young <- df_young[,-14]
DecisionTreeModel(df_young)








###########################
#male
view(df)
df_male <- df[df$Gender == 'Male',]
df_female <- df[df$Gender == 'Female',]
df_male <- ProcessDF(df_male)
df_male <- df_male[,-14]
DecisionTreeModel(df_male)

############################
#female
df_female <- ProcessDF(df_female)
df_female <- df_female[,-14]
DecisionTreeModel(df_female)
###############################
# type of travel
df_bus <- df[df$Type.of.Travel == 'Business travel',]
df_per <- df[df$Type.of.Travel == 'Personal Travel',]
df_mil <- df[df$Type.of.Travel == 'Mileage tickets',]
df_bus <- ProcessDF(df_bus)
df_bus <- df_bus[,-14]
DecisionTreeModel(df_bus)
df_per <- ProcessDF(df_per)
df_per <- df_per[,-14]
DecisionTreeModel(df_per)
df_mil <- ProcessDF(df_mil)
df_mil <- df_mil[,-14]
DecisionTreeModel(df_mil)



