##############################################################################################
################## Part1 : Data Description###################################################
##############################################################################################

######  Monthly Data Analysis

rm(list = ls())
setwd('/Users/renjiezhu/desktop/mas766/proj')
# load data
data <- read.csv("raw_sales.csv",header=TRUE,sep=",")
# describe data
dim(data)
names(data)
summary(data)

# missing value
sapply(data, function(x) sum(is.na(x)))

# time series plot
# Libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
png('DatesoldVsPrice.png')
data$datesold <- as.Date(data$datesold)
max_price <- max(data$price)
max_price_date <-data$datesold[which.max(data$price)]
options(scipen=10000000)
data %>% 
  ggplot( aes(x=datesold, y=price)) +
  geom_line(color="#69b3a2") +
  labs(title = "Datesold vs price time series plot") + 
  ylim(0,8500000) +
  xlab("Datesold") +
  ylab("Price($)") +
  annotate(geom="text", x=as.Date("2013-08-01"), y=max_price, 
           label="Sale price reached 8m$\nat the end of 2015") +
  annotate(geom="point", x=max_price_date, y=max_price, size=10, shape=21, fill="transparent") +
  geom_hline(yintercept=609736, color="orange", size=.5) +
  theme_ipsum()
dev.off()

# scatter plot
png('NumBedroomVsPrice.png')
ggplot(data, aes(x = bedrooms, y = price)) +
  labs(title = "Number of bedrooms vs price plot") + 
  geom_point(aes(color = factor(propertyType))) +theme_ipsum()+
  ylab("Price($)")
dev.off()

png('NumBedroomVsLnPrice.png')
ggplot(data, aes(x = bedrooms, y = log(price))) +
  labs(title = "Number of bedrooms vs log(price) plot") + 
  geom_point(aes(color = factor(propertyType))) +theme_ipsum() +
  ylab("Price($)")
dev.off()
# Summary statistics
summary(data$price)

# histogram
png("PriceHistogram.png")
data %>%
  filter( price<3000000 ) %>%
  ggplot( aes(x=price)) +
  geom_boxplot( fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Sale price histogram") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )+
  xlab("Price($)")
dev.off()
# Boxplot of price
png("PriceBoxplot.png")
data %>%
  ggplot( aes(y=price,color = price)) +
  geom_boxplot() +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  coord_flip()+
  ggtitle("SalePrice boxplot") +
  ylab("Price($)")
dev.off()

# Boxplot of property types
#install.packages("viridis")
png("PropertyTypeBoxplot.png")
library(viridis)
data %>%
ggplot( aes(x=propertyType, y=price, fill=propertyType)) +
  geom_boxplot() +
  scale_fill_hue()+
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  ylab("Price($)")
dev.off()

# piechart of property types
png("PropertyTypePiechart.png")
type_count <- data.frame(group = c("house","unit"), value=summary(data$propertyType))
ggplot(type_count, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +# remove background, grid, numeric labels+
  geom_text(aes(label = paste0(round(value))), position = position_stack(vjust = 0.5),size=10) +
  theme(
    legend.title = element_text( size = 28),
    legend.text = element_text( size = 25)
  )+
  ggtitle("PropertyTypePiechart")
dev.off()

# box plot of number of bedrooms
png("NumofBedroomsBoxplot.png")
data %>%
  ggplot( aes(x=as.character(bedrooms), y=price, fill=as.character(bedrooms))) +
  geom_boxplot() +
  scale_fill_hue()+
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("boxplot of different number of bedrooms") +
  xlab("number of bedrooms")+
  ylab("Price($)")
dev.off()

# pie chart of number of bedrooms
png("NumofBedroomsPiechart.png")
bedroom_count <- data.frame(table(data$bedrooms))
ggplot(bedroom_count, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +# remove background, grid, numeric labels
  theme(
    legend.title = element_text(size = 28),
    legend.text = element_text( size = 20)
  )+ 
  scale_fill_discrete(name="num of bedrooms", labels=bedroom_count$Var1)+ 
  ggtitle("NumofBedroomsPiechart")
dev.off()

# correlations 
# Pearson correlation between 2 variables
corr_data <- data %>% select(2,3,5)
cor(data$bedrooms,data$price)
data[0]

library(corrplot)
M<-cor(corr_data)
head(round(M,2))
corrplot(M, method="number",title = "Correlation Plot",mar=c(0,0,1,0))

corrplot(M, tl.cex = 1.5, tl.col = "black", method = "color", 
         outline = T,  order="hclust", 
         addCoef.col = "black", number.digits = 2, number.cex = 1.5, 
         cl.pos = 'b', cl.cex = 1, addrect = 1, rect.lwd = 3, 
         col = colorRampPalette(c("midnightblue", "white","darkred"))(100),mar=c(0,0,1,7))
mtext("Correlation Plot", at=2, line=-4, cex=3)


##############################################################################################
################## Part2 : Time Series########################################################
##############################################################################################

# load data
data <- read.csv("raw_sales.csv",header=TRUE,sep=",")
# describe data
dim(data)
names(data)
summary(data)
strdate <-as.character(data$datesold)
data$date <- format(as.Date(strdate), "%Y-%m")
View(data)
library(dplyr)

# extract the house avg data
month_data=data%>%
  filter(propertyType =='house')%>%
  group_by(date)%>%
  summarize(avg_price=mean(price,na.rm=T))

# lm prepare
Price <- month_data$avg_price
nr<-nrow(month_data)
Pricelag<-c(NA,Price[-nr])
# trend
Trend<-c(1:nr)       # 1:121
# number of lag
numlag<-1 
# date range for regression
tp<-c((numlag+1):nr) # 2:121

### 1. Trend Model
model1 <- lm(Price~Trend)
summary(model1)
# res vs x
res1 <- resid(model1)
plot(res1~Trend,axes = FALSE,xlab="x",ylab = "Residual",main ="Model1_residual vs x")
axis(side=1,pos=0)
axis(side=2)


### 2. Cubed Trend Model
Cubed_Trend <- Trend^3
model2 <- lm(Price~Cubed_Trend)
summary(model2)
# res vs x
res2 <- resid(model2)
plot(res2~Cubed_Trend,axes = FALSE,xlab="x",ylab = "Residual",main ="Model2_residual vs x")
axis(side=1,pos=0)
axis(side=2)


### 3.Exponential Trend
LnPrice <- log(Price)
model3 <- lm(LnPrice~Trend)
summary(model3)
# res vs x
res3 <- resid(model3)
plot(res3~Trend,axes = FALSE,xlab="x",ylab = "Residual",main ="Model3_residual vs x")
axis(side=1,pos=0)
axis(side=2)

### 4.AR1 model
y<-Price[tp]           
t<-Trend[tp]             
ylag1<-Pricelag[tp]    

model4<- lm(y~ylag1)
summary(model4)
anova(model4)
# res vs x
res4 <- resid(model4)
plot(res4~ylag1,axes = FALSE,xlab="x",ylab = "Residual",main ="Model4_residual vs x")
axis(side=1,pos=0)
axis(side=2)

### 5.Trend+Autoregressive
model5<- lm(y~ylag1+t)
summary(model5)
# res vs ylag1
res5 <- resid(model5)
plot(res5~ylag1,axes = FALSE,xlab="ylag1",ylab = "Residual",main ="Model5_residual vs ylag1")
axis(side=1,pos=0)
axis(side=2)
# res vs t
plot(res5~t,axes = FALSE,xlab="t",ylab = "Residual",main ="Model5_residual vs t")
axis(side=1,pos=0)
axis(side=2)


### 6.Seasonal Autoregressive Model
#first SAR lag
numlag<-4
rmv<-(nr-numlag+1):nr
Price4<-c(rep(NA,numlag), Price[-rmv])
plot(Price ~ Price4)
# out of sample test
cut<-3
# time period
s_tp<-c((numlag+1):(nr-cut))
y<- Price[s_tp]
t<-Trend[s_tp]
ylag4<-Price4[s_tp]
# model
model6<- lm(y~ylag4)
summary(model6)
# res vs t
res6 <- resid(model6)
plot(res6~ylag4,axes = FALSE,xlab="ylag4",ylab = "Residual",main ="Model6_residual vs ylag4")
axis(side=1,pos=0)
axis(side=2)

### 7.Trend+SAR
model7<- lm(y~t+ylag4)
summary(model7)
# res vs ylag4
res7 <- resid(model7)
plot(res7~ylag4,axes = FALSE,xlab="ylag4",ylab = "Residual",main ="Model7_residual vs ylag4")
axis(side=1,pos=0)
axis(side=2)
# res vs t
res7 <- resid(model7)
plot(res7~t,axes = FALSE,xlab="t",ylab = "Residual",main ="Model7_residual vs t")
axis(side=1,pos=0)
axis(side=2)



### 8.Trend+SAR (YEAR,lag12)
numlag<-12
rmv<-(nr-numlag+1):nr
Price12<-c(rep(NA,numlag), Price[-rmv])
# out of sample test
cut<-11
# time period
y_tp<-c((numlag+1):(nr-cut))
y<- Price[y_tp]
t<-Trend[y_tp]
ylag12<-Price12[y_tp]
y_lag4 <-Price4[y_tp]
# model
model8<- lm(y~t+ylag12)
summary(model8)
# res vs t
res8 <- resid(model8)
plot(res8~t,axes = FALSE,xlab="t",ylab = "Residual",main ="Model8_residual vs t")
axis(side=1,pos=0)
axis(side=2)
# res vs ylag12
res8 <- resid(model8)
plot(res8~ylag12,axes = FALSE,xlab="ylag12",ylab = "Residual",main ="Model8_residual vs ylag12")
axis(side=1,pos=0)
axis(side=2)

#Model8 forecast
modelfit<-fitted(model8)
fit<-c(modelfit)
# plot actual vs fit
plot(y ~ t, type = "l",main= "SAR(year)+Trend Fit",xlab="Trend", ylab="HousePrice")
lines(fit ~ t, col='blue', lty=1)

# 9. ARIMA
library('tseries')
library(forecast)

tsData <- ts(month_data$avg_price,start=c(2007,2),frequency=12)
components.ts = decompose(tsData)
plot(components.ts)
# original time series plot
autoplot(tsData,main="House Price Time Series Plot",ylab="HousePrice")
adf.test(tsData)
# p-value > 0.05, not stational

# Get the optimal ARIMA model
fitARIMA <- auto.arima(tsData)
fitARIMA

# Calculate R-squared
r2<- cor(fitted(fitARIMA),tsData)^2
r2

# Fit line plot
autoplot(forecast(fitARIMA))
tsdisplay(residuals(fitARIMA), lag.max=15, main='Seasonal Model Residuals')

# Forecast
fcast <- forecast(fitARIMA, h=30)
plot(fcast)
# residual
checkresiduals(fitARIMA)



# Adjusted R Squared Value of different models
r2_1<- summary(model1)$adj.r.squared
r2_2<- summary(model2)$adj.r.squared
r2_3<- summary(model3)$adj.r.squared
r2_4<- summary(model4)$adj.r.squared
r2_5<- summary(model5)$adj.r.squared
r2_6<- summary(model6)$adj.r.squared
r2_7<- summary(model7)$adj.r.squared
r2_8<- summary(model8)$adj.r.squared
data <- data.frame(
  name=c("Trend","Cubed Trend","Exp-Trend","AR1","AR1+t","SAR(month)","SAR(month)+t","SAR(year)+t","ARIMA"),
  val=(c(r2_1,r2_2,r2_3,r2_4,r2_5,r2_6,r2_7,r2_8,r2))
)
library(forcats)

# Adjusted R Squared Values Visualization
data %>%
  arrange(val) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(name=factor(name, levels=name)) %>%   # This trick update the factor levels
  ggplot( aes(x=name, y=val,label = sprintf("%0.2f", round(val, digits = 2)))) +
  geom_segment( aes(xend=name,yend=0)) +
  geom_point( size=6, color="orange") +
  ggtitle("Different Time Series Adjusted R-squared") +
  coord_flip() +
  theme_bw() +
  ylab("R-squared value") +
  xlab("Model Name")+
  geom_text(size = 4,hjust=-0.4)+
  scale_y_continuous(expand = c(0, 0),limits = c(0,1))

######  Quarter Data Analysis
library(tidyverse)
library(ggplot2)
rm(list = ls())

df <- read.csv("ma_lga_12345.csv")

View(df)
#Quarter of the Year
#Moving Average of Median Price
#Property Type i.e. house or unit
#Number of bedrooms

# Data Preprocessing
house <- df[df$type=="house", ]
unit <-df[df$type=="unit", ]

house <- house[order(as.Date(house$saledate, format="%d/%m/%Y")),]
View(house)

# 1.ARIMA model
library('tseries')
library(forecast)
house <- house[order(as.Date(house$saledate, format="%d/%m/%Y")),]

# Preparation
avg_house=house%>%
  group_by(saledate)%>%
  summarize(avg_price=mean(MA,na.rm=T))
avg_house <- avg_house[order(as.Date(avg_house$saledate, format="%d/%m/%Y")),]

tsData <- ts(avg_house$avg_price,start=c(2007,3),frequency=4)
print(ts(avg_house$avg_price,start=c(2007,3),frequency=4),calendar = T)

# Original Data Visualization
autoplot(tsData,main="Quarter House Price Time Series", ylab="HousePrice")
# ADF test
adf.test(tsData)
# p-value > 0.05, not stational
fitARIMA <- auto.arima(tsData)
fitARIMA
# Calculate the R-Squared
r2<- cor(fitted(fitARIMA),tsData)^2
r2
# plot the fit line
autoplot(forecast(fitARIMA,h=12), ylab="HousePrice",main="ARIMA Quarterly House Price Forecast")

plot(tsData,type="l",lty=1,lwd=2, ylab="HousePrice",main="ARIMA Quarterly House Price Fit Plot")
lines(fitted(fitARIMA),col="blue",lty=1,lwd=1)

# Calculate the ARIMA Residual
arima_p = fitted(fitARIMA)[48:50]
real = tsData[49:51]
res_arima = real - arima_p
res_arima

# Calculate the ARIMA Out of Sample MAE and MSE
MSE <-sqrt(mean(res_arima^2))
MAE <-mean(abs(res_arima))
stdres_res_arima<-res_arima/sd(res_arima)

# 2.sar + TREND

# Model Preparation
nr <- nrow(avg_house)
numlag<-4
rmv<-(nr-numlag+1):nr
Price4<-c(rep(NA,numlag), avg_house$avg_price[-rmv])
Price12<-c(rep(NA,12), avg_house$avg_price[-c((nr-12+1):nr)])
# out of sample test
cut<-3
# time period
y_tp<-c((numlag+1):(nr-cut))
y<- avg_house$avg_price[y_tp]
Trend <- c(1:nr)
t<-Trend[y_tp]
ylag4<-Price4[y_tp]
ylag12
# model
model8<- lm(y~t+ylag4)
summary(model8)

# Fit
PV0<-predict(model8, data.frame(t=t,ylag4=ylag4))

plot(t,y,type="l",col="black",ylim = c(500000,750000), main ="Trend+SAR (YEAR,lag12) Fit Plot",ylab="",xlab="")
par(new=TRUE)
plot(t,PV0,type="l",col="blue",ylim = c(500000,750000),ylab="HousePrice",xlab="Trend")
legend("topleft",pch=c(15,15),legend=c("Original","Model"),col=c("black","blue"),bty="n")

# 3. SAR + Trend + Trend^2 Model
t2 <- t^2
model9<- lm(y~t+t2+ylag4)
summary(model9)

# Residual Plot
res2 <- resid(model9)
plot(res2~t2,axes = FALSE, xlab="Trend^2",ylab="Residual",main="Residual vs trend^2")
axis(side=1,pos=0)
axis(side=2)

# Fit
PV1<-predict(model9, data.frame(t=t,t2=t2,ylag4=ylag4))

plot(t,y,type="l",col="black",ylim = c(500000,850000),xlim=c(0,60),xlab = "Trend",ylab="HousePrice",main="SAR+Trend+Trend^2 fit line")
par(new=TRUE)
plot(c(47:55),PV2,type="l",col="red",ylim = c(500000,850000),xlim=c(0,60),xlab = "",ylab="")
par(new=TRUE)
plot(t,PV1,type="l",col="blue",ylim = c(500000,850000),xlim=c(0,60),xlab = "",ylab="")
legend("topleft",pch=c(15,15),legend=c("Original","Model","forcast"),col=c("black","blue","red"),bty="n")

# Forecast
PV2<-predict(model9, data.frame(t=c(47:55),t2=c(47:55)^2,ylag4=c(y[39:43],PV1[40:43])))

# Calculate the model out of sample test MAE MSE
MSE <-sqrt(mean(res2[41:43]^2))
MAE <-mean(abs(res2[41:43]))
stdres<-res2/sd(res2)

# Normal Distribution, Q Q line
qqnorm(stdres)
qqline(stdres, col = 'red')

##############################################################################################
################## Part3 : Linear Regression##################################################
##############################################################################################
df_1 <- read.csv("raw_sales.csv")
view(df_1)

# First LR model
model_1<- lm(df_1$price~df_1$bedrooms)
summary(model_1)

# More Preprocessing
bedrooms <- data.frame(bedrooms = df_1$bedrooms)
pred <- predict(model_1, newdata = bedrooms)
df_1$pred <- pred

m <-with(df_1, ave(price, findInterval(bedrooms, c(0, 1, 2, 3, 4, 5)), FUN= mean))
m <- m[!duplicated(m)]
m <- m[order(m)]
mean_price <- data.frame(mean_price = m, bedrooms = c(0:5))
bedrooms <- data.frame(bedrooms = c(0:5))
p <- df_1$pred
p <- p[!duplicated(p)]
mean_price$pred <- p[order(p)]

view(mean_price)
# Visualization
ggplot(data = mean_price, aes(x=bedrooms))+
  geom_line(aes(y=mean_price, color = 'mean_price')) +
  geom_line(aes(y=pred, color = 'pred')) +
  scale_color_manual("", breaks = c('mean_price', 'pred'), values = c('blue', 'red')) +
  ggtitle('Linear regression prediction')

# Residual Analysis
ggplot(data = mean_price, aes(x=bedrooms, y=mean_price-pred))+
  geom_point(aes(color = 'red', size = 1)) + ylab("residual") + ggtitle("residual vs. bedrooms")

df_1$house <- ifelse(df_1$propertyType == 'house', 1, 0)
df_1$unit <- ifelse(df_1$propertyType == 'unit', 1, 0)

# Second Model
model_2<- lm(df_1$price~df_1$bedrooms+df_1$house)
summary(model_2)

pred2 <- predict(model_1, newdata = data.frame(bedrooms = df_1$bedrooms, house=df_1$house))
df_1$pred2 <- pred2






