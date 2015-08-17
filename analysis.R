library("forecast")
library("MASS")

#Set this to the appropriate directory
setwd("~/r/gold-analysis")

gold <- read.csv("data/doug_gold3.csv")
summary(gold)

#####CLEAN THE DATA #####

#Convert Excel dates to R dates
gold$Date <- as.Date(gold$Date,format="%m/%d/%Y")
summary(gold$Date)

#Convert columns into numeric values
indx <- sapply(gold, is.factor)
gold[indx] <- lapply(gold[indx], function(x) as.numeric(gsub(",", "", as.character(x))))
summary(gold)

#plot gold price over time
plot(gold$Date, gold$Gold, type="l")

?#create a time series
ts.gold <- ts(gold[2:2])
summary(ts.gold)
plot(ts.gold)


#Create a variable for gold next month and gold in 6 months
#These will be used as dependant variables

#Create a vector of values shifting gold forward one month
GoldNextMonth <- gold$Gold[2:nrow(gold)]

#Add a value for the last month, default to NA
GoldNextMonth[nrow(gold)] <- NA

#Now put new values into data frame
gold$GoldNextMonth <- GoldNextMonth

#Repeat for 6 months
GoldSixMonth <- gold$Gold[7:nrow(gold)]
GoldSixMonth[(nrow(gold)-6):nrow(gold)] <- NA
gold$GoldSixMonth <- GoldSixMonth

#Look at perecent change month-to-month as a log
plot(log(gold$GoldNextMonth/gold$Gold))

##### DIVIDE DATA INTO TRAINING / TEST ######

#Training set will be all data before 2007 to see if we can predict latest crash
gold.train <- subset(gold, as.Date(Date) <= '2007-01-01')
gold.train <- subset(gold.train, as.Date(Date) > '1972-12-31')

gold.test <- subset(gold, as.Date(Date) > '2007-01-01')


##### BUILD LINEAR MODEL #####
lm.gold <- lm(GoldNextMonth ~ Gold + S.P + US.Dollar.Index + CPI, data=gold.train)
summary(lm.gold)
plot(lm.gold)

#Test polynomial terms
lm.poly <- lm(GoldNextMonth ~ Gold*S.P*US.Dollar.Index*CPI, data=gold.train)
summary(lm.poly)

#Try backwards stepwise regression
lm.reduce.backwards <- step(lm.poly, direction="both")


#This is simply to delete the last row which doesn't have "next month's" data
gold.test <- gold.test[1:(nrow(gold.test)-1),] 

#Predict values based on the model you created
lm.gold.predict <- predict(lm.gold, gold.test) 

#calculate Root Mean Squared Error
test.rmse <- sqrt(mean((lm.gold.predict - gold.test$GoldNextMonth)^2)) 

# the lower this is the better
test.rmse 

plot(lm.gold.predict, gold.test$GoldNextMonth) # this will plot the predicted values vs actual values
#to do:
# 1. create evaluation metrics, like RMSE for predicted value
# 2. see how well this model predicts gold prices on test set


##### BUILD TIME SERIES ANALYSIS ####


##### TRY FRED API #####
#This is not complete

#fred <- fredAPI()
#> fred$key('dd31f41c6573d63ff0ea4c7a2c33a495')
#> xml <- fred$series_observations('GNPCA')

