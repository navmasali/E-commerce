#Loading the required libraries
library(forecast)
library(tseries)
require(graphics)
library(dplyr)
library(tidyr)
library(ggplot2)

#Loading the file into R
data <- read.csv("Global Superstore.csv")
summary(data)
#cleaned_data prepartion

#Retaining only the required columns
cleaned_data <- data[,c(3,8,13,19,20,22)]
summary(cleaned_data)

#Converting Order.Date into Date format
cleaned_data$Order.Date <- as.character(cleaned_data$Order.Date)

#seperating date in day month year
temp <- cleaned_data$Order.Date
cleaned_data <- separate(data = cleaned_data,col = Order.Date,into = c('Day','Month','Year'),sep = "-")
#Verify the range of the data variables ie. Month, Day and Year
range(cleaned_data$Month)
range(cleaned_data$Day)
range(cleaned_data$Year)

#Sorting the cleaned_data set by Order.Date as cleaned_data is not in chronological order
cleaned_data$Order.Date <- as.Date.character(temp,format = "%d-%m-%Y")
head(cleaned_data$Order.Date,n = 20)

cleaned_data<-cleaned_data[order(cleaned_data$Order.Date),]
head(cleaned_data$Order.Date,n = 20)
tail(cleaned_data$Order.Date,n = 20)

summary(cleaned_data)

#checking profits of each sub segment


Profit<- vector(mode = "numeric",length = 21)
Coeffvar <- vector(mode = "numeric",length = 21)
bucketsnames <- vector(mode = "numeric",length = 21)

sm.df <- list2env(split(cleaned_data, list(cleaned_data$Segment,cleaned_data$Market)),envir=.GlobalEnv)

for (i in 1:21){
  
  name <- paste("bucket",i,sep="")
  sp.df <- split(cleaned_data, list(cleaned_data$Segment,cleaned_data$Market))
  temp <- as.data.frame(sp.df[[i]])
  Profit[[i]] <- as.numeric(sum(temp$Profit))
  Coeffvar[[i]] <- as.numeric(round(sd(temp$Profit)/mean(temp$Profit),2))
  bucketsnames[[i]] <- name 
  assign(name, temp)
}

profit_df<- cbind.data.frame(bucketsnames,Profit, Coeffvar)

ggplot(profit_df, aes(x=profit_df$Profit, y=profit_df$Coeffvar)) + geom_point()

#Bucket 4(Consumer-APAC) and 13(Consumer-EU) are the most profitable

#Aggregating All the data to Create TS

duration<-c(1:48)

#Consumer.APAC
Consumer.APAC$Month<-as.numeric(Consumer.APAC$Month)
Consumer.APAC$Year<-as.numeric(Consumer.APAC$Year)

Consumer.APAC$Month[which(Consumer.APAC$Year==2012)] <- Consumer.APAC$Month[which(Consumer.APAC$Year==2012)]+12
Consumer.APAC$Month[which(Consumer.APAC$Year==2013)] <- Consumer.APAC$Month[which(Consumer.APAC$Year==2013)]+24
Consumer.APAC$Month[which(Consumer.APAC$Year==2014)] <- Consumer.APAC$Month[which(Consumer.APAC$Year==2014)]+36

sales_Consumer.APAC <- vector(mode="numeric",length = 48)
quantity_Consumer.APAC <- vector(mode="numeric",length = 48)


for(i in 1:48){
  sales_Consumer.APAC[i] <- sum(Consumer.APAC$Sales[which(Consumer.APAC$Month == i)])
  quantity_Consumer.APAC[i]<- sum(Consumer.APAC$Quantity[which(Consumer.APAC$Month == i)])
}

sales_Consumer_APAC_df <- as.data.frame(cbind(duration,sales_Consumer.APAC))
colnames(sales_Consumer_APAC_df) <- c('Month','Sales')
quantity_Consumer_APAC_df <- as.data.frame(cbind(duration,quantity_Consumer.APAC))
colnames(quantity_Consumer_APAC_df) <- c('Month','Quantity')


#Time Series for Consumer.APAC, Sales and Quantity
Consumer.APAC_sales_ts <- ts(sales_Consumer_APAC_df)
Consumer.APAC_qnty_ts <- ts(quantity_Consumer_APAC_df)

# Plot the time series
plot(Consumer.APAC_sales_ts)
plot(Consumer.APAC_qnty_ts)

#######################################################################
#Bucket 13(Consumer.EU)
Consumer.EU$Month<-as.numeric(Consumer.EU$Month)
Consumer.EU$Year<-as.numeric(Consumer.EU$Year)

Consumer.EU$Month[which(Consumer.EU$Year==2012)] <- Consumer.EU$Month[which(Consumer.EU$Year==2012)]+12
Consumer.EU$Month[which(Consumer.EU$Year==2013)] <- Consumer.EU$Month[which(Consumer.EU$Year==2013)]+24
Consumer.EU$Month[which(Consumer.EU$Year==2014)] <- Consumer.EU$Month[which(Consumer.EU$Year==2014)]+36

sales_Consumer.EU <- vector(mode="numeric",length = 48)
quantity_Consumer.EU <- vector(mode="numeric",length = 48)

for(i in 1:48){
  sales_Consumer.EU[i] <- sum(Consumer.EU$Sales[which(Consumer.EU$Month==i)])
  quantity_Consumer.EU[i] <- sum(Consumer.EU$Quantity[which(Consumer.EU$Month==i)])
}

sales_Consumer_EU_df <- as.data.frame(cbind(duration,sales_Consumer.EU))
colnames(sales_Consumer_EU_df) <- c('Month','Sales')
quantity_Consumer_EU_df <- as.data.frame(cbind(duration,quantity_Consumer.EU))
colnames(quantity_Consumer_EU_df) <- c('Month','Quantity')

#Time Series for Consumer.EU, Sales and Quantity

Consumer.EU_sales_ts<-ts(sales_Consumer_EU_df)
Consumer.EU_qnty_ts<-ts(quantity_Consumer_EU_df)

# Plot the time series
plot(Consumer.EU_sales_ts)
plot(Consumer.EU_qnty_ts)

##########END OF DATA PREPERATION##########

###########Time series modelling on the below 4 time series############
#Consumer.APAC_sales_ts
#Consumer.APAC_qnty_ts
#Consumer.EU_sales_ts
#Consumer.EU_qnty_ts

###########Time series modelling############
#Let's create a trainig set of 42 records
# and 6 records for testing the model

#Start Consumer APAC sales Modelling
total_timeser1<-ts(sales_Consumer_APAC_df$Sales)
indata1<-sales_Consumer_APAC_df[1:42,]
timeser1<-ts(indata1$Sales)
plot(timeser1)
str(timeser1)


#Smoothing the series - Moving Average Smoothing
#using moving avarage to soften the peaks and normalising it
w = 1

smoothseries1 <- stats::filter(timeser1, 
                               filter=rep(1/(2*w+1),(2*w+1)), 
                               method='convolution', sides=2)
lines(smoothseries1, col = "red")
w <- 2
smoothseries2 <- stats::filter(timeser1, 
                               filter=rep(1/(2*w+1),(2*w+1)), 
                               method='convolution', sides=2)
lines(smoothseries2, col = "blue")

smoothseries3<-ma(x = timeser1,order = 2,centre = T)
lines(smoothseries3,col="green")

# based on our calculations we take order=2 as it softens the peaks 
# but doesnot flatten it 
smoothedseries <- smoothseries3
w <- 2
#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser1)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series
plot(timeser1)
timevals_in <- indata1$Month
lines(smoothedseries, col="red", lwd=2)

# We have succesfully smoothened the series for further use, we have arrested local trend

#Building a model on the smoothed time series using classical decomposition
#converting the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#working an additive model with trend and seasonality to the data

#Seasonality and trend will be modeled togetherby using a sinusoid function

lmfit1 <- lm(Sales ~ sin(0.5*Month)+ Month, data=smootheddf)
global_pred <- predict(lmfit1, Month=timevals_in)
summary(global_pred)
summary(lmfit1)

## based of summary examination all parameters are important
#plotting series
plot(timeser1)
lines(smoothedseries, col="red", lwd=2)
lines(timevals_in, global_pred, col='blue', lwd=2)
# Remaining locally predictable series
# we will try modeling it as ARMA series

local_pred <- timeser1-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#to check residual series is white noise

resi <- local_pred-fitted(armafit)
plot(local_pred, col='red', type = "l")
lines(resi,col="blue")
adf.test(resi,alternative = "stationary")
kpss.test(resi)

# model evaluation 

valdata <- sales_Consumer_APAC_df[43:48,]

timevals_out <- valdata$Month

global_pred_out <- predict(lmfit1,data.frame(Month =timevals_out))

forecast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(forecast,valdata[,2])[5]
MAPE_class_dec
#MAPE=21.03181
# predicting the entire series to see the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser1, col = "black", main = "Sales Prediction model - Classical decomposition", xlab ="Months", ylab = "Sales")
lines(class_dec_pred, col = "red")

# auto.arima fit assessment

autoarima <- auto.arima(timeser1)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Residual Analysis (to ascertain white noise resemblance)

resi_auto_arima <- timeser1 - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#auto.arima model evaluation
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,valdata[,2])[5]
MAPE_auto_arima
#MAPE=27.68952
#Final plot of auto.arima model completed fitted values vs actual values

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser1, col = "black", main = "Sales Prediction model - Auto Arima", xlab ="Months", ylab = "Sales")
lines(auto_arima_pred, col = "red")

#############################################

#Start Consumer APAC Quantity Modelling
total_timeser2<-ts(quantity_Consumer_APAC_df$Quantity)
indata2<-quantity_Consumer_APAC_df[1:42,]
timeser2<-ts(indata2$Quantity)
plot(timeser2)
str(timeser2)


#Smoothing the series - Moving Average Smoothing
#using moving avarage to soften the peaks and normalising it
w = 1

smoothseries1 <- stats::filter(timeser2, 
                               filter=rep(1/(2*w+1),(2*w+1)), 
                               method='convolution', sides=2)
lines(smoothseries1, col = "red")
w <- 2
smoothseries2 <- stats::filter(timeser2, 
                               filter=rep(1/(2*w+1),(2*w+1)), 
                               method='convolution', sides=2)
lines(smoothseries2, col = "blue")

smoothseries3<-ma(x = timeser2,order = 2,centre = T)
lines(smoothseries3,col="green")

# based on our calculations we take order=2 as it softens the peaks 
# but doesnot flatten it 
smoothedseries <- smoothseries3
w <- 2
#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser2)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series
plot(timeser2)
timevals_in <- indata2$Month
lines(smoothedseries, col="red", lwd=2)

# We have succesfully smoothened the series for further use, we have arrested local trend

#Building a model on the smoothed time series using classical decomposition
#converting the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

#working an additive model with trend and seasonality to the data

#Seasonality and trend will be modeled togetherby using a sinusoid function

lmfit2 <- lm(Quantity ~ sin(0.5*Month)+ Month, data=smootheddf)
global_pred <- predict(lmfit2, Month=timevals_in)
summary(global_pred)
summary(lmfit2)

## based of summary examination all parameters are important
#plotting series
plot(timeser2)
lines(smoothedseries, col="red", lwd=2)
lines(global_pred, col='blue', lwd=2)
# Remaining locally predictable series
# we will try modeling it as ARMA series

local_pred <- timeser2-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#to check residual series is white noise

resi <- local_pred-fitted(armafit)
plot(local_pred, col='red', type = "l")
lines(resi,col="blue")
adf.test(resi,alternative = "stationary")
kpss.test(resi)

# model evaluation 

valdata <- quantity_Consumer_APAC_df[43:48,]

timevals_out <- valdata$Month

global_pred_out <- predict(lmfit2,data.frame(Month =timevals_out))

forecast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(forecast,valdata[,2])[5]
MAPE_class_dec
#MAPE=23.72372

# predicting the entire series to see the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser2, col = "black", main = "Quantity Prediction model - Classical decomposition", xlab ="Months", ylab = "Quantity")
lines(class_dec_pred, col = "red")

# auto.arima fit assessment

autoarima <- auto.arima(timeser2)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Residual Analysis (to ascertain white noise resemblance)

resi_auto_arima <- timeser2 - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#auto.arima model evaluation
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,valdata[,2])[5]
MAPE_auto_arima
#MAPE=26.24458
#Final plot of auto.arima model completed fitted values vs actual values

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser2, col = "black", main = "Quantity Prediction model - Auto Arima", xlab ="Months", ylab = "Quantity")
lines(auto_arima_pred, col = "red")

#############################################

#Start Consumer EU sales Modelling
total_timeser3<-ts(sales_Consumer_EU_df$Sales)
indata3<-sales_Consumer_EU_df[1:42,]
timeser3<-ts(indata3$Sales)
plot(timeser3)
str(timeser3)


#Smoothing the series - Moving Average Smoothing
#using moving avarage to soften the peaks and normalising it
w = 1

smoothseries1 <- stats::filter(timeser3, 
                               filter=rep(1/(2*w+1),(2*w+1)), 
                               method='convolution', sides=2)
lines(smoothseries1, col = "red")
w <- 2
smoothseries2 <- stats::filter(timeser3, 
                               filter=rep(1/(2*w+1),(2*w+1)), 
                               method='convolution', sides=2)
lines(smoothseries2, col = "blue")

smoothseries3<-ma(x = timeser3,order = 2,centre = T)
lines(smoothseries3,col="green")

# based on our calculations we take order=2 as it softens the peaks 
# but doesnot flatten it 
smoothedseries <- smoothseries3
w <- 2
#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser3)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series
plot(timeser3)
timevals_in <- indata3$Month
lines(smoothedseries, col="red", lwd=2)

# We have succesfully smoothened the series for further use, we have arrested local trend

#Building a model on the smoothed time series using classical decomposition
#converting the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#working an additive model with trend and seasonality to the data

#Seasonality and trend will be modeled togetherby using a sinusoid function

lmfit3 <- lm(Sales ~ sin(0.5*Month)  + cos(0.5*Month)+ Month, data=smootheddf)
global_pred <- predict(lmfit3, Month=timevals_in)
summary(global_pred)
summary(lmfit1)



## based of summary examination all parameters are important
#plotting series
plot(timeser3)
lines(smoothedseries, col="red", lwd=2)
lines(timevals_in, global_pred, col='blue', lwd=2)
# Reamining locally predictable series
# we will try modeling it as ARMA series

local_pred <- timeser3-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#to check residual series is white noise

resi <- local_pred-fitted(armafit)
plot(local_pred, col='red', type = "l")
lines(resi,col="blue")
adf.test(resi,alternative = "stationary")
kpss.test(resi)

# model evaluation 

valdata <- sales_Consumer_EU_df[43:48,]

timevals_out <- valdata$Month

global_pred_out <- predict(lmfit3,data.frame(Month =timevals_out))

forecast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(forecast,valdata[,2])[5]
MAPE_class_dec
#MAPE=25.73527
# predicting the entire series to see the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser3, col = "black", main = "Sales Prediction - Classical Decomposition", xlab = "Months", ylab = "Sales")
lines(class_dec_pred, col = "red")

# auto.arima fit assessment

autoarima <- auto.arima(timeser3)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Residual Analysis (to ascertain white noise resemblance)

resi_auto_arima <- timeser3 - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#auto.arima model evaluation
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,valdata[,2])[5]
MAPE_auto_arima
#MAPE=28.9226
#Final plot of auto.arima model completed fitted values vs actual values

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser3, col = "black", main = "Sales Prediction - Auto Arima", xlab = "Months", ylab = "Sales")
lines(auto_arima_pred, col = "red")

########################################################

#Start Consumer EU Quantity Modelling
total_timeser4<-ts(quantity_Consumer_EU_df$Quantity)
indata4<-quantity_Consumer_EU_df[1:42,]
timeser4<-ts(indata4$Quantity)
plot(timeser4)
str(timeser4)


#Smoothing the series - Moving Average Smoothing
#using moving avarage to soften the peaks and normalising it
w <- 1

smoothseries1 <- stats::filter(timeser4, 
                               filter=rep(1/(2*w+1),(2*w+1)), 
                               method='convolution', sides=2)
alines(smoothseries1, col = "red")

w <- 2
smoothseries2 <- stats::filter(timeser4, 
                               filter=rep(1/(2*w+1),(2*w+1)), 
                               method='convolution', sides=2)
lines(smoothseries2, col = "blue")

smoothseries3<-ma(x = timeser4,order = 2,centre = T)
lines(smoothseries3,col="green")

# based on our calculations we take order=2 as it softens the peaks 
# but doesnot flatten it 
smoothedseries <- smoothseries3
w <- 2
#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser4)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series
plot(timeser4)
timevals_in <- indata4$Month
lines(smoothedseries, col="red", lwd=2)

# We have succesfully smoothened the series for further use, we have arrested local trend

#Building a model on the smoothed time series using classical decomposition
#converting the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

#working an additive model with trend and seasonality to the data

#Seasonality and trend will be modeled togetherby using a sinusoid function

lmfit4 <- lm(Quantity ~ sin(0.1 * Month) + poly(Month,1) , data=smootheddf)
global_pred <- predict(lmfit4, Month=timevals_in)
summary(global_pred)
summary(lmfit4)



## based of summary examination all parameters are important
#plotting series
plot(timeser4)
lines(smoothedseries, col="red", lwd=2)
lines(timevals_in, global_pred, col='blue', lwd=2)
# Reamining locally predictable series
# we will try modeling it as ARMA series

local_pred <- timeser4-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#to check residual series is white noise

resi <- local_pred-fitted(armafit)
plot(local_pred, col='red', type = "l")
lines(resi,col="blue")
adf.test(resi,alternative = "stationary")
kpss.test(resi)

# Even though the Dicker fuller test slightly fails for non stationarity, the MAPE value is 
# better for this particular model, hence we are going ahead with the model
# model evaluation 

valdata <- sales_Consumer_EU_df[43:48,]

timevals_out <- valdata$Month

global_pred_out <- predict(lmfit4,data.frame(Month =timevals_out))

forecast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(forecast,valdata[,2])[5]
MAPE_class_dec
#MAPE=99.01272

# predicting the entire series to see the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser4, col = "black", main = "Quantity Prediction - Classical Decomposition", xlab = "Months", ylab = "Quantity")
lines(class_dec_pred, col = "red")

# auto.arima fit assessment

autoarima <- auto.arima(timeser4)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Residual Analysis (to ascertain white noise resemblance)

resi_auto_arima <- timeser4 - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_ceq <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima_ceq <- accuracy(fcast_auto_arima_ceq$pred,valdata[,2])[5]
MAPE_auto_arima_ceq
#MAPE=99.03949
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_ceq <- c(fitted(autoarima),ts(fcast_auto_arima_ceq$pred))
plot(total_timeser4, col = "black", main = "Quantity Prediction - Auto Arima", xlab = "Months", ylab = "Quantity")
lines(auto_arima_pred_ceq, col = "red")

###################END OF MODELING####################
###################PREDICTING NEXT 6 MONTHS DATA########################

mnths <- as.vector(c(1:54))

#Prediction of Consumer APAC Sales
global_pred_out <- predict(lmfit1,data.frame(Month = mnths))
plot(total_timeser1,xlim=c(0,54),main="Prediction of Consumer APAC Sales",ylab="Sales",xlab="Months")
lines(ts(global_pred_out),col="Red")
sales_consumer_apac<-sum(global_pred_out[49:54])
sales_consumer_apac
#sales_consumer_apac - 320156.5

#Prediction of Consumer APAC Quantity
global_pred_out <- predict(lmfit2,data.frame(Month =mnths))
plot(total_timeser2,xlim=c(0,54),main="Prediction of Consumer APAC Quantity",ylab="Quantity",xlab="Months")
lines(ts(global_pred_out),col="Red")
quantity_consumer_apac<-sum(global_pred_out[49:54])
quantity_consumer_apac
#quantity_consumer_apac: 3551.26

#Prediction of Consumer EU Sales
global_pred_out <- predict(lmfit3,data.frame(Month =mnths))
plot(total_timeser3,xlim=c(0,54),main="Prediction of Consumer EU Sales",ylab="Sales",xlab="Months")
lines(ts(global_pred_out),col="Red")
sales_consumer_eu<-sum(global_pred_out[49:54])
sales_consumer_eu
#sales_consumer_eu: 238082.5

#Prediction of Consumer EU Quantity
global_pred_out <- predict(lmfit4,data.frame(Month =mnths))
plot(total_timeser4,xlim=c(0,54),main="Prediction of Consumer EU Quantity",ylab="Quantity",xlab="Months")
lines(ts(global_pred_out),col="Red")
quantity_consumer_eu<-sum(global_pred_out[49:54])
quantity_consumer_eu
#3245.998