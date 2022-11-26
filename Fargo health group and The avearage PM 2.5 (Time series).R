# project timeseries
#---------------------------------------------------------------------------

# fargo health
install.packages("readxl")
library("readxl")
data<-read_excel('cleaned_ts_nodate.xlsx')
data


data <- ts(data, start=c(2006, 1), end=c(2013, 12), frequency=12)
plot(data,ylab="Incoming Examinations",xlab="Year")
# ???????????????????????????????????????????????????????????????????????? ???????????????seasonal

# test Kruskal-wallis seasonality test
# install.packages('seastests')
# library(seastests)
# isSeasonal(data, test="combined", freq = 12)


# Holt's linear trend method

library(forecast)
holt.additive <- ets(data, model="AAN")
holt.additive 
plot(holt.additive)
# summary(holt.additive)

# Forecast the future (12 steps ahead)
fc <- forecast(holt.additive, h = 12)
plot(fc,ylab="Incoming Examinations",xlab="Year")




# validation 
forecast <- forecast(holt.additive)$fitted
plot(forecast,ylab="Incoming Examinations",xlab="Year")
accuracy(forecast,data)

#---------------------------------------------------------------------------
# PM 2.5 (1) ????????????????????????????????????????????? model ?????????
library("readxl")
df<-read_excel('pm25average.xlsx')
df

# 1
library(forecast)
df = ts(df, start = 2012, frequency = 12)
df
plot(df,ylab="The average PM 2.5",xlab="Year")

# 2. Seasonal Decomposition #####
decompose(df)
plot(decompose(df))

# 3. Holt-Winter smoothimg 
ets(df)


# 4. Forecast plot (5 years = 60 months)
holt_pm25 = ets(df)

fc <- forecast(holt_pm25, h = 12)
fc
plot(fc,ylab="The average PM 2.5",xlab="Year")


# 5. validation 
forecast_pm25 <- forecast(holt_pm25)$fitted  #????????? forecast 1/2012-06/2022
forecast_pm25

plot(forecast_pm25,ylab="The average PM 2.5",xlab="Year")
accuracy(forecast_pm25,df)


#---------------------------------------------------------------------------

# PM 2.5 (2)  ??????????????? model ?????????

# 3. Holt-Winter smoothimg 
ets(df, model = "ANA")  # ????????? MAPE ??????????????????????????????????????? MNM


# 4. Forecast plot (5 years = 60 months)
holt_pm25_2 = ets(df, model = "ANA")

fc2 <- forecast(holt_pm25_2, h = 12)
fc2
plot(fc2,ylab="The average PM 2.5",xlab="Year")


# 5. validation 
forecast_pm25_2 <- forecast(holt_pm25_2)$fitted  #????????? forecast 1/2012-06/2022
forecast_pm25_2

plot(forecast_pm25_2,ylab="The average PM 2.5",xlab="Year")
accuracy(forecast_pm25_2,df)