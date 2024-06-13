#install.packages("readr")
#install.packages("colorspace")
#install.packages("vctrs")
#install.packages("gtrendsR")


library(gtrendsR)
library(ggplot2)
library(ggfortify)
library(plotly)
library(timeDate)
library(timeSeries)
library(fpp2)
library(zoo)

library(glmnet)
library(readr)
library(colorspace)
library(vctrs)

library(fpp2)
library(Hmisc)
library(foreign)



p20 <- glm(recession_t ~ yield_curve_t_l, family=binomial(link="probit"))
yield_prob <- predict(p20, type = 'response')
plot(yield_prob)

write.csv(data.frame(years_yc[13:486],recession_t[13:486],yield_prob), "yield_model.csv" )

## Credentials ##

## for recession 

library(Quandl)
Quandl.api_key("vvvvv")
rec  = Quandl("FRED/USREC",type = "ts", start_date="2004-01-01")
sm.Prob  = Quandl("FRED/RECPROUSM156N",type = "ts", start_date="2004-01-01")/100

#yield_curve  = Quandl("FRED/T10Y3M",type = "ts", start_date="2004-01-01")





##
inflation <- gtrends(c("inflation"), time = "all", geo =c("US"))

inflation <- inflation$interest_over_time$hits

##


jobs <- gtrends(c("jobs"), time = "all", geo =c("US"))

jobs <- jobs$interest_over_time$hits


coupons <- gtrends(c(""), cat = "365", time = "all", geo =c("US"))

coupons <- coupons$interest_over_time$hits

re <- gtrends(c(""), cat = "1080", time = "all", geo =c("US"))
re <- re$interest_over_time$hits
bankR <- gtrends(c(""), cat = "423", time = "all", geo =c("US"))
bankR <- bankR$interest_over_time$hits
apparel <- gtrends(c(""), cat = "68", time = "all", geo =c("US"))
apparel <- apparel$interest_over_time$hits
machinery <- gtrends(c(""), cat = "837", time = "all", geo =c("US"))
machinery <- machinery$interest_over_time$hits
un <- gtrends(c("unemployment"), cat = "0", time = "all", geo =c("US"))
un <- un$interest_over_time$hits
##
wu <- gtrends(c("unemployment insurance"), cat = "0", time = "all", geo =c("US"))
wu <- wu$interest_over_time$hits
debt <- gtrends(c("debt relief"), cat = "0", time = "all", geo =c("US"))
debt <- debt$interest_over_time$hits

whols <- gtrends(c(""), cat = "1225", time = "all", geo =c("US"))
whols <- whols$interest_over_time$hits
creditc <- gtrends(c("credit cards"), cat = "18", time = "all", geo =c("US"))
creditc <- creditc$interest_over_time$hits
mort <- gtrends(c("mortgage"), cat = "0", time = "all", geo =c("US"))
mort<- mort$interest_over_time$hits
aut <- gtrends(c(""), cat = "1190", time = "all", geo =c("US"))
aut <- aut$interest_over_time$hits
commv <- gtrends(c(""), cat = "1214", time = "all", geo =c("US"))
commv <- commv$interest_over_time$hits
oilgas <- gtrends(c(""), cat = "659", time = "all", geo =c("US"))
oilgas <- oilgas$interest_over_time$hits
constr <- gtrends(c(""), cat = "48", time = "all", geo =c("US"))
constr <- constr$interest_over_time$hits
foodr <- gtrends(c(""), cat = "121", time = "all", geo =c("US"))
foodr <- foodr$interest_over_time$hits
carrent <- gtrends(c(""), cat = "205", time = "all", geo =c("US"))
carrent <- carrent$interest_over_time$hits
suv <- gtrends(c(""), cat = "610", time = "all", geo =c("US"))
suv <- suv$interest_over_time$hits

## seasonal adj
end_month <- 04
end_year <- 2024
## seasonal ##
jobs.ts <- ts(jobs, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ))
jobs.stl <- stl(jobs.ts, s.window = "periodic")
job.sa <- jobs.stl$time.series[,2]+jobs.stl$time.series[,3]

autoplot(job.sa)
coupons.ts <- ts(coupons, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
coupons.stl <- stl(coupons.ts, s.window = "periodic")
coupons.sa <- coupons.stl$time.series[,2] + coupons.stl$time.series[,3]

autoplot(coupons.sa)
re.ts <- ts(re, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
re.stl <- stl(re.ts, s.window = "periodic")
re.sa <- re.stl$time.series[,2] + re.stl$time.series[,3]
autoplot(re.sa)

debt.ts <- ts(debt, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
debt.stl <- stl(debt.ts, s.window = "periodic")
debt.sa <- debt.stl$time.series[,2] + debt.stl$time.series[,3]
autoplot(debt.sa)

bankr.ts <- ts(bankR, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
bankr.stl <- stl(bankr.ts, s.window = "periodic")
bankr.sa <- bankr.stl$time.series[,2] + bankr.stl$time.series[,3]
autoplot(bankr.sa)

machinery.ts <- ts(machinery, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
machinery.stl <- stl(machinery.ts, s.window = "periodic")
machinery.sa <- machinery.stl$time.series[,2]+machinery.stl$time.series[,3]
autoplot(machinery.sa)
autoplot(apparel.sa)

apparel.ts <- ts(apparel, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
apparel.stl <- stl(apparel.ts, s.window = "periodic")
apparel.sa <- apparel.stl$time.series[,2] + apparel.stl$time.series[,3]

un.ts <- ts(un, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
un.stl <- stl(un.ts, s.window = "periodic")
un.sa <- un.stl$time.series[,2] + un.stl$time.series[,3]

autoplot(un.sa)
##
autoplot(wu.sa)
wu.ts <- ts(wu, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
wu.stl <- stl(wu.ts, s.window = "periodic")
wu.sa <- wu.stl$time.series[,2] + un.stl$time.series[,3]
##
whols.ts <- ts(whols, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
whols.stl <- stl(whols.ts, s.window = "periodic")
whols.sa <- whols.stl$time.series[,2] + whols.stl$time.series[,3]

autoplot(whols.sa)

creditc.ts <- ts(creditc, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
creditc.stl <- stl(creditc.ts, s.window = "periodic")
creditc.sa <- creditc.stl$time.series[,2] + creditc.stl$time.series[,3]

autoplot(creditc.sa)


mort.ts <- ts(mort, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
mort.stl <- stl(mort.ts, s.window = "periodic")
mort.sa <- mort.stl$time.series[,2] + mort.stl$time.series[,3]
autoplot(mort.sa)

aut.ts <- ts(aut, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
aut.stl <- stl(aut.ts, s.window = "periodic")
aut.sa <- aut.stl$time.series[,2] + aut.stl$time.series[,3]

commv.ts <- ts(commv, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
commv.stl <- stl(commv.ts, s.window = "periodic")
commv.sa <- commv.stl$time.series[,2] + commv.stl$time.series[,3]

autoplot(oilgas.sa)

oilgas.ts <- ts(oilgas, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
oilgas.stl <- stl(oilgas.ts, s.window = "periodic")
oilgas.sa <- oilgas.stl$time.series[,2] + oilgas.stl$time.series[,3]

constr.ts <- ts(constr, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
constr.stl <- stl(constr.ts, s.window = "periodic")
constr.sa <- constr.stl$time.series[,2] + constr.stl$time.series[,3]


foodr.ts <- ts(foodr, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
foodr.stl <- stl(foodr.ts, s.window = "periodic")
foodr.sa <- foodr.stl$time.series[,2] + foodr.stl$time.series[,3]

autoplot(foodr.sa)

carrent.ts <- ts(carrent, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
carrent.stl <- stl(carrent.ts, s.window = "periodic")
carrent.sa <- carrent.stl$time.series[,2] + carrent.stl$time.series[,3]

autoplot(carrent.sa)

suv.ts <- ts(suv, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
suv.stl <- stl(suv.ts, s.window = "periodic")
suv.sa <- suv.stl$time.series[,2] + suv.stl$time.series[,3]

rec.ts <- ts(rec, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )
sm.prob.ts <- ts(sm.Prob, frequency = 12, start=c(2004, 1), end=c(end_year, end_month ) )

#yield_curve.ts <- ts(yield_curve, frequency = 12, start=c(2004, 1), end=c(2022, 6) )
#yield_curve_lag <- Lag(yield_curve,12)

data.frame(rec.ts, yield_curve_lag, yield_curve.ts )

write.csv(data.frame(constr.sa , debt.sa, oilgas.sa , creditc.sa , bankr.sa , un.sa ,coupons.sa , apparel.sa , 
                     job.sa , re.sa , machinery.sa , whols.sa ,commv.sa , carrent.sa,suv.sa ), "searches.csv")
p20 <- glm(rec.ts ~ yield_curve_lag, family=binomial(link="probit"))
yield_prob <- predict(p20, type = 'response')
## machine learning 


#dummies changes in Google trends engine 
dummy1 <- numeric(length(rec.ts)) 

for (i in 1:length(rec.ts)) {dummy1[i] <-ifelse(i>=85, 1,0)}

dummy2 <- numeric(length(rec.ts)) 

for (i in 1:length(rec.ts)) {dummy2[i] <-ifelse(i>=139, 1,0)}

dummy3 <- numeric(length(rec.ts)) 

for (i in 1:length(rec.ts)) {dummy3[i] <-ifelse(i>=217, 1,0)}
# lasso 


xy <- data.frame(rec.ts, dummy1, dummy2, dummy3, constr.sa , debt.sa, oilgas.sa , creditc.sa , bankr.sa , un.sa ,coupons.sa , apparel.sa , job.sa , re.sa , machinery.sa , whols.sa ,commv.sa , carrent.sa,suv.sa )

machinel.1 <- model.matrix(rec.ts ~ dummy1 + dummy2 + dummy3 + constr.sa + debt.sa + oilgas.sa + creditc.sa + bankr.sa +  un.sa + coupons.sa + apparel.sa + job.sa + re.sa + machinery.sa + whols.sa + commv.sa + carrent.sa + suv.sa )[,-1]

x        <- as.matrix(data.frame(machinel.1))
x
rec.max <- as.matrix(rec.ts)
# cross validation

set.seed(123) 
cv <- cv.glmnet(x = x, y = rec.max, alpha = 1)


## best lambda

best.lambda <- cv$lambda.min
best.lambda
# fit model 
glmmod_lasso <- glmnet(x, rec.max, alpha= 1, lambda = best.lambda, family="binomial")
plot(glmmod_lasso)
coef(glmmod_lasso)





# prediction in sample 
predictions <- predict(glmmod_lasso, type = "response", newx=x)
predictions
plot(predictions, type = "l")
length(end_month)
years <- as.Date(seq(ISOdate(2004,1,1), by = "month", length.out = 244), format = "%d/%m/%Y")

library(plotly)
p_pcg <- plot_ly(x = ~years) %>%
  add_trace(y = ~rec.max, name = 'Recession', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = 'rgba(255, 212, 96, 0.5)') %>%
  add_trace(y = ~predictions, name = "Google-Based Model", mode ='lines' )
p_pcg

add_trace(y = ~sm.prob.ts, name = "Smoothed Probabilites", mode ='lines' ) 
add_trace(y = ~yield_prob[253:474], name = "Yield Curve", mode ='lines' ) %>%
  #credentials
  Sys.setenv("plotly_username"="grojasmatute")
Sys.setenv("plotly_api_key"="CCHhbi6CwBTyOiA3rbIc")

chart_link = api_create(p_pcg, filename="Google-Based Model 2")

rec_prob <- data.frame(years, predictions, rec.max, yield_prob[253:474], sm.prob.ts)
rec_prob <- data.frame(years, predictions, rec.max)
rec_prob <- write.csv(rec_prob, "rec_prob.csv")
rec_prob[200:222,]
plot(yield_prob)

yield_prob[253:474]

write.csv( data.frame(years, predictions, constr.sa , debt.sa, oilgas.sa , creditc.sa , bankr.sa , un.sa ,coupons.sa , apparel.sa , job.sa , re.sa , machinery.sa , whols.sa ,commv.sa , carrent.sa,suv.sa ), "prediction_recession.csv")

write.csv(data.frame(inflation),"inflation_searches.csv")
