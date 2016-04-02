library(ggplot2)
library(forecast)
library(rpart)
library(randomForest)


#Initial Trial: data from reported numbers, plus a quick OBIEE query back to 2005... projecting forward 5 years based on 10 year data seems questionable?
###############################################################################
commence_complete <- read.csv('2005_commence_complete.csv', header = TRUE)

commence_ts <- ts(commence_complete[,2], start=2005)
plot (commence_ts)

complete_ts <- ts(commence_complete[,3], start =2005)
plot(complete_ts)

ts.plot(complete_ts,commence_ts, col =c('red','blue'))
commence_ts
################################################################################
#ETS and HoltWinters forecasting methods are the top two refered to for time series predications
#(literature states ets should be more reliable in all situations)
?ets
?HoltWinters
#ets forecast for commencements
commence_fit_ets <- ets(commence_ts)
plot(commence_fit_ets)
commence_pred_ets <- forecast.ets(commence_fit_ets, h=5)
plot(commence_pred_ets)
lines(commence_fit_ets$states[,1],col='red')
#    #hmmm.. this does not look useable... fit inside data looks ok, but it appears to just be be projecting the last point of the fit?
#    #maybe not enough data to model accurately

#HoltWinters forecast for commencements
commence_fit_HW <- HoltWinters(commence_ts[1:11], gamma=F)
plot(commence_fit_HW)
commence_fit_HW
commence_pred_HW <- forecast.HoltWinters(commence_fit_HW, h=5)
plot(commence_pred_HW)
commence_pred_HW
#    #This Seems reasonable... but the in range model appears to diverge significantly from the observations....



#ets forecast for completions
complete_fit_ets <- ets(complete_ts)
plot(complete_fit_ets)
complete_pred_ets <- forecast.ets(complete_fit_ets, h=5)
plot(complete_pred_ets)
lines(complete_fit_ets$states[,1],col='red')
#     #again this does not look useable... but at least the model matches well to the observed......

#HoltWinters forecast completions
complete_fit_HW <- HoltWinters(complete_ts, gamma=FALSE)
plot(complete_fit_HW)
complete_pred_HW <- forecast.HoltWinters(complete_fit_HW, h=5)
plot(complete_pred_HW)
#     #shows a downward trend... highly unlikely given commencements look to be increasing, plus the model was signifcantly divergent. 
#     #maybe not enough data?



###############################################################################
#Start Again, with a bigger data set:
#Read in Data (sourced from OBIEE back to 1990 for completions, 1985 for commencements, due to 3 year lag, maybe can offset to look at trend?)
commence_complete <- read.csv('commence_complete.csv', header =TRUE)
comm_ts <- ts(commence_complete[,2], start=1985)
plot(comm_ts)
comp_ts <- ts(commence_complete[,3], start=1985)
plot(comp_ts)

ts.plot(comp_ts,comm_ts, col =c('red','blue'))

#Try offseting to see trend?...  will just rebuild the completions ts object, starting 3 years earlier
comp_ts_offset <- ts(comp_ts, start=1982)
ts.plot(comp_ts_offset, comm_ts, col=c('red','blue'))
#      #seems to be fairly well correlated, but doesn't really help with the prediction...


###############################################################################
#Lets try ets again
#ETS commencing:
comm_fit_ets <- ets(comm_ts)
plot(comm_fit_ets)
# looks like the model has properly decomposed the overall growth and error variation
comm_pred_ets <- forecast.ets(comm_fit_ets, h=5)
plot(comm_pred_ets)
lines(comm_fit_ets$states[,1],col='red')

comm_pred_ets
#    # this looks useable... red line shows the model fit inside the data, numbers seem reasonable for forward projections


#ETS completions:
comp_fit_ets <- ets(comp_ts)
plot(comp_fit_ets)
comp_pred_ets <- forecast.ets(comp_fit_ets, h=5)
plot(comp_pred_ets)
lines(comp_fit_ets$states[,1],col='red')

comp_pred_ets
#    # again this looks useable.... model has correclty split out the overlying trend, and inferred that the spike and drop in recent 
#    # years was likely annomolous (aligns with 2010ish completions, so maybe due to Melbourne Model changes...)


### Create Data frame based on commencing projections, then use logistic regression to predict completions:
df <- as.data.frame(commence_complete)
df[32,] <- c(2016, 9807,NA)
df[33,] <- c(2017,10035,NA)
df[34,] <- c(2018,10264,NA)
df[35,] <- c(2019,10493,NA)
df[36,] <- c(2020,10721,NA)

comp_fit_rpart <- rpart(Complete ~ Commence, data=df[!is.na(df$Complete),], method='anova')
df$Complete[is.na(df$Complete)] <- predict(comp_fit_rpart, df[is.na(df$Complete), ])
df
ggplot(df,aes (x=Year, y=Complete))+
  geom_line()
#no dice.

### Take 2 with Random Forest
df <- as.data.frame(commence_complete)
df[32,] <- c(2016, 9807,NA)
df[33,] <- c(2017,10035,NA)
df[34,] <- c(2018,10264,NA)
df[35,] <- c(2019,10493,NA)
df[36,] <- c(2020,10721,NA)

comp_fit_rf <- randomForest(Complete ~ Commence, data=df[!is.na(df$Complete),], minsplit=2)
plot(comp_fit_rf)
df$Complete[is.na(df$Complete)] <- predict(comp_fit_rf, df[is.na(df$Complete), ])
df
ggplot(df,aes (x=Year, y=Complete))+
  geom_line()
# no dice

### Take 3 with simple linear model
df <- as.data.frame(commence_complete)
df[32,] <- c(2016, 9807,NA)
df[33,] <- c(2017,10035,NA)
df[34,] <- c(2018,10264,NA)
df[35,] <- c(2019,10493,NA)
df[36,] <- c(2020,10721,NA)
df
comp_fit_lm <- lm(Complete ~ Year + Commence, data=df[!is.na(df$Complete),])
plot(comp_fit_lm)
#30,31, 27 identified as outliers in all models... corresponds to 2014,2015 and 2011

df$Complete[is.na(df$Complete)] <- predict(comp_fit_lm, df[is.na(df$Complete), ])

ggplot(df,aes (x=Year, y=Complete))+
  geom_line()
# well it did something.... lets compare to the ets time series predictions:

df[32:36,c(1,3)]
comp_pred_ets
# looks to be overestimating, but all lie within the ets 80% confidence window... maybe retry with outliers excluded??

### Take 4 with simple linear model - exclude outliers
df <- as.data.frame(commence_complete)
df[32,] <- c(2016, 9807,NA)
df[33,] <- c(2017,10035,NA)
df[34,] <- c(2018,10264,NA)
df[35,] <- c(2019,10493,NA)
df[36,] <- c(2020,10721,NA)
df
comp_fit_lm <- lm(Complete ~ Year + Commence, data=df[c(6:26,28:29),])
plot(comp_fit_lm)

df$Complete[is.na(df$Complete)] <- predict(comp_fit_lm, df[is.na(df$Complete), ])

ggplot(df,aes (x=Year, y=Complete))+
  geom_line()
df[32:36,c(1,3)]
# gah, worse... exluding the outliers appears to just amplify the overall growth.