##############################
##### Load Neccessary Packages
##############################

library(rtweet)
library(tidyverse)
library(tidytext)
library(textdata)
library(tidyquant)
library(lmtest)
library(tsibble)
library(forecast)
library(MTS)
library(syuzhet)
library(vars)
library(lubridate)
library(fpp2)

########################################## Import Data

#Import sentiment lexicons

bing_sentiment<-get_sentiments('bing')
afinn_sentiment <- get_sentiments('afinn')


bing_sentiment
#Write in csv files to join them

temp = list.files("./StateTweets/",pattern="*.csv")

######################################### Combine the files into a data frame

TotCandidates = do.call(rbind, lapply(paste0("./StateTweets/",temp), function(x) read.csv(x, stringsAsFactors = FALSE)))



####################################################################
########################################## Sentiment Analysis #######
#####################################################################
#remove negators from stop words
stop_words1 <- stop_words %>%
  filter(!word == "not"&!word == "no"&!word == "never"&!word == "without")

stop_words1

view(bing_sentiment)

#create list of negators
negation_words <- c("not", "no", "never", "without")



############ bing lexicon

TweetsDF <- TotCandidates %>%
  filter(!str_detect(screen_name, 'Biden|biden|Warren|warren|Sanders|sanders'))%>% #filter out usernames
  rowid_to_column("tweetID")%>% #Create primary key
  group_by(text)%>% #Delete Retweets
  arrange(desc(retweet_count))%>%
  filter(row_number()==1)%>%
  mutate(stripped_text = gsub("http\\S+","",text))%>% #strip text
  unnest_tokens(bigram,stripped_text,token = "ngrams",n=2)%>% #split into bigrams
  separate(bigram, c("word1", "word2"), sep = " ")%>%#separate bigrams into columns
  filter(!word1 %in% stop_words1$word) %>% #filter out stop words
  filter(!word2 %in% stop_words1$word) %>%
  inner_join(bing_sentiment, by = c(word2 = "word"))%>% #join with bing sentiment
  mutate(numSentiment = ifelse(sentiment == "positive" ,1,-1))%>% #score words in each teet
  mutate(adjSentiment = ifelse(word1 %in% negation_words,-1*numSentiment,1*numSentiment)) #reverse score of words following negators





TweetsDF1 <- TweetsDF%>%
  mutate(date = as.Date(created_at,format = "%Y-%m-%d"))%>%
  group_by(tweetID)%>% #group by each tweet
  count(date,candidate,sentiment,retweet_count,favorite_count)%>% #total sentiment score for each tweet
  spread(sentiment,n,fill = 0)%>%
  mutate(sentiment = (positive-negative))%>%
  mutate(sentimentRaw = (ifelse(sentiment>0,1,ifelse(sentiment<0,-1,0))))%>% #normalize scores
  mutate(sentimentScaled = sentimentRaw*(retweet_count+favorite_count))%>% #scale by retweet and favorite count
  ungroup()%>%
  group_by(candidate,date)%>% #aggregate scores by candidate, date)
  mutate(sentiment1 = sum(sentimentScaled))%>%
  filter(row_number()==1)%>%
  dplyr::select(date,candidate,sentiment1)%>%
  mutate(sentiment = sentiment1)%>%
  ungroup()

################### AFINN lexicon

TweetsDF <- TotCandidates %>%
  filter(state == "South Carolina")%>%
  filter(!str_detect(screen_name, 'Biden|biden|Warren|warren|Sanders|sanders'))%>% #filter out usernames
  rowid_to_column("tweetID")%>% #Create primary key
  group_by(text)%>% #Delete Retweets
  arrange(desc(retweet_count))%>%
  filter(row_number()==1)%>%
  mutate(stripped_text = gsub("http\\S+","",text))%>% #strip text
  unnest_tokens(bigram,stripped_text,token = "ngrams",n=2)%>% #split into bigrams
  separate(bigram, c("word1", "word2"), sep = " ")%>%#separate bigrams into columns
  filter(!word1 %in% stop_words1$word) %>%
  filter(!word2 %in% stop_words1$word) %>%
  inner_join(afinn_sentiment, by = c(word2 = "word"))%>%
  mutate(adjSentiment = ifelse(word1 %in% negation_words,-1*value,1*value))



TweetsDF1 <- TweetsDF%>%
  mutate(date = as.Date(created_at,format = "%Y-%m-%d"))%>% #fix date
  group_by(tweetID)%>% #we want only unique tweets
  mutate(score = sum(value))%>% #find score of each tweet
  replace_na(list(score = 0))%>% #replace NAs with 0
  mutate(sentimentRaw = (ifelse(score>0,1,ifelse(score<0,-1,0))))%>%
  mutate(sentimentScaled = sentimentRaw*(retweet_count+favorite_count))%>%
  ungroup()%>% #scale sentiment down
  dplyr::select(date,candidate,tweetID,text,sentimentScaled)%>% #select columns
  group_by(candidate,date)%>% #calculate sentiment for each candidate per day
  mutate(n = length(c(tweetID)))%>%
  mutate(sentiment = (sum(sentimentScaled))/n)%>%
  filter(row_number()==1)%>%
  ungroup()  



#Built in smoother for sentiments by candidate

TweetsDF1 %>%
  ggplot(aes(x = date, y = sentiment, color = candidate)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Democratic Primary: Daily Sentiment", x = "") +
  facet_wrap(~ candidate, ncol = 2, scale = "free_y") +
  expand_limits(y = 0) + 
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")
# ylim(-200,150) 
#  ylab("sentiment")
#  scale_x_date(breaks = as.Date(c("2019-11-06","2019-11-30", "2019-12-30")),date_labels = "%b-%d") 



#######################################################################
########  Scrape Polling Data ########################################
#####################################################################



PrimaryData <- read.csv("president_primary_polls.csv",header = T)

#Wrangle Polling Data From 538
PrimaryData = PrimaryData%>%
  dplyr::select(question_id,cycle,state,pollster,office_type,end_date,stage,party,answer,pct,notes)%>%
  filter(cycle == 2020)%>%
  filter(state == "South Carolina")%>%
  filter(office_type == "U.S. President")%>%
  filter(stage == "primary")%>%
  filter(party == "DEM")%>%
  filter(notes != "head-to-head poll")

#Make sure it is a date type
PrimaryData$end_date = as.Date(PrimaryData$end_date,format = "%m/%d/%Y")

#Filter for top four candidates, with adjustable end date
PrimaryData1<-PrimaryData%>%
  filter(end_date>="0019-11-05")%>%
  filter(answer == "Biden" | answer == "Warren" | answer =="Sanders" | answer == "Buttigieg")

#fix dates
PrimaryData2<-PrimaryData1%>%
  mutate(date = as.Date(end_date))

PrimaryData2$date = gsub("00","20",PrimaryData2$date)

PrimaryData2$date = as.Date(PrimaryData2$date)

#select columns we want, calculate mean pct per day
PrimaryData3 <- PrimaryData2%>%
  dplyr::select(date,pollster,answer,pct)%>%
  arrange(date)%>%
  group_by(date,answer)%>%
  mutate(percent = mean(pct))%>%
  filter(row_number() == 1)%>%
  ungroup()%>%
  dplyr::select(date,answer,percent)


#Built in smoother for polling by candidate
PrimaryData3 %>%
  ggplot(aes(x = date, y = percent, color = answer)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Democratic Primary: Daily Polling", x = "") +
  facet_wrap(~ answer, ncol = 2, scale = "free_y") +
  expand_limits(y = 0) + 
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")+
  ylim(0,40) +
  ylab("percent")

#fix date one last time
PrimaryData3$date = as.Date(PrimaryData3$date)

##################################################################################################
##################################################  Data Frames by Sentiment/Polling and Candidate
#################################################################################################

####################################################### Biden

# Convert Sentiment and Polling to Time Series Objects, and Fill/Pad them (Biden)
#create sentiment df for biden
sentDF_Bi <- TweetsDF1%>%
  filter(candidate == "Biden")%>%
  dplyr::select(date,sentiment)%>%
  complete(date = seq(min(date),max(date),by = "1 day")) %>% #fill blank dates with the previous day
  fill(sentiment)%>%
  as_tibble(index = date)%>%
  filter(date <= as.Date("2020-02-27"))%>%
  filter(date >= as.Date("2019-11-13"))


#do the same for polling
pollDF_Bi <- PrimaryData3%>%
  filter(answer == "Biden")%>%
  dplyr::select(date,percent)%>%
  complete(date = seq(min(date),max(date),by = "1 day")) %>%
  fill(percent)%>%
  as_tibble(index = date)%>%
  filter(date <= as.Date("2020-02-27"))





#Create and plot Time Series Objects (Biden)

sentimentTS_Bi <- xts(x = sentDF_Bi$sentiment, order.by = (seq(as.Date("2019-11-13"),length = 107, by ="days")))
pollTS_Bi <- xts(x = pollDF_Bi$percent, order.by = (seq(as.Date("2019-11-13"),length = 107, by ="days")))

#Create time series objects


pollTS <- ts(pollTS_Bi)
sentTS <- ts(sentimentTS_Bi)

plot(sentimentTS_Bi)
plot(pollTS_Bi)

#################################################################### Warren

# Convert Sentiment and Polling to Time Series Objects, and Fill/Pad them 
sentDF_W <- TweetsDF1%>%
  filter(candidate == "Warren")%>%
  dplyr::select(date,sentiment)%>%
  complete(date = seq(min(date),max(date),by = "1 day")) %>%
  fill(sentiment)%>%
  as_tibble(index = date)%>%
  filter(date <= as.Date("2020-02-27"))%>%
  filter(date >= as.Date("2019-11-13"))


pollDF_W <- PrimaryData3%>%
  filter(answer == "Warren")%>%
  dplyr::select(date,percent)%>%
  complete(date = seq(min(date),max(date),by = "1 day")) %>%
  fill(percent)%>%
  as_tibble(index = date)%>%
  filter(date <= as.Date("2020-02-27"))%>%
  filter(date >= as.Date("2019-11-13"))





#Create and plot Time Series Objects 

sentimentTS_W <- xts(x = sentDF_W$sentiment, order.by = (seq(as.Date("2019-11-13"),length = 107, by ="days")))
pollTS_W <- xts(x = pollDF_W$percent, order.by = (seq(as.Date("2019-11-13"),length = 107, by ="days")))


#create time series objects

pollTS1 <- ts(pollTS_W)
sentTS1<- ts(sentimentTS_W)

plot(sentimentTS_W)
plot(pollTS_W)

#################################################################### Sanders

# Convert Sentiment and Polling to Time Series Objects, and Fill/Pad them 
sentDF_S <- TweetsDF1%>%
  filter(candidate == "Sanders")%>%
  dplyr::select(date,sentiment)%>%
  complete(date = seq(min(date),max(date),by = "1 day")) %>%
  fill(sentiment)%>%
  as_tibble(index = date)%>%
  filter(date <= as.Date("2020-02-27"))%>%
  filter(date >= as.Date("2019-11-13"))


pollDF_S <- PrimaryData3%>%
  filter(answer == "Sanders")%>%
  dplyr::select(date,percent)%>%
  complete(date = seq(min(date),max(date),by = "1 day")) %>%
  fill(percent)%>%
  as_tibble(index = date)%>%
  filter(date <= as.Date("2020-02-27"))%>%
  filter(date >= as.Date("2019-11-13"))




#Create and plot Time Series Objects 

sentimentTS_S <- xts(x = sentDF_S$sentiment, order.by = (seq(as.Date("2019-11-13"),length = 107, by ="days")))
pollTS_S <- xts(x = pollDF_S$percent, order.by = (seq(as.Date("2019-11-13"),length = 107, by ="days")))

#create time series objects

pollTS2 <- ts(pollTS_S)
sentTS2<- ts(sentimentTS_S)



plot(sentimentTS_S)
plot(pollTS_S)
#################################################################### Buttigieg

# Convert Sentiment and Polling to Time Series Objects, and Fill/Pad them 
sentDF_Bu <- TweetsDF1%>%
  filter(candidate == "Buttigieg")%>%
  dplyr::select(date,sentiment)%>%
  complete(date = seq(min(date),max(date),by = "1 day")) %>%
  fill(sentiment)%>%
  as_tibble(index = date)%>%
  filter(date <= as.Date("2020-02-27"))%>%
  filter(date >= as.Date("2019-11-13"))


pollDF_Bu <- PrimaryData3%>%
  filter(answer == "Buttigieg")%>%
  dplyr::select(date,percent)%>%
  complete(date = seq(min(date),max(date),by = "1 day")) %>%
  fill(percent)%>%
  as_tibble(index = date)%>%
  filter(date <= as.Date("2020-02-27"))%>%
  filter(date >= as.Date("2019-11-13"))





#Create and plot Time Series Objects 

sentimentTS_Bu <- xts(x = sentDF_Bu$sentiment, order.by = (seq(as.Date("2019-11-13"),length = 107, by ="days")))
pollTS_Bu <- xts(x = pollDF_Bu$percent, order.by = (seq(as.Date("2019-11-13"),length = 107, by ="days")))

pollTS3 <- ts(pollTS_Bu)
sentTS3<- ts(sentimentTS_Bu)

plot(sentimentTS_Bu)
plot(pollTS_Bu)

##############################################################################################################
#############################################################################################################
###############################  Models ###############################################################
#############################################################################################################

#Create multivariate time series of sentiment and polling values


sentTS_Total<-cbind(sentTS,sentTS1,sentTS2,sentTS3)
pollTS_Total<-cbind(pollTS,pollTS1,pollTS2,pollTS3)
totTS_Total <- cbind(sentTS_Total,pollTS_Total)
bidenTS_Poll <- pollTS
warrenTS_Poll <- pollTS1
sandersTS_Poll <- pollTS2
buttigiegTS_Poll <- pollTS3
bidenTS_PollSent <- cbind(sentTS,pollTS)
warrenTS_PollSent <- cbind(sentTS1,pollTS1)
sandersTS_PollSent <- cbind(sentTS2,pollTS2)
buttigiegTS_PollSent <- cbind(sentTS3,pollTS3)

############################ACF plots

acf(pollTS, main = "Biden Polling")
acf(sentTS, main = "Biden Sentiment")

pacf(pollTS, main = "Biden Polling")
pacf(sentTS, main = "Biden Sentiment")



#Format time series into appropriate objects, smooth time series by week

Tot_TS <- ts(data = totTS_Total,start = as.Date("2019-11-13"),end = as.Date("2020-02-27"))  
Tot_zoo <- zoo(Tot_TS, seq(from = as.Date("2019-11-13"), to = as.Date("2020-02-27"), by = 1))
smooth_Tot <- rollmean(Tot_zoo,7,align = "right")
ndiffs(smooth_Tot)
diffTot <- diff(smooth_Tot,1)

Poll_TS <- ts(data = pollTS_Total,start = as.Date("2019-11-13"),end = as.Date("2020-02-27"))
Poll_zoo <- zoo(Poll_TS, seq(from = as.Date("2019-11-13"), to = as.Date("2020-02-27"), by = 1))
smooth_Poll <- rollmean(Poll_zoo,7,align = "right")
ndiffs(smooth_Poll)
diffPoll <- diff(smooth_Poll,1)

Biden_TS <- ts(data = bidenTS_Poll,start = as.Date("2019-11-13"),end = as.Date("2020-02-27"))
Biden_zoo <- zoo(Biden_TS, seq(from = as.Date("2019-11-13"), to = as.Date("2020-02-27"), by = 1))
smooth_Biden <- rollmean(Biden_zoo,7,align = "right")
ndiffs(smooth_Biden)
diffBidenP <- diff(smooth_Biden,1)
plot(diffBidenP,main = "Biden Polling",ylab = "Differenced Percent",xlab = "Time")

BidenS_TS <- ts(data = sentTS,start = as.Date("2019-11-13"),end = as.Date("2020-02-27"))
BidenS_zoo <- zoo(BidenS_TS, seq(from = as.Date("2019-11-13"), to = as.Date("2020-02-27"), by = 1))
smoothS_Biden <- rollmean(BidenS_zoo,7,align = "right")
ndiffs(smoothS_Biden)
diffBidenS <- diff(smoothS_Biden,1)
plot(diffBidenS,main = "Biden Sentiment",ylab = "Differenced Sentiment",xlab = "Time")


plot(Biden_zoo,main = "Biden Polling",ylab = "Percent",xlab = "Time")
plot(smooth_Biden,main = "Biden Polling",ylab = "Percent",xlab = "Time")
plot(BidenS_zoo,main = "Biden Sentiment",ylab = "Sentiment",xlab = "Time")
plot(smoothS_Biden,main = "Biden Sentiment",ylab = "Sentiment",xlab = "Time")


Warren_TS <- ts(data = warrenTS_Poll,start = as.Date("2019-11-13"),end = as.Date("2020-02-27"))
Warren_zoo <- zoo(Warren_TS, seq(from = as.Date("2019-11-13"), to = as.Date("2020-02-27"), by = 1))
smooth_Warren <- rollmean(Warren_zoo,7,align = "right")
ndiffs(smooth_Warren)
diffWarrenP <- diff(smooth_Warren,1)
plot(diffWarrenP,main = "Warren Polling",ylab = "Differenced Percent",xlab = "Time")

WarrenS_TS <- ts(data = sentTS1,start = as.Date("2019-11-13"),end = as.Date("2020-02-27"))
WarrenS_zoo <- zoo(WarrenS_TS, seq(from = as.Date("2019-11-13"), to = as.Date("2020-02-27"), by = 1))
smoothS_Warren <- rollmean(WarrenS_zoo,7,align = "right")
ndiffs(smoothS_Warren)
diffWarrenS <- diff(smoothS_Warren,1)
plot(diffWarrenS,main = "Warren Sentiment",ylab = "Sentiment",xlab = "Time")

Sanders_TS <- ts(data = sandersTS_Poll,start = as.Date("2019-11-13"),end = as.Date("2020-02-27"))
Sanders_zoo <- zoo(Sanders_TS, seq(from = as.Date("2019-11-13"), to = as.Date("2020-02-27"), by = 1))
smooth_Sanders <- rollmean(Sanders_zoo,7,align = "right")
ndiffs(smooth_Sanders)
diffSandersP <- diff(smooth_Sanders,1)
plot(diffSandersP,main = "Sanders Polling",ylab = "Differenced Percent",xlab = "Time")

SandersS_TS <- ts(data = sentTS2,start = as.Date("2019-11-13"),end = as.Date("2020-02-27"))
SandersS_zoo <- zoo(SandersS_TS, seq(from = as.Date("2019-11-13"), to = as.Date("2020-02-27"), by = 1))
smoothS_Sanders <- rollmean(SandersS_zoo,7,align = "right")
ndiffs(smoothS_Sanders)
diffSandersS <- diff(smoothS_Sanders,1)
plot(diffSandersS,main = "Sanders Sentiment",ylab = "Differenced Sentiment",xlab = "Time")

Buttigieg_TS <- ts(data = buttigiegTS_Poll,start = as.Date("2019-11-13"),end = as.Date("2020-02-27"))
Buttigieg_zoo <- zoo(Buttigieg_TS,seq(from = as.Date("2019-11-13"), to = as.Date("2020-02-27"), by = 1))
smooth_Buttigieg <- rollmean(Buttigieg_zoo,7,align = "right")
ndiffs(smooth_Buttigieg)
diffButtigiegP <- diff(smooth_Buttigieg,1)
plot(diffButtigiegP,main = "Buttigieg Polling",ylab = "Differenced Percent",xlab = "Time")

ButtigiegS_TS <- ts(data = sentTS3,start = as.Date("2019-11-13"),end = as.Date("2020-02-27"))
ButtigiegS_zoo <- zoo(ButtigiegS_TS,seq(from = as.Date("2019-11-13"), to = as.Date("2020-02-27"), by = 1))
smoothS_Buttigieg <- rollmean(ButtigiegS_zoo,7,align = "right")
ndiffs(smoothS_Buttigieg)
diffButtigiegS <- diff(smoothS_Buttigieg,1)
plot(diffButtigiegS,main = "Buttigieg Sentiment",ylab = "Differenced Sentiment",xlab = "Time")


Biden1_TS <- ts(data = bidenTS_PollSent,start = as.Date("2019-11-13"),end = as.Date("2020-02-27"))
Biden1_zoo <- zoo(Biden1_TS,seq(from = as.Date("2019-11-13"), to = as.Date("2020-02-27"), by = 1))
smooth_Biden1 <- rollmean(Biden1_zoo,7,align = "right")
ndiffs(smooth_Biden1)
diffBiden1 <- diff(smooth_Biden1,1)

Warren1_TS <- ts(data = warrenTS_PollSent,start = as.Date("2019-11-13"),end = as.Date("2020-02-27"))
Warren1_zoo <- zoo(Warren1_TS,seq(from = as.Date("2019-11-13"), to = as.Date("2020-02-27"), by = 1))
smooth_Warren1 <- rollmean(Warren1_zoo,7,align = "right")
ndiffs(smooth_Warren1)
diffWarren1 <- diff(smooth_Warren1,1)

Sanders1_TS <- ts(data = sandersTS_PollSent,start = as.Date("2019-11-13"),end = as.Date("2020-02-27"))
Sanders1_zoo <- zoo(Sanders1_TS,seq(from = as.Date("2019-11-13"), to = as.Date("2020-02-27"), by = 1))
smooth_Sanders1 <- rollmean(Sanders1_zoo,7,align = "right")
ndiffs(smooth_Sanders1)
diffSanders1 <- diff(smooth_Sanders1)

Buttigieg1_TS <- ts(data = buttigiegTS_PollSent,start = as.Date("2019-11-13"),end = as.Date("2020-02-27"))
Buttigieg1_zoo <- zoo(Buttigieg1_TS,seq(from = as.Date("2019-11-13"), to = as.Date("2020-02-27"), by = 1))
smooth_Buttigieg1 <- rollmean(Buttigieg1_zoo,7,align = "right")
ndiffs(smooth_Buttigieg1)
diffButtigieg1 <- diff(smooth_Buttigieg1)

########################################################### Evaluate Models

##########Correlation
ccf(as.numeric(diffBidenS),as.numeric(diffBidenP),lag.max = 5,main = "Biden CCF",ylab = "Correlation",xlab = "Sentiment Lag (Days)")
ccf(as.numeric(diffWarrenS),as.numeric(diffWarrenP),lag.max = 5,main = "Warren CCF",ylab = "Correlation",xlab = "Sentiment Lag (Days)")
ccf(as.numeric(diffSandersS),as.numeric(diffSandersP),lag.max = 5,main = "Sanders CCF",ylab = "Correlation",xlab = "Sentiment Lag (Days)")
ccf(as.numeric(diffButtigiegS),as.numeric(diffButtigiegP),lag.max = 5,main = "Buttigieg CCF",ylab = "Correlation",xlab = "Sentiment Lag (Days)")


##############Granger Causality
grangertest(diffBidenS,diffBidenP,order = 5)
grangertest(diffBidenP,diffBidenS,order = 5)

grangertest(diffWarrenS,diffWarrenP,order = 5)
grangertest(diffWarrenP,diffWarrenS,order = 5)

grangertest(diffSandersS,diffSandersP,order = 5)
grangertest(diffSandersP,diffSandersS,order = 5)

grangertest(diffButtigiegS,diffButtigiegP,order = 5)
grangertest(diffButtigiegP,diffButtigiegS,order = 5)




###################    Models
Bidenactual<- 48.7
Warrenactual<-7.1
Sandersactual<-19.8
Buttigiegactual<-8.2

Total_VAR <- VAR(smooth_Tot,type = "both")
Bidenpred<-predict(Total_VAR,n.ahead = 2)$fcst$pollTS_Total.pollTS[2,][1]
Warrenpred<-predict(Total_VAR,n.ahead = 2)$fcst$pollTS_Total.pollTS1[2,][1]
Sanderspred<-predict(Total_VAR,n.ahead = 2)$fcst$pollTS_Total.pollTS2[2,][1]
Buttigiegpred<-predict(Total_VAR,n.ahead = 2)$fcst$pollTS_Total.pollTS3[2,][1]

Biden_VAR <- VAR(smooth_Biden1,type = "both")
Bidenpred1<-predict(Biden_VAR,n.ahead = 2)$fcst$pollTS[2,][1]
Warren_VAR <- VAR(smooth_Warren1,type = "both")
Warrenpred1<-predict(Warren_VAR,n.ahead = 2)$fcst$pollTS1[2,][1]
Sanders_VAR <- VAR(smooth_Sanders1,type = "both")
Sanderspred1<-predict(Sanders_VAR,n.ahead = 2)$fcst$pollTS2[2,][1]
Buttigieg_VAR <- VAR(smooth_Buttigieg1,type = "both")
Buttigiegpred1<-predict(Buttigieg_VAR,n.ahead = 2)$fcst$pollTS3[2,][1]

Poll_VAR <- VAR(smooth_Poll,type = "both")
Bidenpred2<-predict(Poll_VAR,n.ahead = 2)$fcst$pollTS[2,][1]
Warrenpred2<-predict(Poll_VAR,n.ahead = 2)$fcst$pollTS1[2,][1]
Sanderspred2<-predict(Poll_VAR,n.ahead = 2)$fcst$pollTS2[2,][1]
Buttigiegpred2<-predict(Poll_VAR,n.ahead = 2)$fcst$pollTS3[2,][1]

Biden_ARIMA <- auto.arima(smooth_Biden)
Bidenpred3<-forecast(Biden_ARIMA,2)[4]$mean[2]
Warren_ARIMA <- auto.arima(smooth_Warren)
Warrenpred3<-forecast(Warren_ARIMA,2)[4]$mean[2]
Sanders_ARIMA <- auto.arima(smooth_Sanders)
Sanderspred3<-forecast(Sanders_ARIMA,2)[4]$mean[2]
Buttigieg_ARIMA <- auto.arima(smooth_Buttigieg)
Buttigiegpred3<-forecast(Buttigieg_ARIMA,2)[4]$mean[2]

Total_VAR_error<- (abs(Bidenactual-Bidenpred)^2) + (abs(Warrenactual-Warrenpred)^2) +(abs(Sandersactual-Sanderspred)^2) +(abs(Buttigiegactual-Buttigiegpred)^2) 
Separate_VAR_error <- (abs(Bidenactual-Bidenpred1)^2) + (abs(Warrenactual-Warrenpred1)^2) +(abs(Sandersactual-Sanderspred1)^2) +(abs(Buttigiegactual-Buttigiegpred1)^2)
Total_Poll_error <- (abs(Bidenactual-Bidenpred2)^2) + (abs(Warrenactual-Warrenpred2)^2) +(abs(Sandersactual-Sanderspred2)^2) +(abs(Buttigiegactual-Buttigiegpred2)^2)
Separate_ARIMA_error <- (abs(Bidenactual-Bidenpred3)^2) + (abs(Warrenactual-Warrenpred3)^2) +(abs(Sandersactual-Sanderspred3)^2) +(abs(Buttigiegactual-Buttigiegpred3)^2)
Total_VAR_error
Separate_VAR_error
Total_Poll_error
Separate_ARIMA_error

full_results <- rbind(
  as.numeric(Total_VAR_error),
  as.numeric(Total_Poll_error),
  as.numeric(Separate_VAR_error),
  as.numeric(Separate_ARIMA_error))



rownames(full_results)<- c('Total Sentiment-Poll VAR','Total Poll VAR','Separate Sentiment_Poll VARs','Separate Poll ARIMAs')


colnames(full_results)<-c('SSE')

plot(predict(Poll_VAR,n.ahead = 2),main="Total Poll VAR Model: Biden 2-day Forecast",xlim=c(80,107),ylim=c(0,45),xlab="Feb",xaxt="n")

