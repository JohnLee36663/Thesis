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


#Write in csv files to join them

temp = list.files("./NationalTweets/",pattern="*.csv")



######################################### Combine the files into a data frame

TotCandidates = do.call(rbind, lapply(paste0("./NationalTweets/",temp), function(x) read.csv(x, stringsAsFactors = FALSE)))



###############################################################################################
########################################## Sentiment Analysis and Data Cleaning #######
##############################################################################################

#filter negators from stopwords
stop_words1 <- stop_words %>%
  filter(!word == "not"&!word == "no"&!word == "never"&!word == "without")

#Create negators list
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
  mutate(numSentiment = ifelse(sentiment == "positive" ,1,-1))%>% #score words in each tweet
  mutate(adjSentiment = ifelse(word1 %in% negation_words,-1*numSentiment,1*numSentiment)) #reverse scores based on negation
  
  



TweetsDF1 <- TweetsDF%>%
  mutate(date = as.Date(created_at,format = "%Y-%m-%d"))%>% #change date type
  group_by(tweetID)%>% #group by each tweet
  count(date,candidate,sentiment,retweet_count,favorite_count)%>% #score each tweet by subtracting negative words from positive words
  spread(sentiment,n,fill = 0)%>%
  mutate(sentiment = (positive-negative))%>%
  mutate(sentimentRaw = (ifelse(sentiment>0,1,ifelse(sentiment<0,-1,0))))%>%
#  mutate(sentimentScaled = sentimentRaw*(retweet_count+favorite_count))%>% #weigh scores by retweets and favorites
  ungroup()%>%
  group_by(candidate,date)%>% #aggregate scores by candidate and date
  mutate(sentiment1 = sum(sentimentRaw))%>%
  filter(row_number()==1)%>%
  dplyr::select(date,candidate,sentiment1)%>%
  mutate(sentiment = sentiment1)%>%
  ungroup()

################### AFINN lexicon

TweetsDF <- TotCandidates %>%
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
########  Scrape and Clean Polling Data #############################
#####################################################################



PrimaryData <- read.csv("president_primary_polls.csv",header = T)

#Wrangle Polling Data From 538
PrimaryData = PrimaryData%>%
  dplyr::select(question_id,cycle,state,pollster,office_type,end_date,stage,party,answer,pct,notes)%>%
  filter(cycle == 2020)%>%
  filter(state == "")%>%
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
  filter(date <= as.Date("2020-02-27"))
  

#do the same for polling
pollDF_Bi <- PrimaryData3%>%
  filter(answer == "Biden")%>%
  dplyr::select(date,percent)%>%
  complete(date = seq(min(date),max(date),by = "1 day")) %>%
  fill(percent)%>%
  as_tibble(index = date)%>%
  filter(date >= as.Date("2019-11-06"))
  




#Create and plot Time Series Objects (Biden)

sentimentTS_Bi <- xts(x = sentDF_Bi$sentiment, order.by = (seq(as.Date("2019-11-06"),length = 114, by ="days")))
pollTS_Bi <- xts(x = pollDF_Bi$percent, order.by = (seq(as.Date("2019-11-06"),length = 114, by ="days")))

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
  filter(date <= as.Date("2020-02-27"))
  

pollDF_W <- PrimaryData3%>%
  filter(answer == "Warren")%>%
  dplyr::select(date,percent)%>%
  complete(date = seq(min(date),max(date),by = "1 day")) %>%
  fill(percent)%>%
  as_tibble(index = date)%>%
  filter(date >= as.Date("2019-11-06"))
 




#Create and plot Time Series Objects 

sentimentTS_W <- xts(x = sentDF_W$sentiment, order.by = (seq(as.Date("2019-11-06"),length = 114, by ="days")))
pollTS_W <- xts(x = pollDF_W$percent, order.by = (seq(as.Date("2019-11-06"),length = 114, by ="days")))


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
  filter(date <= as.Date("2020-02-27"))
  

pollDF_S <- PrimaryData3%>%
  filter(answer == "Sanders")%>%
  dplyr::select(date,percent)%>%
  complete(date = seq(min(date),max(date),by = "1 day")) %>%
  fill(percent)%>%
  as_tibble(index = date)%>%
  filter(date >= as.Date("2019-11-06"))




#Create and plot Time Series Objects 

sentimentTS_S <- xts(x = sentDF_S$sentiment, order.by = (seq(as.Date("2019-11-06"),length = 114, by ="days")))
pollTS_S <- xts(x = pollDF_S$percent, order.by = (seq(as.Date("2019-11-06"),length = 114, by ="days")))

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
  filter(date <= as.Date("2020-02-27"))
  

pollDF_Bu <- PrimaryData3%>%
  filter(answer == "Buttigieg")%>%
  dplyr::select(date,percent)%>%
  complete(date = seq(min(date),max(date),by = "1 day")) %>%
  fill(percent)%>%
  as_tibble(index = date)%>%
  filter(date >= as.Date("2019-11-06"))
  




#Create and plot Time Series Objects 

sentimentTS_Bu <- xts(x = sentDF_Bu$sentiment, order.by = (seq(as.Date("2019-11-06"),length = 114, by ="days")))
pollTS_Bu <- xts(x = pollDF_Bu$percent, order.by = (seq(as.Date("2019-11-06"),length = 114, by ="days")))

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

Tot_TS <- ts(data = totTS_Total,start = as.Date("2019-11-06"),end = as.Date("2020-02-27"))  
Tot_zoo <- zoo(Tot_TS, seq(from = as.Date("2019-11-06"), to = as.Date("2020-02-27"), by = 1))
smooth_Tot <- rollmean(Tot_zoo,7,align = "right")
ndiffs(smooth_Tot)
diffTot <- diff(smooth_Tot,1)

Poll_TS <- ts(data = pollTS_Total,start = as.Date("2019-11-06"),end = as.Date("2020-02-27"))
Poll_zoo <- zoo(Poll_TS, seq(from = as.Date("2019-11-06"), to = as.Date("2020-02-27"), by = 1))
smooth_Poll <- rollmean(Poll_zoo,7,align = "right")
ndiffs(smooth_Poll)
diffPoll <- diff(smooth_Poll,1)

Biden_TS <- ts(data = bidenTS_Poll,start = as.Date("2019-11-06"),end = as.Date("2020-02-27"))
Biden_zoo <- zoo(Biden_TS, seq(from = as.Date("2019-11-06"), to = as.Date("2020-02-27"), by = 1))
smooth_Biden <- rollmean(Biden_zoo,7,align = "right")
ndiffs(smooth_Biden)
diffBidenP <- diff(smooth_Biden,1)
plot(diffBidenP,main = "Biden Polling",ylab = "Differenced Percent",xlab = "Time")

BidenS_TS <- ts(data = sentTS,start = as.Date("2019-11-06"),end = as.Date("2020-02-27"))
BidenS_zoo <- zoo(BidenS_TS, seq(from = as.Date("2019-11-06"), to = as.Date("2020-02-27"), by = 1))
smoothS_Biden <- rollmean(BidenS_zoo,7,align = "right")
ndiffs(smoothS_Biden)
diffBidenS <- diff(smoothS_Biden,1)
plot(diffBidenS,main = "Biden Sentiment",ylab = "Differenced Sentiment",xlab = "Time")


plot(Biden_zoo,main = "Biden Polling",ylab = "Percent",xlab = "Time")
plot(smooth_Biden,main = "Biden Polling",ylab = "Percent",xlab = "Time")
plot(BidenS_zoo,main = "Biden Sentiment",ylab = "Sentiment",xlab = "Time")
plot(smoothS_Biden,main = "Biden Sentiment",ylab = "Sentiment",xlab = "Time")


Warren_TS <- ts(data = warrenTS_Poll,start = as.Date("2019-11-06"),end = as.Date("2020-02-27"))
Warren_zoo <- zoo(Warren_TS, seq(from = as.Date("2019-11-06"), to = as.Date("2020-02-27"), by = 1))
smooth_Warren <- rollmean(Warren_zoo,7,align = "right")
ndiffs(smooth_Warren)
diffWarrenP <- diff(smooth_Warren,1)
plot(diffWarrenP,main = "Warren Polling",ylab = "Differenced Percent",xlab = "Time")

WarrenS_TS <- ts(data = sentTS1,start = as.Date("2019-11-06"),end = as.Date("2020-02-27"))
WarrenS_zoo <- zoo(WarrenS_TS, seq(from = as.Date("2019-11-06"), to = as.Date("2020-02-27"), by = 1))
smoothS_Warren <- rollmean(WarrenS_zoo,7,align = "right")
ndiffs(smoothS_Warren)
diffWarrenS <- diff(smoothS_Warren,1)
plot(diffWarrenS,main = "Warren Sentiment",ylab = "Sentiment",xlab = "Time")

Sanders_TS <- ts(data = sandersTS_Poll,start = as.Date("2019-11-06"),end = as.Date("2020-02-27"))
Sanders_zoo <- zoo(Sanders_TS, seq(from = as.Date("2019-11-06"), to = as.Date("2020-02-27"), by = 1))
smooth_Sanders <- rollmean(Sanders_zoo,7,align = "right")
ndiffs(smooth_Sanders)
diffSandersP <- diff(smooth_Sanders,1)
plot(diffSandersP,main = "Sanders Polling",ylab = "Differenced Percent",xlab = "Time")

SandersS_TS <- ts(data = sentTS2,start = as.Date("2019-11-06"),end = as.Date("2020-02-27"))
SandersS_zoo <- zoo(SandersS_TS, seq(from = as.Date("2019-11-06"), to = as.Date("2020-02-27"), by = 1))
smoothS_Sanders <- rollmean(SandersS_zoo,7,align = "right")
ndiffs(smoothS_Sanders)
diffSandersS <- diff(smoothS_Sanders,1)
plot(diffSandersS,main = "Sanders Sentiment",ylab = "Differenced Sentiment",xlab = "Time")

Buttigieg_TS <- ts(data = buttigiegTS_Poll,start = as.Date("2019-11-06"),end = as.Date("2020-02-27"))
Buttigieg_zoo <- zoo(Buttigieg_TS,seq(from = as.Date("2019-11-06"), to = as.Date("2020-02-27"), by = 1))
smooth_Buttigieg <- rollmean(Buttigieg_zoo,7,align = "right")
ndiffs(smooth_Buttigieg)
diffButtigiegP <- diff(smooth_Buttigieg,1)
plot(diffButtigiegP,main = "Buttigieg Polling",ylab = "Differenced Percent",xlab = "Time")

ButtigiegS_TS <- ts(data = sentTS3,start = as.Date("2019-11-06"),end = as.Date("2020-02-27"))
ButtigiegS_zoo <- zoo(ButtigiegS_TS,seq(from = as.Date("2019-11-06"), to = as.Date("2020-02-27"), by = 1))
smoothS_Buttigieg <- rollmean(ButtigiegS_zoo,7,align = "right")
ndiffs(smoothS_Buttigieg)
diffButtigiegS <- diff(smoothS_Buttigieg,1)
plot(diffButtigiegS,main = "Buttigieg Sentiment",ylab = "Differenced Sentiment",xlab = "Time")


Biden1_TS <- ts(data = bidenTS_PollSent,start = as.Date("2019-11-06"),end = as.Date("2020-02-27"))
Biden1_zoo <- zoo(Biden1_TS,seq(from = as.Date("2019-11-06"), to = as.Date("2020-02-27"), by = 1))
smooth_Biden1 <- rollmean(Biden1_zoo,7,align = "right")
ndiffs(smooth_Biden1)
diffBiden1 <- diff(smooth_Biden1,1)

Warren1_TS <- ts(data = warrenTS_PollSent,start = as.Date("2019-11-06"),end = as.Date("2020-02-27"))
Warren1_zoo <- zoo(Warren1_TS,seq(from = as.Date("2019-11-06"), to = as.Date("2020-02-27"), by = 1))
smooth_Warren1 <- rollmean(Warren1_zoo,7,align = "right")
ndiffs(smooth_Warren1)
diffWarren1 <- diff(smooth_Warren1,1)

Sanders1_TS <- ts(data = sandersTS_PollSent,start = as.Date("2019-11-06"),end = as.Date("2020-02-27"))
Sanders1_zoo <- zoo(Sanders1_TS,seq(from = as.Date("2019-11-06"), to = as.Date("2020-02-27"), by = 1))
smooth_Sanders1 <- rollmean(Sanders1_zoo,7,align = "right")
ndiffs(smooth_Sanders1)
diffSanders1 <- diff(smooth_Sanders1)

Buttigieg1_TS <- ts(data = buttigiegTS_PollSent,start = as.Date("2019-11-06"),end = as.Date("2020-02-27"))
Buttigieg1_zoo <- zoo(Buttigieg1_TS,seq(from = as.Date("2019-11-06"), to = as.Date("2020-02-27"), by = 1))
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
#################################### Total VAR (8 time series)



#Select Model Order

VARselect(diffTot,lag.max = 5)

summary(VAR(smooth_Tot,type = "both"))

#Cross validate and average test set evaluation metrics

#Initialize rolling training and test data, as well as metrics 
trainlist = list()
testlist = list()
evaluationdf = data.frame()

#Cross Validation 
for(i in 0:10)
{
  #Set rolling windows
  trainlist[[i+1]]<- window(smooth_Tot, end=as.Date("2019-12-12") + (7*i))
  testlist[[i+1]] <- window(Tot_zoo, start=as.Date("2019-12-13")+(7*i),end=as.Date("2019-12-19")+(7*i))
  
  
  tr <- trainlist[[i+1]]
  te <- testlist[[i+1]]
  #Fit model
  mod <- VAR(tr,type = "both")
  fits <- fitted(mod)
  preds <- predict(mod,n.ahead=7)
  
  #Evaluate predictions for each time series
  for(j in 1:8)
  {
    fc <- structure(list(mean=preds$fcst[[j]][,"fcst"], x=tr[,j],
                         fitted=c(NA,NA,fits[,j])),class="forecast")
    print(forecast::accuracy(fc,te[,j]))
    evaluationdf <- rbind(evaluationdf,forecast::accuracy(fc,te[,j]))
    
  }
  
  
  
}








#Filter out sentiment evaluations
evaluationdfTotal<- rbind(
evaluationdf[9:16,],
evaluationdf[25:32,],
evaluationdf[41:48,],
evaluationdf[57:64,],
evaluationdf[73:80,],
evaluationdf[89:96,],
evaluationdf[105:112,],
evaluationdf[121:128,],
evaluationdf[137:144,],
evaluationdf[153:160,],
evaluationdf[169:176,])

#evaluationdf[185:192,],
#evaluationdf[201:208,],
#evaluationdf[217:224,],
#evaluationdf[233:240,],
#evaluationdf[249:256,],
#evaluationdf[265:272,],
#evaluationdf[281:288,],
#evaluationdf[297:304,],
#evaluationdf[313:320,],
#evaluationdf[329:336,],
#evaluationdf[345:352,],
#evaluationdf[361:368,],
#evaluationdf[377:384,],
#evaluationdf[393:400,],
#evaluationdf[409:416,],


#evaluationdf[409:416,],
#evaluationdf[425:432,],
#evaluationdf[441:448,],
#evaluationdf[457:464,],
#evaluationdf[473:480,],
#evaluationdf[489:496,],
#evaluationdf[505:512,],
#evaluationdf[521:528,],
#evaluationdf[537:544,],
#evaluationdf[553:560,],
#evaluationdf[569:576,],
#evaluationdf[585:592,],
#evaluationdf[601:608,],
#evaluationdf[617:624,])

#evaluationdf[633:640,],
#evaluationdf[649:656,],
#evaluationdf[665:672,],
#evaluationdf[681:688,],
#evaluationdf[697:704,],
#evaluationdf[713:720,],
#evaluationdf[729:736,],
#evaluationdf[745:752,],
#evaluationdf[761:768,],
#evaluationdf[777:784,],
#evaluationdf[793:800,],
#evaluationdf[809:816,],
#evaluationdf[825:832,],
#evaluationdf[841:848,],
#evaluationdf[857:864,],
#evaluationdf[873:880,],
#evaluationdf[889:896,],
#evaluationdf[905:912,],
#evaluationdf[921:928,],
#evaluationdf[937:944,],
#evaluationdf[953:960,],
#evaluationdf[969:976,],
#evaluationdf[985:992,],
#evaluationdf[1001:1008,],
#evaluationdf[1017:1024,],
#evaluationdf[1033:1040,],
#evaluationdf[1049:1056,],
#evaluationdf[1065:1072,],
#evaluationdf[1081:1088,],
#evaluationdf[1095:1102,],
#evaluationdf[1111:1118,],
#evaluationdf[1127:1134,],
#evaluationdf[1143:1150,],
#evaluationdf[1159:1166,],
#evaluationdf[1175:1182,],
#evaluationdf[1191:1198,],
#evaluationdf[1207:1214,],
#evaluationdf[1223:1230,])





#Fill na values and filter for only test sets. Then calculate metrics
evaluationdfTotal2 <-
  na.aggregate(evaluationdfTotal)%>%
  #slice(193:200)%>%
  filter(row_number() %% 2 == 0)%>%
  summarise(mnME = mean(ME),mnRMSE = mean(RMSE),mnMAE = mean(MAE),mnMPE = mean(MPE),mnMAPE = mean(MAPE),mnMASE = mean(MASE))


evaluationdfTotal2



########################################### Polling VAR (4 time series)

#select model order
VARselect(smooth_Poll,lag.max = 5)

summary(VAR(smooth_Poll,2,type = "both"))

#Initialize rolling training and test data, as well as metrics 
trainlistP = list()
testlistP = list()
evaluationdfP = data.frame()

#Cross Validation
for(i in 0:10)
{
  #Set rolling windows
  trainlistP[[i+1]]<- window(smooth_Poll, end=as.Date("2019-12-12") + (7*i))
  testlistP[[i+1]] <- window(Poll_zoo, start=as.Date("2019-12-13")+(7*i),end=as.Date("2019-12-19")+(7*i))
  
  #Fit model
  trP <- trainlistP[[i+1]]
  teP <- testlistP[[i+1]]
  modP <- VAR(trP,1,type = "both")
  fitsP <- fitted(modP)
  predsP <- predict(modP,n.ahead=7)
  for(j in 1:4)
  {
    #Evaluate predictions for each time series
    fcP <- structure(list(mean=predsP$fcst[[j]][,"fcst"], x=trP[,j],
                         fitted=c(NA,NA,fitsP[,j])),class="forecast")
    print(forecast::accuracy(fcP,teP[,j]))
    evaluationdfP <- rbind(evaluationdfP,forecast::accuracy(fcP,teP[,j]))
    
  }
  
  
  
}




#Fill NA values, filter for only test sets, calculate metrics
evaluationdfTotalP1 <- na.aggregate(evaluationdfP)%>%
  #slice(193:200)%>%
  filter(row_number() %% 2 == 0)%>%
  summarise(mnME = mean(ME),mnRMSE = mean(RMSE),mnMAE = mean(MAE),mnMPE = mean(MPE),mnMAPE = mean(MAPE),mnMASE = mean(MASE))



evaluationdfTotalP1
######################################## Polling Sentiment VARs (2 time series)

#########################################Biden



####### Select Model Order
VARselect(diffBiden1,lag.max = 5)
VARselect(smooth_Biden1,lag.max = 5)
########### Investigate Causality
modBiden <- VAR(diffBiden1,2)
causality(modBiden)

summary(VAR(smooth_Biden1,2,type = "both"))
#Initialize rolling training and test data, as well as metrics 
trainlistB1 = list()
testlistB1 = list()
evaluationdfB1 = data.frame()

#Cross Validation
for(i in 0:10)
{
  #Set rolling windows
  trainlistB1[[i+1]]<- window(smooth_Biden1, end=as.Date("2019-12-12") + (7*i))
  testlistB1[[i+1]] <- window(Biden1_zoo, start=as.Date("2019-12-13")+(7*i),end=as.Date("2019-12-19")+(7*i))
  
  #Fit model
  trB1 <- trainlistB1[[i+1]]
  teB1 <- testlistB1[[i+1]]
  modB1 <- VAR(trB1,1,type = "both")
  fitsB1 <- fitted(modB1)
  predsB1 <- predict(modB1,n.ahead=7)
  for(j in 1:2)
  {
    #Evaluate predictions for each time series
    fcB1 <- structure(list(mean=predsB1$fcst[[j]][,"fcst"], x=trB1[,j],
                          fitted=c(NA,NA,fitsB1[,j])),class="forecast")
    print(forecast::accuracy(fcB1,teB1[,j]))
    evaluationdfB1 <- rbind(evaluationdfB1,forecast::accuracy(fcB1,teB1[,j]))
    
  }
  
  
  
}




#Fill NA values, filter for only test sets, calculate metrics
evaluationdfTotalB1 <- na.aggregate(evaluationdfB1)%>%
  #slice(193:200)%>%
  filter(row_number() %% 2 == 0)%>%
  filter(row_number() %% 2 == 0)%>%
  summarise(mnME = mean(ME),mnRMSE = mean(RMSE),mnMAE = mean(MAE),mnMPE = mean(MPE),mnMAPE = mean(MAPE),mnMASE = mean(MASE))



evaluationdfTotalB1


#########################################Warren



########Select Model Order
VARselect(diffWarren1,lag.max = 5)
modWarren <- VAR(smooth_Warren1,2)

########### Investigate Causality

causality(modWarren)

#Initialize rolling training and test data, as well as metrics 
trainlistW1 = list()
testlistW1 = list()
evaluationdfW1 = data.frame()

#Cross Validation
for(i in 0:10)
{
  #Set rolling windows
  trainlistW1[[i+1]]<- window(smooth_Warren1, end=as.Date("2019-12-12") + (7*i))
  testlistW1[[i+1]] <- window(Warren1_zoo, start=as.Date("2019-12-13")+(7*i),end=as.Date("2019-12-19")+(7*i))
  
  #Fit model
  trW1 <- trainlistW1[[i+1]]
  teW1 <- testlistW1[[i+1]]
  modW1 <- VAR(trW1,type = "both")
  fitsW1 <- fitted(modW1)
  predsW1 <- predict(modW1,n.ahead=7)
  for(j in 1:2)
  {
    #Evaluate predictions for each time series
    fcW1 <- structure(list(mean=predsW1$fcst[[j]][,"fcst"], x=trW1[,j],
                           fitted=c(NA,NA,fitsW1[,j])),class="forecast")
    print(forecast::accuracy(fcW1,teW1[,j]))
    evaluationdfW1 <- rbind(evaluationdfW1,forecast::accuracy(fcW1,teW1[,j]))
    
  }
  
  
  
}




#Fill NA values, filter for only test sets, calculate metrics
evaluationdfTotalW1 <- na.aggregate(evaluationdfW1)%>%
  #slice(193:200)%>%
  filter(row_number() %% 2 == 0)%>%
  filter(row_number() %% 2 == 0)%>%
  summarise(mnME = mean(ME),mnRMSE = mean(RMSE),mnMAE = mean(MAE),mnMPE = mean(MPE),mnMAPE = mean(MAPE),mnMASE = mean(MASE))



evaluationdfTotalW1


######################################### Sanders


#Select Model Order
VARselect(diffSanders1,lag.max = 5)

########### Investigate Causality
modSanders <- VAR(smooth_Sanders1,2)
causality(modSanders)


#Initialize rolling training and test data, as well as metrics 
trainlistS1 = list()
testlistS1 = list()
evaluationdfS1 = data.frame()

#Cross Validation
for(i in 0:10)
{
  #Set rolling windows
  trainlistS1[[i+1]]<- window(smooth_Sanders1, end=as.Date("2019-12-12") + (7*i))
  testlistS1[[i+1]] <- window(Sanders1_zoo, start=as.Date("2019-12-13")+(7*i),end=as.Date("2019-12-19")+(7*i))
  
  #Fit model
  trS1 <- trainlistS1[[i+1]]
  teS1 <- testlistS1[[i+1]]
  modS1 <- VAR(trS1,type = "both")
  fitsS1 <- fitted(modS1)
  predsS1 <- predict(modS1,n.ahead=7)
  for(j in 1:2)
  {
    #Evaluate predictions for each time series
    fcS1 <- structure(list(mean=predsS1$fcst[[j]][,"fcst"], x=trS1[,j],
                           fitted=c(NA,NA,fitsS1[,j])),class="forecast")
    print(forecast::accuracy(fcS1,teS1[,j]))
    evaluationdfS1 <- rbind(evaluationdfS1,forecast::accuracy(fcS1,teS1[,j]))
    
  }
  
  
  
}




#Fill NA values, filter for only test sets, calculate metrics
evaluationdfTotalS1 <- na.aggregate(evaluationdfS1)%>%
  #slice(193:200)%>%
  filter(row_number() %% 2 == 0)%>%
  filter(row_number() %% 2 == 0)%>%
  summarise(mnME = mean(ME),mnRMSE = mean(RMSE),mnMAE = mean(MAE),mnMPE = mean(MPE),mnMAPE = mean(MAPE),mnMASE = mean(MASE))



evaluationdfTotalS1



######################################### Buttigieg



#Select Model Order
VARselect(diffButtigieg1,lag.max = 5)
modButtigieg <- VAR(smooth_Buttigieg1,3)
summary(modButtigieg)

plot(predict(modButtigieg,3))
########### Investigate Causality

causality(modButtigieg)

#Initialize rolling training and test data, as well as metrics 
trainlistBu1 = list()
testlistBu1 = list()
evaluationdfBu1 = data.frame()

#Cross Validation
for(i in 0:10)
{
  #Set rolling windows
  trainlistBu1[[i+1]]<- window(smooth_Buttigieg1, end=as.Date("2019-12-12") + (7*i))
  testlistBu1[[i+1]] <- window(Buttigieg1_zoo, start=as.Date("2019-12-13")+(7*i),end=as.Date("2019-12-19")+(7*i))
  
  #Fit model
  trBu1 <- trainlistBu1[[i+1]]
  teBu1 <- testlistBu1[[i+1]]
  modBu1 <- VAR(trBu1,type = "both")
  fitsBu1 <- fitted(modBu1)
  predsBu1 <- predict(modBu1,n.ahead=7)
  for(j in 1:2)
  {
    #Evaluate predictions for each time series
    fcBu1 <- structure(list(mean=predsBu1$fcst[[j]][,"fcst"], x=trBu1[,j],
                           fitted=c(NA,NA,fitsBu1[,j])),class="forecast")
    print(forecast::accuracy(fcBu1,teBu1[,j]))
    evaluationdfBu1 <- rbind(evaluationdfBu1,forecast::accuracy(fcBu1,teBu1[,j]))
    
  }
  
  
  
}




#Fill NA values, filter for only test sets, calculate metrics
evaluationdfTotalBu1 <- na.aggregate(evaluationdfBu1)%>%
  #slice(193:200)%>%
  filter(row_number() %% 2 == 0)%>%
  filter(row_number() %% 2 == 0)%>%
  summarise(mnME = mean(ME),mnRMSE = mean(RMSE),mnMAE = mean(MAE),mnMPE = mean(MPE),mnMAPE = mean(MAPE),mnMASE = mean(MASE))



evaluationdfTotalBu1

#################################Combine individual VARs
evaluationdfVARSep <- 
  rbind(evaluationdfTotalB1,evaluationdfTotalW1,evaluationdfTotalS1,evaluationdfTotalBu1)%>%
  summarise(mnME = mean(mnME),mnRMSE = mean(mnRMSE),mnMAE = mean(mnMAE),mnMPE = mean(mnMPE),mnMAPE = mean(mnMAPE),mnMASE = mean(mnMASE))

evaluationdfVARSep
#######################################  Polling ARIMA (1 time series)


############ Biden

#Check assumptions
Box.test(diffBidenP)

auto.arima(diffBidenP)


#Initialize rolling training and test data, as well as metrics 
trainlistB = list()
testlistB = list()
evaluationdfB = data.frame()

#Cross Validation
for(i in 0:10)
{
  trainlistB[[i+1]]<- window(smooth_Biden, end=as.Date("2019-12-12") + (7*i))
  testlistB[[i+1]] <- window(Biden_zoo, start=as.Date("2019-12-13")+(7*i),end=as.Date("2019-12-19")+(7*i))
  
  trB <- trainlistB[[i+1]]
  teB <- testlistB[[i+1]]
  modB <- auto.arima(trB)
  
  predsB <- forecast(modB,3)
  
  
  print(accuracy(predsB,teB))
  evaluationdfB <- rbind(evaluationdfB,accuracy(predsB,teB))
  
}


evaluationdfB <- rbind(evaluationdfB,accuracy(predsB,teB))

#Evaluate accuracy of test sets
evaluationdfB1 <- evaluationdfB%>%
#  slice(47:50)%>%
  filter(row_number() %% 2 == 0)%>%
  summarise(mnME = mean(ME),mnRMSE = mean(RMSE),mnMAE = mean(MAE),mnMPE = mean(MPE),mnMAPE = mean(MAPE),mnMASE = mean(MASE))


evaluationdfB1

######### Warren

#Check Assumptions 
Box.test(diffWarrenP)

auto.arima(diffWarrenP)
#Initialize rolling training and test data, as well as metrics 
trainlistW = list()
testlistW = list()
evaluationdfW = data.frame()

#Cross Validation
for(i in 0:10)
{
  trainlistW[[i+1]]<- window(smooth_Warren, end=as.Date("2019-12-12") + (7*i))
  testlistW[[i+1]] <- window(Warren_zoo, start=as.Date("2019-12-13")+(7*i),end=as.Date("2019-12-19")+(7*i))
  
  trW <- trainlistW[[i+1]]
  teW <- testlistW[[i+1]]
  modW <- auto.arima(trW)
  
  predsW <- forecast(modW,7)
  
  
  print(accuracy(predsW,teW))
  evaluationdfW <- rbind(evaluationdfW,accuracy(predsW,teW))
  
}

#Evaluate accuracy of test sets
evaluationdfW1 <- evaluationdfW%>%
#  slice(47:50)%>%
  filter(row_number() %% 2 == 0)%>%
  summarise(mnME = mean(ME),mnRMSE = mean(RMSE),mnMAE = mean(MAE),mnMPE = mean(MPE),mnMAPE = mean(MAPE),mnMASE = mean(MASE))
  

evaluationdfW1

############ Sanders

#Check Assumptions
Box.test(diffSandersP)

auto.arima(smooth_Sanders)
#Initialize rolling training and test data, as well as metrics 
trainlistS = list()
testlistS = list()
evaluationdfS = data.frame()

#Cross Validation
for(i in 0:10)
{
  trainlistS[[i+1]]<- window(smooth_Sanders, end=as.Date("2019-12-12") + (7*i))
  testlistS[[i+1]] <- window(Sanders_zoo, start=as.Date("2019-12-13")+(7*i),end=as.Date("2019-12-19")+(7*i))
  
  trS <- trainlistS[[i+1]]
  teS <- testlistS[[i+1]]
  modS <- auto.arima(trS)
  
  predsS <- forecast(modS,7)
  
  
  print(accuracy(predsS,teS))
  evaluationdfS <- rbind(evaluationdfS,accuracy(predsS,teS))
  
}

#Evaluate accuracy of test sets
evaluationdfS1 <- evaluationdfS%>%
#  slice(47:50)%>%
  filter(row_number() %% 2 == 0)%>%
  summarise(mnME = mean(ME),mnRMSE = mean(RMSE),mnMAE = mean(MAE),mnMPE = mean(MPE),mnMAPE = mean(MAPE),mnMASE = mean(MASE))

evaluationdfS1
############### Buttigieg

#Check Assumptions
Box.test(diffButtigiegP)
ndiffs(smooth_Buttigieg)
auto.arima(smooth_Buttigieg)
#Initialize rolling training and test data, as well as metrics 
trainlistBu = list()
testlistBu = list()
evaluationdfBu = data.frame()

#Cross Validation
for(i in 0:10)
{
  trainlistBu[[i+1]]<- window(smooth_Buttigieg, end=as.Date("2019-12-12") + (7*i))
  testlistBu[[i+1]] <- window(Buttigieg_zoo, start=as.Date("2019-12-13")+(7*i),end=as.Date("2019-12-19")+(7*i))
  
  trBu <- trainlistBu[[i+1]]
  teBu <- testlistBu[[i+1]]
  modBu <- auto.arima(trBu)
  
  predsBu <- forecast(modBu,7)
  
  
  print(accuracy(predsBu,teBu))
  evaluationdfBu <- rbind(evaluationdfBu,accuracy(predsBu,teBu))
  
}
evaluationdfBu

#Evaluate accuracy of test sets
evaluationdfBu1 <- evaluationdfBu %>%
#  slice(47:50)%>%
  filter(row_number() %% 2 == 0)%>%
  summarise(mnME = mean(ME),mnRMSE = mean(RMSE),mnMAE = mean(MAE),mnMPE = mean(MPE),mnMAPE = mean(MAPE),mnMASE = mean(MASE))



evaluationdfBu1

######################################Combine individual ARIMA results

evaluationdfSep <- 
  rbind(evaluationdfB1,evaluationdfW1,evaluationdfS1,evaluationdfBu1)%>%
  summarise(mnME = mean(mnME),mnRMSE = mean(mnRMSE),mnMAE = mean(mnMAE),mnMPE = mean(mnMPE),mnMAPE = mean(mnMAPE),mnMASE = mean(mnMASE))

evaluationdfSep

#####################################################
##########################Compare models
#####################################################
full_results <- rbind(
  evaluationdfTotal2,
  evaluationdfTotalP1,
  evaluationdfVARSep,
  evaluationdfSep)



rownames(full_results)<- c('Total Sentiment-Poll VAR','Total Poll VAR','Separate Sentiment_Poll VARs','Separate Poll ARIMAs')


MAE_result <- full_results[,3,drop=FALSE]
colnames(MAE_result)<-c('MAE')
