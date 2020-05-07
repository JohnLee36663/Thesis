library(rtweet)
library(tidyverse)




#pull tweets from states with early primaries

#Loop method, create function that stores each name 


rename<-function(x){
  if (x<10){
    return(name<-paste('000',i,'result',sep=''))
  }
  if(x<100&&i>=10){
    return(name<-paste('0',i,'result',sep=''))
  }
  if(x>=100){
    return(name<-paste('0',i,'result',sep=''))
  }
}


######################################## Location: Iowa and South Carolina

state.name2 = c("Iowa","South Carolina") #Create vector of important, early primary states


#Pull 600 tweets for each candidate

#Joe Biden

n<-2 #number of states
for(i in 1:n){ #loop
  name1<-rename(i) #naming function
  print(state.name2[i]) #keep track of which state we are on in the loop
  assign(name1,search_tweets(q = "Biden OR Joe Biden OR @JoeBiden OR #JoeBiden", n = 1500,geocode = lookup_coords(state.name2[i]),retryonratelimit = F))
  #assign a name and pull 100 tweets from all states and D.C
}

Biden_Tweets1 = list(`0001result`,`0002result`)

Biden_Tweet_List1 = list() #create empty list
for(i in 1:2){ #for each state 
  Biden_Tweet_List1[[i]]=Biden_Tweets1[[i]]%>% #store in new list
    select(user_id,screen_name,text,created_at,favorite_count,retweet_count)%>% #select only the columns we want
    mutate(state = state.name2[[i]]) #assign state name column
}

Biden.data.frame1 <- do.call(rbind,Biden_Tweet_List1) #bind candidate list into data frame

#Elizabeth Warren

n<-2 #number of states
for(i in 1:n){ #loop
  name<-rename(i) #naming function
  print(state.name2[i]) #keep track of which state we are on in the loop
  assign(name,search_tweets(q = "Warren OR Elizabeth Warren OR @ewarren OR #ElizabethWarren", n = 1500,geocode = lookup_coords(state.name2[i]),retryonratelimit = F))
  #assign a name and pull 100 tweets from all states and D.C
}

Warren_Tweets1 = list(`0001result`,`0002result`)

Warren_Tweet_List1 = list() #create empty list
for(i in 1:2){ #for each state 
  Warren_Tweet_List1[[i]]=Warren_Tweets1[[i]]%>% #store in new list
    select(user_id,screen_name,text,created_at,favorite_count,retweet_count)%>% #select only the columns we want
    mutate(state = state.name2[[i]]) #assign state name column
}

Warren.data.frame1 <- do.call(rbind,Warren_Tweet_List1) 

#Pete Buttigieg

n<-2 #number of states
for(i in 1:n){ #loop
  name<-rename(i) #naming function
  print(state.name2[i]) #keep track of which state we are on in the loop
  assign(name,search_tweets(q = "Buttigieg OR Pete Buttigieg OR @PeteButtigieg OR #PeteButtigieg", n = 1500,geocode = lookup_coords(state.name2[i]),retryonratelimit = F))
  #assign a name and pull 100 tweets from all states and D.C
}

Pete_Tweets1 = list(`0001result`,`0002result`)

Pete_Tweet_List1 = list() #create empty list
for(i in 1:2){ #for each state 
  Pete_Tweet_List1[[i]]=Pete_Tweets1[[i]]%>% #store in new list
    select(user_id,screen_name,text,created_at,favorite_count,retweet_count)%>% #select only the columns we want
    mutate(state = state.name2[[i]]) #assign state name column
}

Pete.data.frame1 <- do.call(rbind,Pete_Tweet_List1) 

#Bernie Sanders

n<-2 #number of states
for(i in 1:n){ #loop
  name<-rename(i) #naming function
  print(state.name2[i]) #keep track of which state we are on in the loop
  assign(name,search_tweets(q = "Sanders OR Bernie Sanders OR @SenSanders OR #BernieSanders", n = 1500,geocode = lookup_coords(state.name2[i]),retryonratelimit = F))
  #assign a name and pull 100 tweets from all states and D.C
}

Sanders_Tweets1 = list(`0001result`,`0002result`)

Sanders_Tweet_List1 = list() #create empty list
for(i in 1:2){ #for each state 
  Sanders_Tweet_List1[[i]]=Sanders_Tweets1[[i]]%>% #store in new list
    select(user_id,screen_name,text,created_at,favorite_count,retweet_count)%>% #select only the columns we want
    mutate(state = state.name2[[i]]) #assign state name column
}

Sanders.data.frame1 <- do.call(rbind,Sanders_Tweet_List1) #bind list into big data frame

#Add candidate column to each data frame

Biden.data.frame1 = Biden.data.frame1%>%
  mutate(candidate = "Biden")

Warren.data.frame1 = Warren.data.frame1%>%
  mutate(candidate = "Warren")

Sanders.data.frame1 = Sanders.data.frame1 %>%
  mutate(candidate = "Sanders")

Pete.data.frame1 = Pete.data.frame1%>%
  mutate(candidate = "Buttigieg")

#Bind all the data frames into one big candidate data frame

CandidateTweets1 <- rbind(Biden.data.frame1,Warren.data.frame1,Sanders.data.frame1,Pete.data.frame1)

#Store the data in a csv file

write.csv(CandidateTweets1,"StateLevelTweets105.csv")

################################## Location: United States

#Pull 600 tweets for each candidate

#Joe Biden

n<-1 #number of states
for(i in 1:n){ #loop
  name2<-rename(i) #naming function
  assign(name2,search_tweets(q = "Biden OR Joe Biden OR @JoeBiden OR #JoeBiden", n = 1500,geocode = lookup_coords("United States"),retryonratelimit = F))
  #assign a name and pull 100 tweets from all states and D.C
}

Biden_Tweets2 = list(`0001result`)

Biden.data.frame2 <- do.call(rbind,Biden_Tweets2) #bind candidate list into data frame

Biden.data.frame2 = Biden.data.frame2%>%
  select(user_id,screen_name,text,created_at,favorite_count,retweet_count)#select only the columns we want


#Elizabeth Warren


n<-1 #number of states
for(i in 1:n){ #loop
  name2<-rename(i) #naming function
  assign(name2,search_tweets(q = "Warren OR Elizabeth Warren OR @ewarren OR #ElizabethWarren", n = 1500,geocode = lookup_coords("United States"),retryonratelimit = F))
  #assign a name and pull 100 tweets from all states and D.C
}

Warren_Tweets2 = list(`0001result`)


Warren.data.frame2 <- do.call(rbind,Warren_Tweets2) #bind candidate list into data frame

Warren.data.frame2=Warren.data.frame2%>%
  select(user_id,screen_name,text,created_at,favorite_count,retweet_count)#select only the columns we want




#Pete Buttigieg


n<-1 #number of states
for(i in 1:n){ #loop
  name2<-rename(i) #naming function
  assign(name2,search_tweets(q = "Buttigieg OR Pete Buttigieg OR @PeteButtigieg OR #PeteButtigieg", n = 1500,geocode = lookup_coords("United States"),retryonratelimit = F))
  #assign a name and pull 100 tweets from all states and D.C
}

Pete_Tweets2 = list(`0001result`)

Pete.data.frame2 <- do.call(rbind,Pete_Tweets2) #bind candidate list into data frame

Pete.data.frame2 = Pete.data.frame2%>%
  select(user_id,screen_name,text,created_at,favorite_count,retweet_count)#select only the columns we want


#Bernie Sanders


n<-1 #number of states
for(i in 1:n){ #loop
  name2<-rename(i) #naming function
  assign(name2,search_tweets(q = "Sanders OR Bernie Sanders OR @SenSanders OR #BernieSanders", n = 1500,geocode = lookup_coords("United States"),retryonratelimit = F))
  #assign a name and pull 100 tweets from all states and D.C
}

Sanders_Tweets2 = list(`0001result`)


Sanders.data.frame2 <- do.call(rbind,Sanders_Tweets2) #bind candidate list into data frame

Sanders.data.frame2 = Sanders.data.frame2%>%
  select(user_id,screen_name,text,created_at,favorite_count,retweet_count)#select only the columns we want

  

#Add candidate column to each data frame

Biden.data.frame2 = Biden.data.frame2%>%
  mutate(candidate = "Biden")

Warren.data.frame2 = Warren.data.frame2%>%
  mutate(candidate = "Warren")

Sanders.data.frame2 = Sanders.data.frame2 %>%
  mutate(candidate = "Sanders")

Pete.data.frame2 = Pete.data.frame2%>%
  mutate(candidate = "Buttigieg")

#Bind all the data frames into one big candidate data frame

CandidateTweets2 <- rbind(Biden.data.frame2,Warren.data.frame2,Sanders.data.frame2,Pete.data.frame2)

#Store the data in a csv file



write.csv(CandidateTweets2,"NationalLevelTweets105.csv")




