install.packages('revgeo')
install.packages("rtweet")
install.packages("fiftystater")
library(revgeo)
library(rtweet)
library(fiftystater)

create_token(
  app = "Flu Data Analysis",
  consumer_key = "hjvZfyg326f2h2ouzQdGsMDE4",
  consumer_secret = "8uxDLaz0pOu85RfLZjzDatPQY8WEmMzmEqub5dcsbZRAV8YUr0",
  access_token = "2576684274-xGPqwmXNW46T1MRS6VIcXdrGyoaXtpTAJUl7UIm",
  access_secret = "I8dcX2D6MiBfDDmXgjrMfUbMRRrcAgR5TGCr2dPXrPDiW")

rt <- search_tweets("#fluvaccine OR #flu patients OR #fluUSA OR #flumoratality OR #Influenza OR positiveflutest OR flushot", geocode = lookup_coords("usa"), n = 15000)


rt <- lat_lng(rt)


# Reading the Twitter data stored in CSV
data4 = read.csv("TwitterData6.csv",header=TRUE)

# Eliminating retweets
data4[data4[,12] == FALSE,]

# Eliminating data with lat and long is NA 
data4 = data4[!is.na(data4$lat) & !is.na(data4$lng),]

#Extracting tweets for country as USA

data4 = subset(data4, data4$country == "United States")
testPoints <- data.frame(x = c(data4$lng),y=c(data4$lat))
testPoints$x = as.numeric(testPoints$x)
testPoints$y = as.numeric(testPoints$y)


# Revgeo to extract states from lat and longitude
revgeo_df<-revgeo(longitude=testPoints$x,latitude=testPoints$y,output='frame')
revgeo_df<- subset(revgeo_df, revgeo_df$country == "United States of America")
states<-data.frame(revgeo_df$state)
tweetfreq<-as.data.frame(table(states))


# Extractings state names from USArrests Data Set 
allstate <- data.frame(state = tolower(rownames(USArrests)))

tweetfreq<-data.frame(state = tolower(tweetfreq$state), tweetfreq$Freq) 

final_freq<-merge(x = allstate, y = tweetfreq, by = "state", all.x = TRUE)

# Converting NA values to zero
final_freq[is.na(final_freq)] <- 0

par(mfrow=c(1,2))


#Plotting HeatMap
hmap <- ggplot(final_freq, aes(map_id = state)) + 
  
  geom_map(aes(fill = final_freq$tweetfreq.Freq),color="black", size=0.2, map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "right", 
        panel.background = element_blank())+
  scale_fill_gradient2(name = " Tweet Count",low="green", mid="yellow", high="red", 
                       midpoint=(max(final_freq$tweetfreq.Freq)+min(final_freq$tweetfreq.Freq))/2, limits=range(final_freq$tweetfreq.Freq))+
  fifty_states_inset_boxes()+
  ggtitle("Heat Map from tweets")+
  theme(plot.title = element_text(size=15))

hmap

HeatMap<-read.csv("HeatMap.csv")

#changing the column name of ACTIVITY.LEVEL to level

HeatMap$Heat_level<-as.integer(gsub("[^0-9\\.]", "", HeatMap$ACTIVITY.LEVEL))


#imported this data to get latitude and longitudes of States
data("fifty_states") 



#Eliminate unnecessary columns in the csv

fluHeatmap <- data.frame(state = tolower(HeatMap$STATENAME), HeatMap$Heat_level)


# Ploting the US Map with the help of Fifty Stater Package

h1map <- ggplot(fluHeatmap, aes(map_id = state)) + 
  
  geom_map(aes(fill = HeatMap$Heat_level),color="black", size=0.2, map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "right", 
        panel.background = element_blank())+
  scale_fill_gradient2(name = " ILI Activity Level",low="green", mid="yellow", high="red", 
                       midpoint=5, limits=range(HeatMap$Heat_level))+
  fifty_states_inset_boxes()+
  ggtitle("CDC 2018-19 Influenza Season Week 5 ending Feb 02, 2019")+
  theme(plot.title = element_text(size=15))


h1map

















