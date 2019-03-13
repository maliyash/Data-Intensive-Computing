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

rt <- search_tweets("#fluvaccine OR #Influenza", geocode = lookup_coords("usa"), n = 15000)


rt <- lat_lng(rt)


data4 = read.csv("TwitterData7.csv",header=TRUE)
data4[data4[,12] == FALSE,]
data4 = data4[!is.na(data4$lat) & !is.na(data4$lng),]
data4 = subset(data4, data4$country == "United States")

testPoints <- data.frame(x = c(data4$lng),y=c(data4$lat))
testPoints$x = as.numeric(testPoints$x)
testPoints$y = as.numeric(testPoints$y)

revgeo_df<-revgeo(longitude=testPoints$x,latitude=testPoints$y,output='frame')
revgeo_df<- subset(revgeo_df, revgeo_df$country == "United States of America")
states<-data.frame(revgeo_df$state)
tweetfreq<-as.data.frame(table(states))


allstate <- data.frame(state = tolower(rownames(USArrests)))
tweetfreq<-data.frame(state = tolower(tweetfreq$state), tweetfreq$Freq) 
final_freq<-merge(x = allstate, y = tweetfreq, by = "state", all.x = TRUE)
final_freq[is.na(final_freq)] <- 0


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



