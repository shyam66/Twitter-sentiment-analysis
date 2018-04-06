library(tm)
install.packages("devtools")
library(devtools)
library(httr)
install_github("twitteR", username = "geoffjentry/twitteR")
library(twitteR)
install.packages("wordcloud")
library(wordcloud)
install.packages("syuzhet")
library(syuzhet)
twitter<-function()
{
api_key<- "zxxZEUEpIl9RFlgeH7rnwNvVN"
api_secret<-"IFNvTRY44emhnAMey2YTOAPgqtjlzdTTlIDnihjRu5Vndzi6yL"
access_key<-"140651032-rqJeLOyritVL0ooock8WhGCp1LstjuVoluLfOU0S"
access_key_secret<-"bXNqJeAFGEK8BeTD8N0tl6hLxgD7qmvnqR3oRTSHjdhiM"
s_keyword<-readline(prompt = "Enter the keyword you want to analyise with an # :")
num<-as.numeric(readline(prompt = "enter the number of tweets you want to analyse :"))
setup_twitter_oauth(api_key, api_secret, access_key, access_key_secret)
tweets <- searchTwitter(s_keyword, n=num) 
tweets.df <- do.call(rbind, lapply(tweets, as.data.frame))
View(tweets.df)
write.csv(tweets.df,paste("D:/r/",s_keyword,".csv",sep = ))
summary(tweets.df)
tweets.df<-tolower(tweets.df)
tweets.df<-gsub("rt","",tweets.df)
tweets.df <- gsub("@\\w+", "", tweets.df)
tweets.df<-gsub("[[:punct:]]","",tweets.df)
tweets.df<- gsub("http\\w+", "", tweets.df)
tweets.df <- gsub("[ |\t]{2,}", "", tweets.df)
tweets.df<-gsub("^ ","",tweets.df)
tweets.df <- gsub(" $", "", tweets.df)
tweets.df<-gsub("false","",tweets.df)
tweets.df<-gsub("true","",tweets.df)
tweets.df<-gsub("href","",tweets.df)
# to remove stop words install text mining package
tweets.df.corpus <- Corpus(VectorSource(tweets.df))
tweets.df.corpus <- tm_map(tweets.df.corpus,
                           function(x)removeWords(x,stopwords()))

png(paste("wordcloud",s_keyword,".png",sep = ""), width=1280,height=800)
wordcloud(tweets.df.corpus,min.freq = 3, 
          scale=c(7,0.5),colors=brewer.pal(8, "Dark2"), 
          random.color= TRUE, random.order = FALSE, max.words = 150)
dev.off()

ts<-get_nrc_sentiment(tweets.df)
ts
td<-data.frame(t(ts))
td1<-as.matrix(rowSums(td))
return(td1)
}
# trying 

td_new <- data.frame(rowSums(td))
colnames(td1)<-"count of emotions"
td1
twitter()
woeid=availableTrendLocations(29229014,"woeid")
t<-getTrends(woeid=1)#period="daily",date=Sys.Date())
class(t)
View(t)
t1<-data.frame()
t1<-t[,c(1,2)]
class(t1)
View(t1)
install.packages("ROAuth")
library(ROAuth)
d<-DocumentTermMatrix(tweets.df.corpus)
d1<-as.matrix(d)
View(d3)
d2=sort(rowSums(d1),decreasing = TRUE)
d3<-data.frame(word=names(d2),freq=d2)
?getTrends
