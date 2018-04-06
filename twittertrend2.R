# to remove stop words install text mining package
install.packages("tm")
library("tm")
install.packages("syuzhet")
library(syuzhet)
install.packages("wordcloud")
library(wordcloud)
library(twitteR)
pac<-function(p=packages)
{
  packages<-c("twitteR","httr","tm","wordcloud","syuzhet","ggplot2")
  new.packages <- p[!(is.element( p,installed.packages()[,"Package"]))]
  
  if(length(new.packages))
  {
    install.packages(new.packages,dependencies = TRUE)
    
  }
  
pac()    
    
    sapply(p,require,character.only=TRUE)
  }

twitter<-function()
{
  api_key<- "zxxZEUEpIl9RFlgeH7rnwNvVN"
  api_secret<-"IFNvTRY44emhnAMey2YTOAPgqtjlzdTTlIDnihjRu5Vndzi6yL"
  access_key<-"140651032-rqJeLOyritVL0ooock8WhGCp1LstjuVoluLfOU0S"
  access_key_secret<-"bXNqJeAFGEK8BeTD8N0tl6hLxgD7qmvnqR3oRTSHjdhiM"
  con<-1
  while(con == 1)
   {
    
    s_keyword<-readline(prompt = "Enter the keyword you want to analyise with an # :")
    num<-as.numeric(readline(prompt = "enter the number of tweets you want to analyse :"))
    setup_twitter_oauth(api_key, api_secret, access_key, access_key_secret)
    tweets <- searchTwitter(s_keyword, n=num) 
   #class(tweets)
    #length(tweets)
     tweets.df <- do.call(rbind, lapply(tweets, as.data.frame))
     View(tweets.df)
      write.csv(tweets.df,"D:/r/tweet.csv")
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
  tweets.df.corpus <- Corpus(VectorSource(tweets.df))
  tweets.df.corpus <- tm_map(tweets.df.corpus,
                             function(x)removeWords(x,stopwords()))
  wordcloud(tweets.df.corpus,min.freq = 3, 
            scale=c(7,0.5),colors=brewer.pal(8, "Dark2"), 
            random.color= TRUE, random.order = FALSE, max.words = 150)
  ts<-get_nrc_sentiment(tweets.df)
  ts
  td<-data.frame(t(ts))
  td1<-as.matrix(rowSums(td))
  return(td1)
  con<-as.integer(readline(prompt = "do u want to search other keywords(yes/no)"))
  }
 }
twitter()
## trying 

td_new <- data.frame(rowSums(td))
colnames(td1)<-"count of emotions"
td1
twitter()
woeid=availableTrendLocations(29229014)
t<-getTrends(period="daily",date=Sys.Date())
install.packages("ROAuth")
library(ROAuth)

?getTrends

