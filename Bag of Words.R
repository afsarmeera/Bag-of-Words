                         #to merge two data frame:

a<-bind_rows(a,b)

 

 

 

 

library(tm)

library(RWeka)

 

someCleanText <-a$Description

write.csv(a$Description,"p.csv")

#import the p

b<- sapply(p$x,function(row) iconv(row, "latin1", "ASCII", sub=""))

 

 

aCorpus <- VCorpus(VectorSource(b))   #With this, only 1-Grams are created

#aCorpus <- VCorpus(VectorSource(someCleanText)) #With this, biGrams are created as desired

aCorpus <- tm_map(aCorpus, stripWhitespace)

#review_corpus = tm_map(review_corpus, )

 

aCorpus<- tm_map(aCorpus, removePunctuation)

aCorpus <- tm_map(aCorpus, removeNumbers)

aCorpus <- tm_map(aCorpus, removeWords, stopwords("english"))

#w2 <- tm_map(w1, PlainTextDocument)

#content_transformer(tolower)

aCorpus <- tm_map(aCorpus, content_transformer(tolower))

 

 

 

aCorpus <- tm_map(aCorpus, stemDocument)

 

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=7))

 

aTDM <- DocumentTermMatrix(aCorpus, control=list(tokenize=BigramTokenizer))

#aTDM<- DocumentTermMatrix(tm,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE),stopwords = TRUE))

aTDM = removeSparseTerms(aTDM,0.99)

 

s<-as.matrix(aTDM)

 

 

 

 

print(aTDM$dimnames$Terms)

write.csv(s,"L.csv")

 

 

#based on sentances:

 

library(wordcloud)

v<-sort(colSums(s),decreasing=TRUE)

 

head(v,14)

words <- names(v)

y <- data.frame(word=words, freq=v)

 

write.csv(y,"y.csv")

 

#wordcloud(y$word, y$freq,min.freq = 1, rot.per=0.5, colors=brewer.pal(8, "Dark2"))

 

 

#wordcloud(y$word,y$freq)

 

 

#clolor

colorlist = c("red","blue","green","red")

basecolors = rainbow(length(unique(group)))

 

group <- c("x","y","z","x")

 

wordcloud(y$word, y$freq,min.freq = 0,random.order=FALSE,

          rot.per=0.15,

          colors=brewer.pal(8, "Dark2"))

 

 

 

 

 

#based on rows:

 

library(wordcloud)

v<-sort(rowSums(s),decreasing=TRUE)

 

head(v,14)

words <- names(v)

x <- data.frame(word=words, freq=v)

 

write.csv(x,"x.csv")

 

#wordcloud(y$word, y$freq,min.freq = 2, rot.per=0.5, colors=brewer.pal(8, "Dark2"))

 

 

#wordcloud(x$word,x$freq)

 

 

#clolor

colorlist = c("red","green","blue","red")

basecolors = colorlist(length(unique(group)))

 

group <- c("x","y","z","x")

 

#wordcloud(x$word, x$freq,colors = basecolors)

 

wordcloud(x$word, x$freq,min.freq =2, min.words=2000,  random.order=FALSE,

          rot.per=0.35,scale=c(70,.10),

          colors=brewer.pal(8, "Dark2"))

 

 

 

 

 

 

library(RTextTools)

library(topicmodels)

 

rowTotals <- apply( s, 1, sum) #Find the sum of words in each Document

dtm.new   <- s[rowTotals> 0, ] 

k <- length(unique(p$x))

lda <- LDA(dtm.new, k)