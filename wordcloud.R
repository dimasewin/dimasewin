# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("readxl")

Comment   = read_excel('E://Private Documents/Bagi Data/Comment Spam.xls')
Spam      = subset(Comment,Class==1)$Comment
NotSpam   = subset(Comment,Class==0)$Comment
NewsTitle = read_excel('E://Private Documents/Bagi Data/News Title.xls')
Category  = unique(NewsTitle$Category)
News1     = subset(NewsTitle,Category==Category[1])[,'News Title']
News2     = subset(NewsTitle,Category==Category[2])[,'News Title']
News3     = subset(NewsTitle,Category==Category[3])[,'News Title']
News4     = subset(NewsTitle,Category=="Medical")[,'News Title']

write.table(News4,"E://Private Documents/Corpus.txt")

text <- readLines("E://Private Documents/Corpus.txt")
docs <- Corpus(VectorSource(text))
inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, toSpace, "ufeff")

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

Accuracy=(read.table("E://Accuracy.txt",sep="\t"))
Accuracy[,1]=str(Accuracy[,1])
Data=cbind(c(Accuracy[,1],Accuracy[,1]),
           c(Accuracy[,2],Accuracy[,3]),
           c(rep("Comment",12),rep("News",12)))
ggplot(Accuracy)+geom_line(aes(y=Accuracy$V2,x=Accuracy$V1),color="red")+
                 geom_line(aes(y=Accuracy$V3,x=Accuracy$V1),color="green")+
                 geom_line(aes(y=Accuracy$V2,x=Accuracy$V1))
                 geom_line(aes(y=Accuracy$V3,x=Accuracy$V1))