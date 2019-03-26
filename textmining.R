library(tm)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(cluster)
library(SnowballC)
cname <- file.path("C:", "texts")   
cname   
dir(cname)
docs <- VCorpus(DirSource(cname))   
summary(docs) 
inspect(docs[1])
writeLines(as.character(docs[1]))
docs <- tm_map(docs,removePunctuation)
for (j in seq(docs))  {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("\u2028", " ", docs[[j]]) } 
docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, PlainTextDocument)
DocsCopy <- docs
docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, removeWords, c("syllogism", "tautology")) 
for (j in seq(docs))
{
  docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
  docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
  docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])
}
docs <- tm_map(docs, PlainTextDocument)
docs_st <- tm_map(docs, stemDocument)   
docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1]))
docs <- docs_st
docs
docs_stc <- tm_map(docs_st, stemCompletion, dictionary = DocsCopy, lazy = TRUE)
docs_stc <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs_stc[1]))
docs <- docs_stc
docs
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)
dtm <- DocumentTermMatrix(docs)   
dtm  
inspect(dtm[1:5, 1:20])
tdm <- TermDocumentMatrix(docs)   
tdm 
freq <- colSums(as.matrix(dtm))   
length(freq)  
ord <- order(freq) 
ord
m <- as.matrix(dtm)   
dim(m) 
write.csv(m, file="DocumentTermMatrix.csv")
dtms <- removeSparseTerms(dtm, 0.2)
dtms
freq <- colSums(as.matrix(dtm))
freq
head(table(freq), 20)
tail(table(freq), 20)
freq <- colSums(as.matrix(dtms))   
freq 
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14) 
findFreqTerms(dtm, lowfreq=50)
wf <- data.frame(word=names(freq), freq=freq)   
head(wf) 
p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p
findAssocs(dtm, c("country" , "american"), corlimit=0.85)
findAssocs(dtms, "think", corlimit=0.70)

set.seed(142)   
wordcloud(names(freq), freq, min.freq=25)
set.seed(142)   
wordcloud(names(freq), freq, max.words=100)
set.seed(142)   
wordcloud(names(freq), freq, min.freq=20, scale=c(5, .1), colors=brewer.pal(6, "Dark2")) 
dtmss <- removeSparseTerms(dtm, 0.15)  
dtmss
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="complete") 
fit  
plot(fit, hang=-1) 