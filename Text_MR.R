################ MF(Matrix Factorization) 

# make sure your text file location align with your R file location
setwd("/Users/watanabetakato/desktop/大学/RFiles")
cname <- file.path(".", "", "")
length(dir(cname))

dir(cname)

#Convert text files into corpus
docs <- Corpus(DirSource(cname))
docs

#Prepare term document matrix
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/|@|\\|")

inspect(docs[3])

docs <- tm_map(docs, content_transformer(tolower))

docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, removePunctuation)

docs <- tm_map(docs, removeWords, stopwords("english"))

docs <- tm_map(docs, stripWhitespace)

# docs <- tm_map(docs, stemDocument)

dtm <- DocumentTermMatrix(docs)
dtm
tdm <- TermDocumentMatrix(docs)
tdm

# save the tdm matrix
m <- as.matrix(tdm)
dim(m)

#write into csv file
write.csv(m, file="/Users/watanabetakato/desktop/大学/RFiles/tdm.csv")
m <- read.csv(file="/Users/watanabetakato/desktop/大学/RFiles/tdm.csv")
head(m)

rownames(m) <- m[,1]
m[,1] <- NULL

#NMF operation
res <- nmf(m, 1,"KL") 

w <- basis(res) #  W  user feature matrix matrix
dim(w)
print(w)

#Test NMF on Real WebPages
df <- as.data.frame(w)
head(df,10)

df$total <- rowSums(df)
df$word<-rownames(df)
colnames(df) <- c("doc1","total","word")
df <-df[order(-df$total),] 
head(df,20)

wordMatrix = as.data.frame(w)
wordMatrix$word<-rownames(wordMatrix)
colnames(wordMatrix) <- c("doc1","word")


# Topic 1
newdata <-wordMatrix[order(-wordMatrix$doc1),] 
head(newdata)

d <- newdata
df <- as.data.frame(cbind(d[1:10,]$word,as.numeric(d[1:10,]$doc1)))
colnames(df)<- c("Word","Frequency")

# for ggplot to understand the order of words, you need to specify factor order

df$Word <- factor(df$Word, levels = df$Word[order(df$Frequency)])
ggplot(df, aes(x=Word, y=Frequency)) + 
  geom_bar(stat="identity", fill="lightgreen", color="grey50")+
  coord_flip()+
  ggtitle("Topic 1")

