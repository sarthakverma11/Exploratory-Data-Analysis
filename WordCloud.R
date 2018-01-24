#### Word Cloud in R

# Library required

library(tm)
library(wordcloud)

# Step 1: Import the file and convert the column containg text.

cloud = read.csv(file.choose())
cloud$text<- as.character(cloud$text)

## Step 2: Using gsub to extract only the words starting from A-Z or a-z

cloud$text = gsub("[^A-Za-z///' ]", " ", cloud$text)

## Step 3: Using TM package we will creating corpus 
cloud_corpus<- Corpus(VectorSource(cloud$text))

inspect(cloud_corpus) # Inspect is used to inspect to docs.

## Step 4: Will do data clearing by removing stopwords, Punctuation

cloud_corpus = tm_map(cloud_corpus, removeNumbers) # To remove numbers
cloud_corpus = tm_map(cloud_corpus, content_transformer(tolower)) # to convert everthing to lower case
cloud_corpus = tm_map(cloud_corpus, removePunctuation) # to reomve punctuation
cloud_corpus = tm_map(cloud_corpus, PlainTextDocument) # to create a plain text documents
cloud_corpus<- tm_map(cloud_corpus, removeWords, stopwords(kind = "en")) # to remove stopwords
cloud_corpus<- tm_map(cloud_corpus, removeWords, c('i', 'the', 'will', 'https', 'amp', '&')) # to remove user based stopwords

## Step 5: Converting Corpus into term document matrix and creating a data frame
tdm <- TermDocumentMatrix(cloud_corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE) # this will sum the row which will give the frequenty of each words
d <- data.frame(word = names(v),freq=v)
top_50<- head(d, 50) # Just to check to 50 words

## Step 6: Creating a word cloud 

library(wordcloud)
set.seed(1234)

wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=100, random.order=FALSE, rot.per=0.20, 
          colors=brewer.pal(8, "Dark2"))


