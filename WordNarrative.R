# September 6, 2017
# I am adding some documentation in this header
# Then I will make a commit using R Studio
# Ultimately I hope this goes to GitHub

library(tm)
library(SnowballC)
library(wordcloud)

setwd('/Users/edwardtaylorUTK/Desktop/2015WCfiles')

jeopQ <-   read.csv("./INJZ_OCCUR_RSN.txt", stringsAsFactors = FALSE,  sep = "\t", header=F)
#The actual questions are available in the Question column.

#Delete columns 1 & 2
jeopQ <- jeopQ[c(2:nrow(jeopQ)) , -c(1:2)]


# Remove non-UTF8 characters
jeopQ <- sapply(jeopQ, function(row) iconv(row, "latin1", "ASCII", sub=""))

# Get words in lower case
jeopQ <- tolower(jeopQ)

#Now, we will perform a series of operations on the text data to simplify it.
#First, we need to create a corpus.
jeopCorpus <- Corpus(VectorSource(jeopQ))

#Next, we will convert the corpus to a plain text document.
jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)

# Strip whitespace
jeopCorpus <- tm_map(jeopCorpus, stripWhitespace)

#Then, we will remove all punctuation and stopwords. Stopwords are commonly used words in the English language such as I, me, my, etc. You can see the full list of stopwords using stopwords('english').
jeopCorpus <- tm_map(jeopCorpus, removePunctuation)

# Remove numbers
jeopCorpus <- tm_map(jeopCorpus, removeNumbers)

# Remove stopwords
jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords('en'))

# Remove other meaningless words
NoGoodWords <- c('left', 'right', 'ee', 'employe', 'employee', 'injuri', 'went', 
                 'caus', 'get', 'got','use', '.get', 'alleg', 'alleged', 'allege',
                 'cause','part')
jeopCorpus <- tm_map(jeopCorpus, removeWords, NoGoodWords)



#Next, we will perform stemming. This means that all the words are converted to their stem (Ex: learning -> learn, walked -> walk, etc.). This will ensure that different forms of the word are converted to the same form and plotted only once in the wordcloud.
jeopCorpus <- tm_map(jeopCorpus, stemDocument)

x <- c('fell', 'slip', 'trip', 'right','left', 'fall', 'employee')

stemCompletion(x, jeopCorpus[[1]][[1]], type="longest")

#Now, we will plot the wordcloud.
pal = brewer.pal(5,"Reds")
wordcloud(jeopCorpus[[1]][[1]], max.words = 49, random.order = F,
          random.color = F,
          colors = pal)
