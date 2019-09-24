# Install library yang dibutuhkan
install.packages('readr')
install.packages('tm')

# Import library
library(readr)
library(tm)

# Import Data
datatext <- read_csv("data-raw/hasilcrawling-zonasi.csv")

# Select Text
data.cleaning=datatext$text
data.cleaning.text=Corpus(VectorSource(data.cleaning))

# Pre-Preocessing
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
clean <- tm_map(data.cleaning.text, removeURL)

removeNL <- function(y) gsub("\n", " ", y)
clean <- tm_map(clean, removeNL)

removepipe <- function(z) gsub("<[^>]+>", "", z)
clean <- tm_map(clean, removepipe)

remove.mention <- function(z) gsub("@\\S+", "", z)
clean <- tm_map(clean, remove.mention)

remove.hashtag <- function(z) gsub("#\\S+", "", z)
clean <- tm_map(clean, remove.hashtag)

removeamp <- function(y) gsub("&amp;", "", y)
clean <- tm_map(clean, removeamp)

removetitik3 <- function(y) gsub("[[:punct:]]", "", y)
clean <- tm_map(clean, removetitik3)

remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
clean <- tm_map(clean,remove.all)

clean <- tm_map(clean, tolower)

clean <- tm_map(clean, stripWhitespace)


##load stopword-ID
stopwordID <- "data/ID-Stopwords.txt"
##membaca stopwordID perbaris
cStopwordID<-readLines(stopwordID);

#load slangword
slang <- read.csv("data/Slangword.csv", header=T)
old_slang <- as.character(slang$old) 
new_slang <- as.character(slang$new)

#load stemming
stemm <- read.csv("data/Stemming.csv", header=T)
old_stemm <- as.character(stemm$old)
new_stemm <- as.character(stemm$new)

#load lemmatization
lemma <- read.csv("data/Lemmatization.csv", header=T)
old_lemma <- as.character(stemm$old)
new_lemma <- as.character(stemm$new)

stemmword <- function(x) Reduce(function(x,r) gsub(stemm$old[r],stemm$new[r],x,fixed=T),
                                seq_len(nrow(stemm)),x)
clean <- tm_map(clean,stemmword)
slangword <- function(x) Reduce(function(x,r) gsub(slang$old[r],slang$new[r],x,fixed=T),
                                seq_len(nrow(slang)),x)
clean <- tm_map(clean,slangword)
lemmatization <- function(x) Reduce(function(x,r) gsub(lemma$old[r],lemma$new[r],x,fixed=T),
                                    seq_len(nrow(lemma)),x)
clean <- tm_map(clean,lemmatization)

clean <- tm_map(clean, removeWords, cStopwordID)
writeLines(strwrap(clean[[2]]$content, 100))

dataframe=data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)

# Show Dataset
View(dataframe)

# Save Sebagai CSV
write.csv(dataframe,file = 'data-raw/text-clean.csv')
