## parler json file ###

install.packages("rprintf")


library(jsonlite)
library(rjson)
library(ndjson)
library(data.table)
library (parallel)
library(dplyr)
library(tibble)
library(rlang)
library(tidyverse)
library(plyr)
library(stringr)
library(stringi)
library(textclean)
library(stringfish)
library(qdap)
library(rJava)
library(rprintf)
library(wordcloud)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(SnowballC)
library(Rcpp)



pagesize=10000

json_parler <-"parler_data000000000147.ndjson"


initialJSON= readLines(json_parler, n = pagesize)
collapsedJSON <- paste(initialJSON[2:pagesize], collapse = ",")
fixedJSON <- sprintf("[%s]", collapsedJSON, collapse= ",")
read_json <-jsonlite::fromJSON(fixedJSON)


parler_new <- read_json %>% 
  select(-starts_with("id"), -starts_with("links"), -starts_with("parent"), -starts_with("color"), 
         -starts_with("controversy"), -starts_with("downvotes"),-starts_with("color"), 
         -starts_with("commentDepth"), -starts_with("color"),-starts_with("post"), -starts_with("score"),
         -starts_with("color"), -starts_with("isPrimary"), -starts_with("color"), -starts_with("conversation"),
         -starts_with("replyingTo"), -starts_with("creator")) 


### cast all blank spaces (delim by "") to NAs ###

parler_new[parler_new==""] <- NA

## then list rows that have missing values 

parler_new[!complete.cases(parler_new),]

## now all missing values in place, drop missing cases from data frame and create a new dataset without missing data ##

parler_2 <- na.omit(parler_new)

head(parler_2)

parler_2 <- apply(parler_2,2,as.character)

### write a CSV file ###

write.csv(parler_2, "C:\\Users\\charl\\OneDrive\\Documents\\R\\Parler\\Parler_comments.csv")

parler_new <- read.csv(file="C:\\Users\\charl\\OneDrive\\Documents\\R\\Parler\\data\\\\comments\\Parler_comments.csv", encoding="UTF-8", sep=",")

view(parler_new)

## filter comments based on qanon mentions ##

parler_new <- parler_new %>%
  filter(grepl("WWG1WGA|QAnon|SAVETHECHILDREN|SaveTheChildren|saveourchildren|save the children!|SaveOurChildren|qarmy|Anons|Q|qanon|wwg1wga|WWGAWGO|the great awakening|Great Awakening|pedophile", body))


### create the corpus for the wordcloud and for cleaning text ## 

docs <- Corpus(VectorSource(parler_filter))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


#Convert the text to lower case#

docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove own stop words
docs <- tm_map(docs, removeWords, c("us", "meme", "htpps"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
docs <- tm_map(docs, stemDocument)

##create a document matrix for wordcloud#
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

##create wordcloud###

wordcloud(docs,
scale=c(5,0.5),
max.words=100,
random.order=FALSE,
rot.per=0.35,
use.r.layout=FALSE,
colors=brewer.pal(8, 'Dark2'))










