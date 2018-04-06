#load packages
library(stringr)
library(tm)
library(textmining)
library(RWeka)
#read datasets
de_blogs<-readLines("de_DE.blogs.txt", skipNul = T, encoding = "UTF-8")
de_news<-readLines("de_DE.news.txt", skipNul = T, encoding = "UTF-8")
de_twitter<-readLines("de_DE.twitter.txt", skipNul = T, encoding = "UTF-8")
#sample datasets to reduce dimentions
blogs_sample<-sample(de_blogs, 0.001*length(de_blogs))
news_sample<-sample(de_news, 0.001*length(de_news))
twitter_sample<-sample(de_twitter, 0.001*length(de_twitter))
#initial cleaning of the samples
blogs_sample <- gsub('[])(;:#%$^*\\~{}[&+=@/"`|<>_]+', "", blogs_sample)
news_sample<-gsub('[])(;:#%$^*\\~{}[&+=@/"`|<>_]+', "", news_sample)
twitter_sample<-gsub('[])(;:#%$^*\\~{}[&+=@/"`|<>_]+', "", twitter_sample)
#union of the three sampled datasets
samples<-c(blogs_sample, news_sample, twitter_sample)
#function to clean the united sample before creating corpus
clean_function <- function(x) { samples_clean<-removePunctuation(x)
samples_clean<-removeNumbers(samples_clean)
samples_clean<-removeWords(samples_clean, stopwords("german"))
samples_clean<-stemDocument(samples_clean)
samples_clean<-stripWhitespace(samples_clean)
samples_clean<-str_replace_all(samples_clean,"[^[:graph:]]", " ") 
}

samples_clean<-clean_function(samples)
Encoding(samples_clean)<-"UTF-8"
#cration of Corpus
samples_corpus<-tmCorpus(VectorSource(samples_clean), readerControl = list(language = "de"), package = "tm")
samples_corpus<-tm_map(samples_corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
#build function to create Tokenizers for TermDocumentMatrix function
bigrammer<-function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigrammer<-function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
quadrigrammer<-function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
pentagrammer<-function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
#create n-grams
bigrams<-TermDocumentMatrix(samples_corpus, control=list(tokenizer=bigrammer))
trigrams<-TermDocumentMatrix(samples_corpus, control=list(tokenizer=trigrammer))
quadrigrams<-TermDocumentMatrix(samples_corpus, control=list(tokenizer=quadrigrammer))
pentagrams<-TermDocumentMatrix(samples_corpus, control=list(tokenizer=pentagrammer))
#function used to calculate frequences of every n-gram
frequence_df<-function(x) {
  frequence<-sort(rowSums(as.matrix(x)), decreasing = T)
  return(data.frame(word = names(frequence), freq = frequence))
}
#calculate frequences
uni_frequence<-frequence_df(
  removeSparseTerms(
    TermDocumentMatrix(samples_corpus), 0.9999)
)
bi_frequence<-frequence_df(
  removeSparseTerms(
    TermDocumentMatrix(samples_corpus, control = list(tokenize = bigrammer)), 0.9999)
)
tri_frequence<-frequence_df(
  removeSparseTerms(
    TermDocumentMatrix(samples_corpus, control = list(tokenize = trigrammer)), 0.9999)
)
quadri_frequence<-frequence_df(
  removeSparseTerms(
    TermDocumentMatrix(samples_corpus, control = list(tokenize = quadrigrammer)), 0.9999)
)
penta_frequence<-frequence_df(
  removeSparseTerms(
    TermDocumentMatrix(samples_corpus, control = list(tokenize = pentagrammer)), 0.9999)
)
#create and save a list with all the n-gram frequences to be inported for the Shiny App (faster!)
frequence_list<-list("uni" = uni_frequence, "bi" = bi_frequence, "tri" = tri_frequence,
                     "quadri" = quadri_frequence, "penta" = penta_frequence)
save(frequence_list, file="German_frequences.RData")