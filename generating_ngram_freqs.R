library("RWeka")
library("tm")
library("SnowballC")
library(slam)

read_file <- function(file){
    file <- paste("../data/final/en_US/en_US.",file,".txt", sep="")
    con <- file(file, encoding="UTF-8")
    content <- scan(con,what=character(), sep="\n", skipNul = TRUE)
    close(con)
    return(content)
}

n_gram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

build_ngram_freqs <- function(){
    text <- iconv(enc2utf8(text), sub = "byte")
    corpus <- Corpus(VectorSource(text))
    rm(text)
    
    stopwords <- c(stopwords("english"))
    
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords) 
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stemDocument)
    
    tdm <- TermDocumentMatrix(corpus, control = list(tokenize = n_gram_tokenizer))
    rm(corpus)
    tdm <- rollup(tdm, 2, na.rm=TRUE, FUN = sum)
    
    ngram_freq <- sort(rowSums(as.matrix(tdm)),TRUE)
    rm(tdm)
    
    return(ngram_freq)
}


print(Sys.time())
news <- read_file("news")
news <- news[sample(length(news), size=0.5*length(news))]
text <- news
rm(news)
news_ng_freqs <- build_ngram_freqs()
save(news_ng_freqs, file="news_ng_freqs.saved")
rm(news_ng_freqs)
print(Sys.time())

twitter <- read_file("twitter")
#twitter <- twitter[sample(length(twitter), size=0.1*length(twitter))]
twitter <- twitter[sample(length(twitter), size=0.02*length(twitter))]
text <- twitter
rm(twitter)
twitter_ng_freqs <- build_ngram_freqs()
save(twitter_ng_freqs, file="twitter_ng_freqs.saved")
print(Sys.time())

load("news_ng_freqs.saved")
ng_freqs <- c(news_ng_freqs,twitter_ng_freqs)
rm(news_ng_freqs)
rm(twitter_ng_freqs)

freqs <- data.frame(Name=names(ng_freqs),Value=ng_freqs)
rm(ng_freqs)

freqs <- transform(freqs, freq = ave(Value, Name, FUN = sum))
freqs$Value <- NULL
freqs <- unique(freqs)
freqs <- freqs[order(-freqs$freq),]

save(freqs, file="freqs.saved")
print(Sys.time())
