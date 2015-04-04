read_file <- function(file){
    file <- paste("../data/final/en_US/en_US.",file,".txt", sep="")
    con <- file(file, encoding="UTF-8")
    content <- scan(con,what=character(), sep="\n", skipNul = TRUE)
    close(con)
    return(content)
}

news <- read_file("news")
twitter <- read_file("twitter")
twitter <- twitter[sample(length(twitter), size=0.1*length(twitter))]

news_words <- unlist(strsplit(news, " +"))
twitter_words <- unlist(strsplit(twitter, " +"))


words <- c(news_words,twitter_words)
words <- gsub("[^[:alpha:]]","",words)
words <- words[nchar(words)>2]

w_freq <- head(sort(table(words),TRUE),10000)
words <- tolower(names(w_freq))

save(words, file="words.saved")