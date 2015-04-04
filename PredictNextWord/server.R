library(shiny)
library(ngram)

load_data <- function(x) {
    load(paste("data/",x,".saved",sep=""))
    if(x=="freqs") x <- freqs else x <- words
    return(x)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

predict_next_word <- function(phrase) {
    set.seed(2365)
    occurrences <- 0
    next_word <- ""
    if(phrase=="Enter your phrase here to see the next word on your") {
        answer <- answer <- list("right",phrase,occurrences)
    } else {
        p_words <- tolower(unlist(strsplit(phrase," +")))
        phrases <- vector()
        if(length(p_words)>=3){
            phrases <- c(phrases,paste(tail(p_words,3),collapse=" "))
            phrases <- c(phrases,paste(tail(p_words[-1],2),collapse=" "))
            for (i in (length(p_words)-2):1) phrases <- c(phrases,paste(trim(p_words[i]),"\\b.+\\b",trim(p_words[length(p_words)]),sep=""))
            phrases <- c(phrases,p_words[length(p_words)])
        } else {
            phrases <- c(phrases,paste(p_words,collapse=" "))
            if(length(p_words)==2) phrases <- c(phrases,paste(p_words[2],collapse=" "))
        }
        phrases <- trim(phrases)
        
        matched_phrases <- data.frame()
        i = 1
        while (nrow(matched_phrases)==0) {
            phrase <- phrases[i]
            regex <- paste(".*\\b",phrase,"\\b ([a-zA-Z]){2}.*",sep="")
            matched_phrases <- freqs[grepl(regex, freqs$Name, perl=TRUE),]
            if (i >= length(phrases)) break
            i <- i+1
        }
        
        if (nrow(matched_phrases)==0) {
            phrase <- paste(p_words,collapse=" ")
            regex <- paste(".*\\b",paste(p_words,collapse="\\b.*|.*\\b"),"\\b.*",sep="")
            text <- freqs[grepl(regex, freqs$Name, perl=TRUE),]
            text <- paste(as.character(text$Name), collapse=" ")
            if(sapply(gregexpr("\\W+", text), length) + 1 >2){
                ng <- ngram(text,n=2)
                b_words <- unlist(strsplit(babble(ng,length(p_words)+1)," +"))
                i <- length(b_words)
                while(i>0){
                    next_word <- b_words[i]
                    if (!next_word %in% p_words) break
                    i <- i-1
                }
            }
            
            occurrences <- -1
            
            if (next_word=="")  {
                next_word <- sample(words,1)  
                occurrences <- -2
            }
            
            answer <- list(next_word,phrase,occurrences)
        } else {
            matched_phrases$word <- trim(gsub(paste(".*",phrase,sep=""),"",matched_phrases$Name))
            matched_phrases$word <- strsplit(matched_phrases$word," +")
            matched_phrases$word <- sapply( matched_phrases$word, function(m) m[1][1] ) 
            matched_phrases <- data.frame(matched_phrases$word, matched_phrases$freq)
            matched_phrases <- as.data.frame(table(matched_phrases))
            matched_phrases <-  matched_phrases[order(-matched_phrases[,3]),c(1,3)]
            occurrences <- sum(matched_phrases[,2])
            matched_phrases$prob <- paste(round(100*matched_phrases$Freq/occurrences, digits=2),"%",sep="")              
            names(matched_phrases) <- c("Word","occurrences","Proportion")
            matched_phrases$Word <- as.character(matched_phrases$Word)
            
            next_word <- matched_phrases$Word[1]
            phrase <- unlist(strsplit(gsub("[^[:alnum:] ]", "", phrase), " +"))
            phrase <- gsub("bb"," ... ",phrase)
            phrase <- paste(as.character(phrase), collapse=" ")
            
            answer <- list(next_word,phrase,occurrences, head(matched_phrases,20))
        }
        
    }   
    
    return(answer)
}

freqs <- load_data("freqs")
words <- load_data("words")

shinyServer(function(input, output) {
    observe({
        if (input$text> 0) {
            result <- predict_next_word(input$text)
            if(length(result)==4) {
                izpis <- as.data.frame(matrix(unlist(result[4][1]),ncol=3))
                names(izpis) <- c("Word","occurrences","Proportion")
            }
            output$next_word <- renderUI({result[1]})
            if(result[3] > 0) { 
                                output$phrase1 <- renderUI(paste('There are ', {result[3]},' occurrences of the phrase "', {result[2]},'" in existing n-grams.',
                                                                  ' The most common word after this phrase is "', {result[1]},'".',sep=""))
                                output$phrase2 <- renderUI(paste('Possible next words of phrase "', {result[2]},'" with their occurrences are listed in the table below.',sep=""))
                                output$phrase3 <- renderUI('')
                                output$table <- renderTable({izpis})
            }
            else {
                output$table <- renderTable({unlist(result[4])})
                output$phrase3 <- renderUI('(c)2015 Franci Jerič')
                output$phrase1 <- renderUI('Welcome to my "Predicting next word" app! The source code is avilable at GitHub:')
                output$phrase2 <- renderUI({   
                    HTML('<a href="https://github.com/frajer/Final-Project-App" target="_blank">https://github.com/frajer/Final-Project-App</a><hr>')
                })
                if(result[3] == -1) {
                    output$phrase1 <- renderUI(paste('There are no occurrences of the phrase "', {input$text}, 
                                                                     '" in existing n-grams. So we use "ngram" package to build new ngrams from lines of text that includes any of the word in the given phrase. And after that we randomly generate new string with bable() function from "ngram" package.',sep=""))
                    output$phrase2 <- renderUI('')
                    output$phrase3 <- renderUI('')
                }
                if(result[3] == -2) {
                    output$phrase1 <- renderUI(paste('There are no occurrences of any of the words in the phrase "', {input$text}, 
                    '" in existing n-grams. So here is one of 10.000 most common words in news and twitter datasets.',sep=""))
                    output$phrase2 <- renderUI('')
                    output$phrase3 <- renderUI('')
                }
            }
        }
    })
    
})

