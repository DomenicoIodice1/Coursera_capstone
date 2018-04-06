library(shiny)

shinyServer(function(input, output) {
  
  # Load packages
  library(repmis)
  library(stringr)
  library(tm)
  library(textmining)
  library(RWeka)
  
  # Load n-gram frequencies
  source_data(
    "https://github.com/DomenicoIodice1/Coursera_capstone/blob/master/German_frequences.RData?raw=true")
  
  # Prediction algorithm
  # Input text will be cleaned as dataset
  clean_function <- function(x) {
    clean<-gsub('[])(;:#%$^*\\~{}[&+=@/"`|<>_]+', "", x)
    clean<-tolower(clean)
    clean<-removePunctuation(clean, preserve_intra_word_dashes = T)
    clean<-removeNumbers(clean)
    clean<-removeWords(clean, stopwords("german"))
    clean<-stemDocument(clean)
    clean<-stripWhitespace(clean)
  }
  
  # Function used to calculate the highest frequences among the words to be predicted
  top_freq_word <- function(words, frequence_list, counts) {
    length_f <- length(unlist(strsplit(as.character(frequence_list$word[1]), " ")))
    pred <- paste(tail(unlist(strsplit(words, " ")), length_f - 1), collapse = " ")
    freq_head <- head(frequence_list[grep(paste("^", pred, " ", sep = ""), frequence_list$word), ], counts)
    sub_freq_head <- gsub(paste("^", pred, " ", sep = ""), "", as.character(freq_head$word))
    sub_freq_head[!sub_freq_head %in% c("s", "<", ">", ":", "-", "o", "j", "c", "m")]
  }
  
  # Prediction algorithm
  prediction_algorithm <- function(text, frequence_list, counts) {
    text_length <- length(unlist(strsplit(text, " ")))
    pred_alg <- NULL
    if(text_length > 3) pred_alg <- top_freq_word(text, frequence_list$penta, counts)
    if(length(pred_alg)) return(pred_alg)
    if(text_length > 2) pred_alg <- top_freq_word(text, frequence_list$quadri, counts)
    if(length(pred_alg)) return(pred_alg)
    if(text_length > 1) pred_alg <- top_freq_word(text, frequence_list$tri, counts)
    if(length(pred_alg)) return(pred_alg)
    pred_alg <- top_freq_word(text, frequence_list$bi, counts)
    if(length(pred_alg)) return(pred_alg)
    as.character(sample(head(frequence_list$uni$word, 30), counts))
  }
  
  # Final algorithm of prediction
  prediction_algorithm_final <- function(text, frequence_list, counts) {
    text <- as.character(clean_function(text)[[1]], remove.punct=T)
    prediction_algorithm(text, frequence_list, counts)
  }
  
  # Final if-else conditional control 
  observe({
    input_char <- as.character(input$text_input)
    counts <- input$choices
    pred_word <- NULL

      if (str_sub(input_char, start = -1) == " ") {
      output$predicted_word=renderPrint(
        cat(prediction_algorithm_final(
          input_char, frequence_list, counts), sep = "\n"))
    } else if (!is.null(pred_word) && lastWords(input_char, 1) %in% pred_word) {
      output$predicted_word=renderPrint(
        cat(prediction_algorithm_final(
            input_char, frequence_list, counts), sep = "\n"))
    } else {
      output$predicted_word=renderPrint(cat(""))
    }
  })
})