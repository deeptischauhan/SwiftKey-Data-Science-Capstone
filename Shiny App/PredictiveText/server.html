
#R server for Coursera Data Science Capstone 

library(shiny); library(tm); library(ngramrr)

# Load functions to clean the data
clean_data <- function(corpus) {
  raw_data <- tm_map(corpus, removePunctuation) #Remove punctuation
  raw_data <- tm_map(raw_data, removeNumbers) #Remove numbers or digits
  raw_data <- tm_map(raw_data, stripWhitespace) #Remove any whitespace
  raw_data <- tm_map(raw_data, removeWords, stopwords("en")) #Remove any English stopwords  
  raw_data <- tm_map(raw_data, content_transformer(tolower)) #Convert text to lower case
  # raw_data <- tm_map(raw_data, stemDocument) # Stemming the text.
  return(raw_data) }

#Build the 2-gram and 3-gram model
build_model <- function(phrase) {
    if (typeof(phrase) == "character") {model_3gram <- function(tokens) {key <- function(tokens) 
      {paste(tail(tokens,n = 2)[1], tail(tokens,n = 2)[2]) }
      
      # find matches and their count
      match_number <- function(phrase) {sapply(names(which(sapply(Terms(tdm_trigram),
                function(terms) {grepl(phrase, paste(strsplit(terms, split = " ")[[1]][1], 
                                      strsplit(terms, split = " ")[[1]][2]), ignore.case = TRUE) }))), 
              function(match) sum(tm_term_score(tdm_trigram, match)))
      }
      
      # Determine last word of the match with highest count
      end_highest_match <- function(phrase) {matches <- match_number(phrase)
        if (length(matches) > 0) 
          {tail(strsplit(names(head(which(matches == max(matches)), n = 1) ), split = " ")[[1]], n = 1) }  
        else model_2gram(tail(corpus_input, n = 1)) }
      return(end_highest_match(key(tokens)))
    }
    
    model_2gram <- function(token) {
      # Find the applicable matches and count the number of occurrences 
      match_number <- function(phrase) {
        sapply(names(which(sapply(Terms(tdm_bigram),function(terms) {grepl(phrase,
                    strsplit(terms, split = " ")[[1]][1], ignore.case = TRUE) } ))),
          function(match) sum(tm_term_score(tdm_bigram, match))) }
      
      # find the last word of the most frequent match
      end_highest_match <- function(phrase) {
        matches <- match_number(phrase)
        if (length(matches) > 0) {tail(strsplit(names(head(which(matches == max(matches)), n = 1)), 
                        split = " ")[[1]],n = 1) } 
        else unigram_model(tail(corpus_input, n = 1)) }
      return(end_highest_match(token) )
    }
    
    unigram_model <- function(token) {
      links <- findAssocs(tdm_unigram, token, corlimit = .99)[[1]]
      if (length(links) > 0) {names(sample(which(links == max(links)), 1)) } 
      else return("will")
    }
    
    # Clean and Pre-process the input phrase
    corpus_input <- VCorpus(VectorSource(phrase), list(reader = PlainTextDocument) )
    corpus_input <- clean_data(corpus_input)
    corpus_input <- scan_tokenizer(corpus_input[[1]][[1]][1])
    
    return(if (length(corpus_input) >= 2) {model_3gram(corpus_input)} 
      else if (length(corpus_input) == 1) {model_2gram(corpus_input)} 
      else return("will") )} 
  else {stop("non-character or null input")}
}

# Load term-document matrices
load("ngrams.RData")

# Function to run the model
shinyServer(function(input, output) {
    output$phrase <-renderText({if (input$predictButton == 0) "Please enter text"
          else input$phrase})
    output$word <-renderText({if (input$predictButton == 0) "Please enter text"
         else build_model(input$phrase) } ) 
    }
)
