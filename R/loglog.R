#' log-log calculation
#' 
#' @param doc Acharacter vector or list of character vectors
#' @param keyword A key word or phrase to test
#' @param window The number of context words to be displayed around the keyword Default 6
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param remove_stopwords Remove stopwords, derived from Quanteda's list
#' @param min_count Collocates that occur fewer times than floor will be removed
#' @param span Whether to include a window's width of words to the left of the keyword, to the right or on both sides 
#' @include get_freqs.R
#' @import tibble dplyr
#' @importFrom utils globalVariables
#' @importFrom stringr str_split
#' @keywords mutual information, collocates, kwic
#' @export

loglog <- function(doc, keyword, window = 6, ngram = 1, remove_stopwords = TRUE, min_count = 2, span = "both"){
      
      # Using the forumula here: http://www.aclweb.org/anthology/J90-1003
      
      freqs <- get_freqs(doc, keyword, window, ngram, remove_stopwords, span = span) %>%
            filter(kwic_count >= as.numeric(min_count))
      
      # Calculate the mi score
      loglog <- freqs %>%
            add_column(wordcount = rep(sum(str_count(doc, "\\S+")), nrow(.))) %>% # Total wordcount
            add_column(keyword_count = rep(sum(str_count(doc, keyword)), nrow(.))) %>% # Number of times the keyword occurs
            mutate(probxy = as.numeric((kwic_count^3)/wordcount)) %>% # The probability of x and y collocating
            mutate(probx = as.numeric(keyword_count/wordcount)) %>% #the probability of the keyword occuring
            mutate(proby = as.numeric(doc_count/wordcount)) %>% # The probabilit of a collocate occurding across the full document
            mutate(span = (window*2)+(length(unlist(str_split(keyword, " "))))) %>%
            mutate(top = as.numeric(log(probxy/(probx*proby*span))*log(probxy))) %>%
            mutate(`log-log` = as.numeric(log2(top^2))) %>%
            arrange(desc(`log-log`)) %>%
            select(ngram, `log-log`)
      
      return(loglog)
}