#' A t-score calculates a probability score that compares the observed collocate frequency to its expected value
#' 
#' @param document A character vector or list of character vectors
#' @param pattern A character vector containing a keyword 
#' @param window The number of context words to be displayed around the keyword Default 5
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param remove_stopwords Remove stopwords from the document (based on tidytext's stopwords data). Default TRUE.
#' @param remove_numerals Remove numerals
#' @param cache Organising collocates is the most time-consuming step in calculating frequencies and other collocation algorithms. The memoise package is used to cache specific iterations of this process. Default FALSE.
#' @import tibble dplyr
#' @include get_freqs.R
#' @keywords t_score, collocates, kwic
#' @export

t_score <- function(document, pattern, window = 5, ngram = 1, floor = 3, remove_stopwords = TRUE, remove_numerals = TRUE, cache = FALSE){
      
  # Get frequencies
      freqs <- get_freqs2(document = document, pattern = pattern, window = window, ngram = ngram, remove_stopwords = remove_stopwords, remove_numerals = remove_numerals, cache = cache)
      
      pattern_occurrences <- freqs[[2]]
      wordcount <- freqs[[3]]
      freqs <- freqs[[1]]
      
      freqs <- freqs %>%
        filter(`kwic freqs` >= floor)
        
      if(nrow(freqs) == 0){
            t_score <- "No collocates. Try setting the floor at a lower level"
      } else {
      
      #calculcate the z-score
      t_score <- tibble(phrase = freqs$word, # Collocates
                        `kwic freq` = freqs$`kwic freqs`,
                        #prob = Probability of collocate occuring where the node does not occur:
                        #frequency in document / overall word count - freq of node
                        prob = freqs$`doc freqs`/(wordcount-pattern_occurrences), 
                        # expected: how many times would collocate occur if randomly distributed?:
                        # prob * freq of node * window * span (window but here calculated from collDB list)
                        expected = prob*pattern_occurrences*(window*2)+(length(unlist(strsplit(pattern, " ")))),
                        # frequency of collocates in context - expected / sqrt (expected * (1-prob))
                        tscore = (freqs$`kwic freqs`) - expected/sqrt(expected*(1 - prob))) # (Fn,c-E/sqrt(E(1-p)))
      
      t_score <- t_score %>% 
        select(phrase, `kwic freq`, `t score` = tscore) %>%
        arrange(desc(`t score`))
      }
      return(t_score)
}
