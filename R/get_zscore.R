#' A z-score calculates a probability score that compares the observed collocate frequency to its expected value
#' 
#' @param doc A character vector or list of character vectors
#' @param keyword A character vector containing a keyword 
#' @param window The number of context words to be displayed around the keyword Default 6
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param min_count The minimum frequency to include in the score
#' @param cache Organising collocates is the most time-consuming step in calculating frequencies and other collocation algorithms. The memoise package is used to cache specific iterations of this process. Default FALSE.
#' @import tibble dplyr
#' @importFrom utils globalVariables
#' @importFrom stringr str_split
#' @include get_freqs.R
#' @keywords z_score, collocates, kwic
#' @export

if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "Document Frequency", "prob", "expected", "z score"))

get_zscore <- function(doc, keyword, window = 6, ngram = 1, min_count = 2, cache = FALSE){
  
  # Get frequencies
  freqs0 <- internal_get_freqs(doc = doc, keyword = keyword, window = window, ngram = ngram, min_count = min_count, cache = FALSE)
  
  freqs <- freqs0[[1]]
  pattern_occurrences <- freqs0[[2]]
  wordcount <- sum(str_count(doc, "\\S+"))
  #calculcate the z-score
  z_score <- freqs %>%
      #prob = Probability of collocate occuring where the node does not occur:
      #frequency in document / overall word count - freq of node
      mutate(prob = as.numeric((`Document Frequency`)/(wordcount - pattern_occurrences))) %>%
      # expected: how many times would collocate occur if randomly distributed?:
      # prob * freq of node * window * span (window but here calculated from collDB list)
      mutate(expected = as.numeric((prob*pattern_occurrences*((window*2)+(length(str_split(keyword, " "))))))) %>%
      # (Fn,c-E/sqrt(E(1-p)))
      mutate(`z score` = as.numeric((`Collocate Frequency` - expected)/(sqrt(expected*(1-prob))))) %>%
      select(ngram, `z score`) %>%
      arrange(desc(`z score`))
  
return(z_score)
}
