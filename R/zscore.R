#' A z-score calculates a probability score that compares the observed collocate frequency to its expected value
#' 
#' @param doc A character vector or list of character vectors
#' @param keyword A character vector containing a keyword 
#' @param window The number of context words to be displayed around the keyword Default 6
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param remove_stopwords Remove stopwords, derived from Quanteda's list
#' @param min_count The minimum frequency to include in the score
#' @param span Whether to include a window's width of words to the left of the keyword, to the right or on both sides
#' @import tibble dplyr
#' @importFrom utils globalVariables
#' @include get_freqs.R
#' @keywords z_score, collocates, kwic
#' @export



zscore <- function(doc, keyword, window = 6, ngram = 1, remove_stopwords = TRUE, min_count = 2, span = "both"){
  
  # Get frequencies
      freqs <- get_freqs(doc = doc, keyword = keyword, window = window, ngram = ngram, remove_stopwords = remove_stopwords, span = span) %>%
            filter(kwic_count >= min_count) 
      
      
  #calculcate the z-score
  z_score <- freqs %>%
        tibble::add_column(wordcount = rep(sum(stringr::str_count(doc, "\\S+")), nrow(.))) %>% # Total wordcount
        tibble::add_column(keyword_count = rep(sum(stringr::str_count(doc, keyword)), nrow(.))) %>% # Number of times the keyword occurs
        #prob = Probability of collocate occuring where the keyword does not occur:
        #frequency in document / overall word count - freq as kwic
        dplyr::mutate(prob = as.numeric((doc_count)/(wordcount - kwic_count))) %>%
        # expected: how many times would collocate occur if randomly distributed?:
        # prob * freq of node * window * span
        dplyr::mutate(expected = as.numeric((prob*keyword_count*((window*2)+(unlist(length(stringr::str_split(keyword, " ")))))))) %>%
        # (Fn,c-E/sqrt(E(1-p)))
        dplyr::mutate(zscore = as.numeric((kwic_count - expected)/(sqrt(expected*(1-prob))))) %>%
        dplyr::select(ngram, zscore) %>%
        dplyr::arrange(desc(zscore))
  
return(z_score)
}
