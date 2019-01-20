#' log-likelihood calculation derived from http://ucrel.lancs.ac.uk/llwizard.html
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

loglikelihood <- function(doc, keyword, window = 6, ngram = 1, remove_stopwords = TRUE, min_count = 2, span = "both"){
      
      # Using the forumula here: http://www.aclweb.org/anthology/J90-1003
      
      freqs0 <- get_freqs(doc, keyword, window, ngram, remove_stopwords, span = span) 
      # Extract the total number of words in the kwic 'corpus
      kwic_total_words <- sum(freqs0$kwic_count)
      freqs <- freqs0 %>%
            filter(kwic_count >= as.numeric(min_count))
      
      calc_ll <- function(a, b, c, d){ # log likelhood function from http://rdues.bcu.ac.uk/bncweb/manual/bncwebman-collocation.htm
            loglikelihood <- 2*((a*log(a))+(b*log(b))+(c*log(c))+(d*log(d)) - ((a+b)*log(a+b)) - ((a+c)*log(a+c)) - ((b+d)*log(b+d)) + ((a+b+c+d)*log(a+b+c+d)))
                return(loglikelihood)
      }
      
      # Calculate the log likelihood score
      loglikelihood <- freqs %>%
            add_column(wordcount = rep(sum(str_count(doc, "\\S+")), nrow(.))) %>% # Total wordcount
            mutate(c = as.numeric(kwic_total_words-kwic_count)) %>%
            mutate(d = as.numeric(wordcount - doc_count)) %>%
            mutate(E1 = as.numeric((c*(kwic_count+doc_count)/(c+d)))) %>%
            mutate(E2 = as.numeric((d*(kwic_count+doc_count)/(c+d)))) %>%
            mutate(loglikelihood = as.numeric(2*((kwic_count*(log(kwic_count/E1)))+(doc_count*log(doc_count/E2))))) %>%
            select(ngram, loglikelihood) %>%
            arrange(desc(loglikelihood)) %>%
            mutate(`p <` = case_when(loglikelihood >= 15.13 ~ "0.0001",
                                            loglikelihood >= 10.83 & loglikelihood < 15.13 ~ "0.001",
                                            loglikelihood >= 6.63 & loglikelihood < 10.83 ~ "0.01",
                                            loglikelihood >= 3.84 & loglikelihood < 6.63 ~ "0.05"))
            
      return(loglikelihood)
}