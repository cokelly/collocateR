#' Extract frequencies for the keywords in context and the full document
#' 
#' @param doc A character vector or list of character vectors
#' @param keyword A key word or phrase to test
#' @param window The number of context words to be displayed around the keyword Default 6
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param remove_stopwords Remove stopwords, derived from Quanteda's list
#' @import tibble magrittr dplyr
#' @importFrom stringi stri_replace_all_fixed
#' @importFrom stringr str_split str_replace_all
#' @importFrom tidytext unnest_tokens
#' @importFrom quanteda tokens tokens_remove kwic stopwords
#' @importFrom splitstackshape cSplit_l
#' @importFrom purrr map map2
#' @keywords mutual information, collocates, kwic
#' @export


get_freqs <- function(doc, keyword, window = 6, ngram = 1, remove_stopwords = TRUE){

# Some sanity checks
      
      # If the doc is a list, unlist it.
      # If it is not a character vector, stop.
      if(is.list(doc) == TRUE){
            doc <- unlist(doc)
      }
      if(is_character(doc) != TRUE){
            stop("collocateR will only act on character vectors (for now).")
      }
      if(length(doc) > 1 && unique(map(doc, length)) == 1){
            doc <- paste(doc, sep = " ", collapse = " ")
      }
      
      # Sanity check that the window is a double
      window <- as.double(window)
      # ...and that the keyword exists
      if(!exists("keyword")){stop("Please supply a keyword")}
      
            
# Remove spaces from the keyword / phrase
      keyword_original <- keyword
      keyword <- keyword_original %>%
            stringi::stri_replace_all_fixed(., " ", "_")
      doc <- gsub(keyword_original, keyword, doc)

      # Remove stopwords if required
      if(remove_stopwords == TRUE){
            stops = quanteda::stopwords()
            doc <- doc %>% quanteda::tokens(.) %>% # Tokenise
                        quanteda::tokens_remove(., stops, padding = TRUE) %>%
                        purrr::map(., function(x) paste(x, sep = " ", collapse = " ")) %>%
                        unlist
      } else {
            doc <- doc
      }
      
      
      # Get kwics
      kwic_scheme <- quanteda::kwic(doc, pattern = keyword, valuetype = "fixed", window = window)
      if(nrow(kwic_scheme) == 0){
            stop("No keywords were found")
      }
      
      
      split_nas_emptyvectors <- function(vector){
            vector_split <- unlist(str_split(vector, " "))
            
            if(length(vector_split) < window){
                  times_length = window-(length(vector_split))
                  returned_vector <- c(rep(NA, times_length), vector_split)
            } else {
                  returned_vector <- vector_split
            }
            returned_vector[returned_vector == ""] <- NA # Replace empty elements with NA
            return(returned_vector)
      }
      
      # This will create a tibble of kwics
      # The aim here is 
      #1. to create a single column of kwic words, alongside docid and location. 
      #2. To process for empty phrases or locations
      #3. To remove overlaps between kwics
      kwics_process <- function(kwic_scheme){
            kwics_process <- kwic_scheme %>%
            as_tibble %>% 
            select(docname, from, pre, keyword, post) %>% # narrow down to necessary cols
            add_column(id = rep(1:nrow(.))) %>% # Give each kwic element its own idenfier
            splitstackshape::cSplit_l(., split.col = "pre", sep = " ") %>% # split string in pre
            splitstackshape::cSplit_l(., split.col = "post", sep = " ") %>%
            select(id, docname, from, pre_list, post_list) %>% # narrow down again
            mutate(pre_list = purrr::map(pre_list, function(x) split_nas_emptyvectors(x))) %>% # run the empty and short vectors function for the left 
            mutate(pre_list = map(pre_list, function(x) 
                        tibble(word = x))) %>% # Turn the vectors into tibbles
            mutate(pre_list = purrr::map2(pre_list, docname, function(x, y) x %>%
                                                add_column(docname = rep(y, window)) %>%
                                                add_row(word = keyword, docname = y))) %>% # Add docnames to the tibbles and the keyword plus its docname
            mutate(pre_list = purrr::map2(pre_list, from, function(x, y) x %>%
                                                add_column(keyword_loc = seq((y-(nrow(x)-1)), y)))) %>% # Add locations to the left tibbles
            mutate(post_list = purrr::map(post_list, function(x) split_nas_emptyvectors(x))) %>% # run the empty and short vectors function for the right 
            mutate(post_list = purrr::map(post_list, function(x) 
                        tibble(word = x))) %>% # Turn the vectors into tibbles
            mutate(post_list = purrr::map2(post_list, docname, function(x, y) x %>%
                                                 add_column(docname = rep(y, window)))) %>%  # Add docnames to the tibbles
            mutate(post_list = purrr::map2(post_list, from, function(x, y) x %>% 
                                                 add_column(keyword_loc = seq((y+1), (y+nrow(x)))))) %>% # Add locations to the right tibbles
            mutate(pre_list = purrr::map2(pre_list, id, function(x, y) x %>%
                                                add_column(id = rep(y, nrow(x))))) %>% # add id
            mutate(post_list = purrr::map2(post_list, id, function(x, y) x %>%
                                                 add_column(id = rep(y, nrow(x))))) # add id
            return(kwics_process)
}
            kwics_processed <- kwics_process(kwic_scheme)

      # Next step: 
      #1. process overlaps away
      #2. extract the full sentences
      #3. Break into ngrams
      
      get_ngrams <- function(kwics_processed){
            ngrams<- bind_rows(kwics_processed$pre_list, kwics_processed$post_list) %>%
            group_by(docname) %>% # Group by docname on the off-chance that two docs have the same loc
            distinct(keyword_loc, .keep_all = TRUE) %>%
            ungroup %>%
            group_by(id) %>% # group by id to retrieve full sentences
            mutate(full_sentence = paste(word, sep = " ", collapse = " ")) %>% 
            select(docname, id, full_sentence) %>%
            mutate(full_sentence = str_replace_all(full_sentence, "_", " ")) %>% # Remove underlines remaining in keywords
            distinct(id, .keep_all = TRUE) %>% # Just keep one sentence per id
            tidytext::unnest_tokens(., ngram, full_sentence, token = "ngrams", n = ngram) %>% # Get ngrams and count
            ungroup %>%
            group_by(ngram) %>%
            summarise(kwic_count = n())
            
            return(ngrams)
      }
      ngrams <- get_ngrams(kwics_processed)

      doc <- str_replace_all(doc, "_", " ") # remove underlines from doc: no longer needed
      
      process_all_words <- function(doc){
      all_words <- doc %>% tibble %>% 
            tidytext::unnest_tokens(., ngram, ., token = "ngrams", n = ngram) %>% # Get ngrams
            group_by(ngram) %>%
            summarise(doc_count = n())
      
      return(all_words)
      }
      
      all_words <- process_all_words(doc)

      collocates <- ngrams %>% # Assemble final table
            left_join(., all_words, by = "ngram") %>%
            filter(!(is.na(doc_count))) %>%
            arrange(desc(kwic_count))

      return(collocates)
      
}