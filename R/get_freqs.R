#' Extract frequencies for the keywords in context and the full document
#' 
#' @param doc A character vector or list of character vectors
#' @param keyword A key word or phrase to test
#' @param window The number of context words to be displayed around the keyword Default 6
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param remove_stopwords Remove stopwords, derived from Quanteda's list
#' @param span Whether to include a window's width of words to the left of the keyword, to the right or on both sides
#' @import tibble magrittr dplyr
#' @importFrom stringi stri_replace_all_fixed
#' @importFrom stringr str_split str_replace_all
#' @importFrom tidytext unnest_tokens
#' @importFrom quanteda tokens tokens_remove kwic stopwords
#' @importFrom splitstackshape cSplit_l
#' @importFrom purrr map map2 is_character
#' @keywords mutual information, collocates, kwic
#' @export


get_freqs <- function(doc = doc, keyword = keyword, window = 6, ngram = 1, remove_stopwords = TRUE, span = "both"){

      
# Some sanity checks
      
      # If the doc is a list, unlist it.
      # If it is not a character vector, stop.
      if(is.list(doc) == TRUE){
            doc <- unlist(doc)
      }
      if(purrr::is_character(doc) != TRUE){
            stop("collocateR will only act on character vectors (for now).")
      }
      if(length(doc) > 1 && unique(purrr::map(doc, length)) == 1){
            doc <- paste(doc, sep = " ", collapse = " ")
      }
      
      # Sanity check that the window is a double
      window <- as.double(window)
      # ...and that the keyword exists
      if(!exists("keyword")){stop("Please supply a keyword")}
      
      # Check that span is correct
      span <- tolower(span)
      if(span !="both" & span != "b" & span !="left" & span != "l" & span != "right" & span != "r"){
            stop("span must equal both or left or right")
      }
      
      
      # Remove spaces from the keyword / phrase
      keyword_original <- keyword
      if(length(unlist(stringr::str_split(keyword, " ")) > 1)){
      keyword <- keyword_original %>%
            stringi::stri_replace_all_fixed(., " ", "_")
      doc <- gsub(keyword_original, keyword, doc)
      }
      
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
      kwic_scheme <- quanteda::kwic(x = doc, pattern = keyword, valuetype = "fixed", window = window)
      if(nrow(kwic_scheme) == 0){
            stop("No keywords were found")
      }
      
      
      split_nas_emptyvectors <- function(vector){
            vector_split <- unlist(stringr::str_split(vector, " "))
            
            if(length(vector_split) < window){
                  times_length = window-(length(vector_split))
                  returned_vector <- c(rep(NA, times_length), vector_split)
            } else {
                  returned_vector <- vector_split
            }
            returned_vector[returned_vector == ""] <- NA # Replace empty elements with NA
            return(returned_vector)
      }
      
      # This will create two tibbles of kwics if span = "both" or one if span = "left" or "right"
      # The aim here is 
      #1. to create a single column of kwic words, alongside docid and location. 
      #2. To process for empty phrases or locations
      #3. To designate locations based on pre or post
      
      kwics_process <- function(kwic_scheme, designated_span){
            
            kwics_process0 <- kwic_scheme %>%
            tibble::as_tibble(.) %>% 
            dplyr::select(docname, from, keyword, designated_span) %>% # narrow down to necessary cols
            tibble::add_column(id = rep(1:nrow(.))) %>% # Give each kwic element its own idenfier
            splitstackshape::cSplit_l(., split.col = designated_span, sep = " ") %>% # split string in the desired span
            dplyr::select(id, docname, from, working_list = paste(designated_span, "list", sep = "_")) %>% # narrow down again
            dplyr::mutate(working_list = purrr::map(working_list, function(x) split_nas_emptyvectors(x))) %>% # run the empty and short vectors function for the left 
            dplyr::mutate(working_list = purrr::map(working_list, function(x) 
                        tibble::tibble(word = x))) %>% # Turn the vectors into tibbles
            dplyr::mutate(working_list = purrr::map2(working_list, docname, function(x, y) x %>%
                                                tibble::add_column(docname = rep(y, window))))  # Add docnames to the tibbles
                                                
            
            if(designated_span == "pre"){
                  kwics_process <- kwics_process0 %>%
                        dplyr::mutate(working_list = purrr::map2(working_list, docname, function(x, y) x %>%
                                                                tibble::add_row(word = keyword, docname = y))) %>% # the keyword plus its docname))
                        dplyr::mutate(working_list = purrr::map2(working_list, from, function(x, y) x %>%
                                                                tibble::add_column(keyword_loc = seq((y-(nrow(x)-1)), y)))) %>% # Add locations to the left tibbles
                        dplyr::mutate(working_list = purrr::map2(working_list, id, function(x, y) x %>%
                                                                tibble::add_column(id = rep(y, nrow(x))))) # add id
            }
            if(designated_span == "post"){
                  kwics_process <- kwics_process0 %>%
                        dplyr::mutate(working_list = purrr::map2(working_list, from, function(x, y) x %>%
                                                                tibble::add_column(keyword_loc = seq((y+1), (y+(nrow(x))))))) %>% # Add locations to the right tibbles
                        dplyr::mutate(working_list = purrr::map2(working_list, id, function(x, y) x %>%
                                                                       tibble::add_column(id = rep(y, nrow(x))))) # add id
            }
            
            return(kwics_process)
}
            
      # Develop a processed list by span
      if(span == "both" | span == "b"){
            left_kwics <- kwics_process(kwic_scheme, designated_span = "pre") %>%
                  dplyr::rename(pre_list = working_list)
            right_kwics <- kwics_process(kwic_scheme, designated_span = "post") %>%
                  dplyr::rename(post_list = working_list)
      }
      if(span == "left" | span == "l"){
            left_kwics <- kwics_process(kwic_scheme, designated_span = "pre") %>%
                  dplyr::rename(pre_list = working_list)
      }
      if(span == "right" | span == "r"){
            right_kwics <- kwics_process(kwic_scheme, designated_span = "post") %>%
                  dplyr::rename(post_list = working_list)
      }
      
      # Next step: 
      #1. Combine rows depending on span
      #2. process overlaps away
      #3. extract the full sentences
      #4. Break into ngrams
      
      get_ngrams <- function(kwics_processed){
            if(span == "both" | span == "b"){
                  ngrams0<- dplyr::bind_rows(left_kwics$pre_list, right_kwics$post_list) 
            }
            if(span == "left" | span == "l"){
                  ngrams0 <- dplyr::bind_rows(left_kwics$pre_list)
            }
            if(span == "right" | span == "r"){
                  ngrams0 <- dplyr::bind_rows(right_kwics$post_list)
            }
                  
      ngrams <- ngrams0 %>%
            dplyr::group_by(docname) %>% # Group by docname on the off-chance that two docs have the same loc
            dplyr::distinct(keyword_loc, .keep_all = TRUE) %>%
            dplyr::ungroup(.) %>%
            dplyr::group_by(id) %>% # group by id to retrieve full sentences
            dplyr::mutate(full_sentence = paste(word, sep = " ", collapse = " ")) %>% 
            dplyr::select(docname, id, full_sentence) %>%
            dplyr::mutate(full_sentence = stringr::str_replace_all(full_sentence, "_", " ")) %>% # Remove underlines remaining in keywords
            dplyr::distinct(id, .keep_all = TRUE) %>% # Just keep one sentence per id
            tidytext::unnest_tokens(., ngram, full_sentence, token = "ngrams", n = ngram) %>% # Get ngrams and count
            dplyr::ungroup(.) %>%
            dplyr::group_by(ngram) %>%
            dplyr::summarise(kwic_count = n())
            
            return(ngrams)
      }
      ngrams <- get_ngrams(kwics_processed)

      doc <- stringr::str_replace_all(doc, "_", " ") # remove underlines from doc: no longer needed
      
      process_all_words <- function(doc){
      all_words <- doc %>% tibble::enframe(name = "text") %>% 
            tidytext::unnest_tokens(., ngram, value, token = "ngrams", n = ngram) %>% # Get ngrams
            dplyr::group_by(ngram) %>%
            dplyr::summarise(doc_count = n())
      
      return(all_words)
      }
      
      all_words <- process_all_words(doc)

      collocates <- ngrams %>% # Assemble final table
            dplyr::left_join(., all_words, by = "ngram") %>%
            dplyr::filter(!(is.na(doc_count))) %>%
            dplyr::arrange(desc(kwic_count))

      return(collocates)
      
}