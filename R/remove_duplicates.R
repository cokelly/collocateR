#' Extract frequencies for the keywords in context and the full document
#' 
#' @param collocates A kwic matrix
#' @param keyword A character vector containing a keyword 
#' @param window The number of context words to be displayed around the keyword Default 6
#' @import tibble dplyr
#' @importFrom stringr str_count
#' @importFrom purrr map map2
#' @importFrom tidytext unnest_tokens
#' @importFrom stringr str_split 
#' @importFrom utils globalVariables
#' @importFrom tidyr unnest
#' @keywords mutual information, collocates, kwic

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "pre", "post", "full_window0", "from", "to", "docname", "full_window", "left_locs", "right_locs", "word", "all_locs00", "all_locs0"))

remove_duplicates <- function(collocates, keyword = keyword, window = window){ # Remove duplicates
      # Create lists of locations to left and right
      
      # Note that if the to an from integers are identical (for one word keywords) then we will have them duplicated here. They are removed later.
      process_left <- function(intgr){
            left <- seq(as.double(intgr)-window, as.double(intgr))
            return(left)
      }
      process_right <- function(intgr){
            right <- seq(as.double(intgr), as.double(intgr)+window)
            return(right)
      }
      # For efficient processing best to treat all collocates as a single string, but we don't want to process ngrams across windows. So add a semi-random phrase at the end of each window, process and remove any ngram that includes it afterwards 
      sentence_end_marker <- paste("sentenceendmarker", stringi::stri_rand_strings(1, 15, '[a-z]'), sep = "")
      
      # A sanity-check function for ensuring that no window strings are shorter than the window
      # Full span plus 1 for the sentence end markers
      expected_window_span <- (as.double(window)*2)+length(unlist(strsplit(keyword, " ")))+1
      lengthen_strings <- function(string){
            container_list <- as.list(rep(NA, times = expected_window_span))
            divided_string <- unlist(stringr::str_split(string, " "))
            container_string <- unlist(lapply(seq(1:length(container_list)), function(x) container_list[[x]] <- divided_string[x]))
            container_string <- paste(container_string, sep = " ", collapse = " ")
            return(container_string)
      }
      # Columns for full window, left locations and right locations
      collocate_windows_and_locs <- collocates %>%
            as_tibble %>% 
            # paste all words plus 'sentence' end marker 
            mutate(full_window0 = paste(pre, keyword, post, sentence_end_marker)) %>%
            # What word length is the string?
            mutate(full_window_span = str_count(full_window0, "\\S+")) %>% 
            #If the string's word length is shorter than expected (an artefact of a Kwic being at the document's start or end) then fill with NAs 
            dplyr::mutate(full_window = dplyr::case_when(full_window_span < expected_window_span ~ lengthen_strings(full_window0), full_window_span == expected_window_span ~full_window0)) %>% 
            # left locs
            dplyr::mutate(left_locs = purrr::map(from, .f = process_left)) %>% 
            # right locs
            dplyr::mutate(right_locs = purrr::map(to, .f = process_right)) %>% 
            # select relevant cols
            select(docname, from, to, full_window, left_locs, right_locs)
      # Place all collocate words in a column 
      all_collocates <- collocate_windows_and_locs$full_window %>% 
            as_tibble %>% 
            tidytext::unnest_tokens(., word, value)
      # A short function to add NA to the end of a list. Necessary because we added a end marker at the end of each string above.
      addNA <- function(list){
            list_with_na <- list %>% c(., paste(stringi::stri_rand_strings(1, 15, "[A-Za-z]"), stringi::stri_rand_strings(1, 150, "[0-9]")))
            return(list_with_na)
      }
      # Place all locations in a list, along with docnames
      all_locs <- collocate_windows_and_locs %>% 
            mutate(all_locs00 = map2(left_locs, right_locs, .f = c)) %>% # put the two windows sides together (can intervene here if I want to identify left or right collocates in future)
            mutate(all_locs0 = map(all_locs00, unique)) %>% # exclude any duplicates from the kwic function
            mutate(all_locs = map(all_locs0, addNA)) %>%
            select(docname, all_locs) %>%
            tidyr::unnest(., all_locs) # flatten the lists
      collocates_and_locs_no_duplicates <- bind_cols(all_collocates, all_locs) %>%
            # Select only relevant columns for identifying duplicates
            select(docname, word, location = all_locs) %>%
            # Ensure only unique rows are maintained
            group_by(docname, word, location) %>%
            distinct %>%
            ungroup %>% 
            # Return words only
            select(word)
      processed_collocates <- list(sentence_end_marker, collocates_and_locs_no_duplicates)
      
      return(processed_collocates)
}