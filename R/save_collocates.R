#' returns a list containing node locations, window locations, the node and its hash, and a doc_table
#'
#' @param document The document to be analysed
#' @param window The size of the collocate window on each side of tne node
#' @param node A key word or phrase
#' @param remove_stops If TRUE, stopwords are removed (stopwords derived from quanteda package)
#' @param remove_numerals If TRUE, numerals are removed
#' @param remove_punct If TRUE, puntuation is removed
#' @param stem Applies the default "porter" wordStem function from SnowballC. Default 'false'
#' @include mins_to_maxs.R
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' @importFrom digest sha1
#' @importFrom quanteda stopwords
#' @importFrom stringr str_replace_all
#' @importFrom stringi stri_extract_first_words
#' @importFrom qdap strip
#' @importFrom SnowballC wordStem
#' @keywords collocates kwic
#' @export

save_collocates <- function(document, window, node, remove_stops = TRUE, remove_numerals = TRUE, remove_punct = TRUE, stem = FALSE){
      node_length <- length(unlist(strsplit(node, " ")))
      # Test to see if the node phrase is larger than the window
      if(node_length > ((window*2)+1)){ # longer than twice the window plus the keyword
            stop("Error: the node phrase is longer than the kwic window")
      }
      
      # Warnings if various binary options are neither true nor false
      if(remove_stops != TRUE & remove_stops != FALSE){
            warning("remove_stops should be either TRUE or FALSE. Assuming FALSE")
      }
      if(remove_numerals != TRUE & remove_numerals != FALSE){
            warning("remove_numerals should be either TRUE or FALSE. Assuming FALSE")
      }
      if(remove_punct != TRUE & remove_punct != FALSE){
            warning("remove_punct should be either TRUE or FALSE. Assuming FALSE")
      }
      if(stem != TRUE & stem != FALSE){
            warning("stem should be either TRUE or FALSE. Assuming FALSE")
      }
      # Create an empty collDB to return in the event of no collocates being found etc
      return_empty_colldb <- function(){
            left_locs <- list(rep(NA, 6))
            right_locs <- list(rep(NA, 6))
            node <- node
            node_hash <- sha1(node)
            node_recurrence <- NA
            doc_table <- tibble(document) %>%
                  tidytext::unnest_tokens(word,
                                          document,
                                          token = "ngrams",
                                          n = 1)
            all_locs <- list(rep(NA, 13))

            empty_collDB <- list(left_locs, right_locs, node, node_hash, node_recurrence, doc_table, all_locs)
            empty_collDB <- as(object = empty_collDB, Class = "collDB")
            return(empty_collDB)
      }
      # Test that the text is greater than length zero
      anywords <- stringi::stri_extract_first_words(document) # Test for words
      if(length(anywords > 1)){anywords <- anywords[1]} # If the object has multiple documents, use the first word
      if(is.na(anywords)){
            # Print a warning
            if(length(names(document)) == 0){warning("The document has no content. Returned NA")
      } else {
            warning(paste("The document \"",
                          names(document),
                          "\" has no content. Returned NA",
                          sep = ""))
      }
            # # Return a collDB full of NAs
            collocate_locs <- return_empty_colldb()
      } else {
      # Remove numerals
      if(remove_numerals == TRUE){
            document <- str_replace_all(document, "[0-9]", "")
      }
      if(remove_punct == TRUE){
            # Deal with "apostrophe-s".
            
            document <- str_replace_all(document, "[[:punct:]]s", "s")
            document <- str_replace_all(document, "[[:punct:]]t", "t")
            document <- str_replace_all(document, "[^[:alnum:]]", " ")
      }
      # To lower
            document <- tolower(document)
      # Hash the node to to create a single phrase (and ensure stopwords contained in the
      # node aren't removed)
      node1 <- sha1(node)
      document <- gsub(x = document, pattern = paste("\\b", node, "\\b", sep = ""), replacement = node1)
      # Tokenise into a tibble
            word.t <- tibble(document) %>%
                  tidytext::unnest_tokens(word,
                                          document,
                                          token = "ngrams",
                                          n = 1)
      
      # If required remove stopwords
      if(remove_stops == TRUE){
            stops <- quanteda::stopwords("english")
            `%notin%` = function(x,y) !(x %in% y)
            word.t <- word.t %>% dplyr::filter(., word %notin% stops)
      }
      # Get locations of node
      node_loc <- which(word.t == node1)
      
      # Stem the document
      if(stem == TRUE){
      word.t <- word.t %>% mutate(word = SnowballC::wordStem(word))
      }
      
      # If there are no matches, just return a record that the node doesn't occur and issue a warning
      if(isTRUE(length(node_loc) == 0)){
            # Print a warning
            if(length(names(document)) == 0){warning("The node does not occur in this document. Returned NA")
            } else {
                  warning(paste("The node does not occur in the document \"",
                                names(document),
                                "\". Returned NA",
                                sep = ""))
            }
            # # Return a collDB full of NAs
            collocate_locs <- return_empty_colldb()
      } else {
# Isolate locations to left and right (could be more efficient, but might be useful in future
# for isolating left and right collocates)
            left_locs <- lapply(node_loc, function(x)
                  ((x-window):(x-1)))
            right_locs <- lapply(node_loc,
                                 function(x)
                                       ((x+1):(x+window)))
            # convert any node_locs into NA
            left_locs <- lapply(left_locs,
                        function(x)
                              unlist(sapply(x,
                                            function(a)
                                                  ifelse(a %in% node_loc, yes = a <- NA, no = a <- a))))

            right_locs <- lapply(right_locs,
                        function(x)
                              unlist(sapply(x,
                                          function(a)
                                                ifelse(a %in% node_loc, yes = a <- NA, no = a <- a))))

            # Now create a list of word sequences (useful for calculating multigrams)
            all_locs <- mins_to_maxs(list(left_locs, right_locs, node, node1, length(node_loc), word.t))
            collocate_locs <- list(left_locs, right_locs, node, node1, length(node_loc), word.t, all_locs)

            names(collocate_locs) <- c("left_locs", "right_locs", "node", "node_hash", "node_recurrence", "doc_table", "all_locs")

            collocate_locs <- as(object = collocate_locs, Class = "collDB")
      }} # closing brackets for two tests above (where the document is length 0 and where node_loc is length 0)



      return(collocate_locs)
}
