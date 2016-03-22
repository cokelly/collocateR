#' Uses tm to tidy a corpus
#' 
#' Addresses words that are less than a collocation_width away from the document's end
#' @param corpus_raw A corpus that has not been processed
#' @keywords collocates concordance
#' @export

tidy <- function(corpus_raw) {
    
    corpus_tidied <- tm_map(corpus_raw, content_transformer(removePunctuation))
    corpus_tidied <- tm_map(corpus_tidied, content_transformer(tolower))
    corpus_tidied <- tm_map(corpus_tidied, content_transformer(removeNumbers))
    corpus_tidied <- tm_map(corpus_tidied, content_transformer(stripWhitespace))
    
    ######################### 
    
    remove_empty_lines <- function(corpus) {
        # remove any empty lines from the document
        
        char_vec_split <- strsplit(corpus, split = " ")  # split on spaces (so, each word as an element)
        char_vec_split <- unlist(char_vec_split)  # unlist
        populatedlines <- which(char_vec_split != "")  # remove empty lines
        char_vec_split <- char_vec_split[populatedlines]
        corpus_return <- paste0(char_vec_split, collapse = " ")
        return(corpus_return)
    }
    
    corpus_tidied <- tm_map(corpus_tidied, content_transformer(remove_empty_lines))
    
    ######################### 
    
    return(corpus_tidied)
} 
