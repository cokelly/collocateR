remove_empty_lines <- function(corpus) {
      # remove any empty lines from the document
      
      char_vec_split <- strsplit(corpus, split = " ")  # split on spaces (so, each word as an element)
      char_vec_split <- unlist(char_vec_split)  # unlist
      populatedlines <- which(char_vec_split != "")  # remove empty lines
      char_vec_split <- char_vec_split[populatedlines]
      corpus_return <- paste0(char_vec_split, collapse = " ")
      return(corpus_return)
}