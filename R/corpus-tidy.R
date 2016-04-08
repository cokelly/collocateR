#' Uses tm to tidy a corpus
#'
#' Addresses words that are less than a collocation_width away from the document's end
#' @param corpus A corpus that has not been processed
#' @import stringr
#' @import tm
#' @keywords collocates concordance
#' @export

tidy <- function(corpus){
      
      vectors <- sapply(corpus, "content")
      
      vectors <- lapply(vectors, function(x) as.matrix(unlist(strsplit(x, " "))))
      
      # To lower
      vectors <- lapply(vectors, function(x) as.matrix(tolower(x)))
      
      # Remove all punctuation
      vectors <- lapply(vectors, function(x) str_replace_all(x,"[[:punct:]]",""))
      
      remove_empty_lines <- function(vector){
            vector <- as.matrix(vector)
            empty_lines <- which(vector == "") # get rid of empty lines
            vector <- as.matrix(vector[-empty_lines,])
            vector <- as.character(vector)
            
            return(vector)
      }
      
      vectors <- lapply(vectors, function(x) remove_empty_lines(x))
      
      vectors <- lapply(vectors, function(x) paste(x, sep = "", collapse = " "))
      
      corpus_new <- Corpus(VectorSource(vectors), readerControl = list(language = "en"))
      
      return(corpus_new)
}
