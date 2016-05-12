#' returns a dfm from a kwic file
#'
#' @param vector A vector of text files
#' @param keyword A keyword to feed to the kwic vector
#' @param window The width of the kwic vector
#' @importFrom qdapRegex rm_white
#' @importFrom quanteda corpus kwic tfidf
#' @keywords kwic corpus

 
kwic_to_corpus <- function(corpus, keywords, window){
# generate the kwic file then process back into a quanteda corpus
kwicfile <- kwic(corpus, keywords, window)
collocates <- kwicfile[,3:5]
collocates <- lapply(seq_along(1:nrow(kwicfile)), function(x) paste(collocates[x,], sep = "", collapse = " "))
collocates <- lapply(collocates, rm_white)
collocates <- unlist(collocates)
collocates <- corpus(collocates)
return(collocates)
}