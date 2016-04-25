#' returns a quanteda corpus of collocates from a quanteda kwic file
#'
#' @param vector A vector of text files
#' @importFrom qdapRegex rm_white
#' @importFrom quanteda corpus
#' @keywords tidy
#' @export

collocates_corpus <- function(kwic){
      collocates <- kwic[,3:5]
      collocates <- lapply(seq_along(1:nrow(kwic)), function(x) paste(collocates[x,], sep = "", collapse = " "))
      collocates <- lapply(collocates, rm_white)
      collocates <- unlist(collocates)
      collocates <- corpus(collocates)
      return(collocates)
}