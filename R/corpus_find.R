#' A small function using tm to find a keyword in a corpus
#'
#' Returns a new corpus composed of all the documents in the original corpus that contain a keyword
#' @param corpus A corpus
#' @param keyword A search term
#' @import tm
#' @keywords search tm
#' @export

corpus_find <- function(corpus, keyword){
      doc_freq <- tm_filter(corpus, FUN = function(x) any(grep(keyword, content(x))))
      print(paste("The word '", keyword, "' was found in ", length(doc_freq), " documents.", sep = ""))
      return(doc_freq)
}