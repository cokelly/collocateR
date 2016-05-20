#' Read a Quanteda corpus into R
#'
#' Read text files into R and tidy them.
#' @param directory A directory
#' @param tidy Tidy the corpus - remove all punctuation, numbers, empty lines and whitespace, convert to lowercase
#' @param remove_stopwords Remove all stopwords. For a list of stopwords input quanteda::stopwords("english")
#' @param as_corpus return as a quanteda corpus
#' @importFrom readr read_file
#' @import quanteda
#' @keywords import texts
#' @export
#' @examples
#' read_texts("~/data/", tidy=TRUE)

read_texts <- function(directory, tidy = TRUE, remove_stopwords = TRUE, as_corpus = FALSE){
      
      print("reading all texts...")
      filenames <- list.files(directory)
      files <- sapply(filenames, function(x) read_file(paste(directory, x, sep="")))
      
      #tidy
      if(tidy == TRUE){
            files <- tidy(files)
      }
      # remove stopwords
      if(remove_stopwords == TRUE){
            files <- remove_stops(files)
      }
      
      # convert to quanteda corpus
      if(as_corpus == TRUE){
      files <- corpus(files)
      }
      
      return(files)
}