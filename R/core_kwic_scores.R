#' returns a wordscore dataframe of collocates from a quanteda corpus file
#'
#' @param vector A vector of text files
#' @param keyword A keyword to feed to the kwic vector
#' @param window The width of the kwic vector
#' @importFrom quanteda dfm tfidf 
#' @keywords tidy
#' @export

kwic_scores <- function(corpus, keywords, window = 5){
      collocates <- simple_kwics(corpus, keywords, window)
      collocates <- corpus(collocates)
      # create a dfm sparse matrix
      collocates.dfm <- dfm(collocates)
      collocates.tfidf <- tfidf(collocates.dfm)
      # get colsums
      scoresums <- colSums(as.matrix(collocates.dfm))
      scoresums_tfidf <- colSums(as.matrix(collocates.tfidf))
      scoresums1 <- data.frame(word = names(scoresums), count = scoresums, tfidf = scoresums_tfidf)
      scoresums2 <- scoresums1[order(scoresums1$count, decreasing = TRUE),]
      return(scoresums2)
}