#' returns a wordscore dataframe of collocates from a quanteda corpus file
#'
#' @param vector A vector of text files
#' @param keyword A keyword to feed to the kwic vector
#' @param window The width of the kwic vector
#' @importFrom qdapRegex rm_white
#' @importFrom quanteda corpus kwic dfm tfidf
#' @keywords tidy
#' @export

kwic_scores <- function(corpus, keywords, window = 5){
      # generate the kwic file then process back into a quanteda corpus
      kwicfile <- kwic(corpus, keywords, window)
      collocates <- kwicfile[,c(3,5)]
      collocates <- lapply(seq_along(1:nrow(kwicfile)), function(x) paste(collocates[x,], sep = "", collapse = " "))
      collocates <- lapply(collocates, rm_white)
      collocates <- unlist(collocates)
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