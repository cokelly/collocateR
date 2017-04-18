#' A generic function for making a dfm
#'
#' @param input A vector or a corpus
#' @param ngram The ngram types
#' @importFrom quanteda tokenize dfm
#' @keywords kwic corpus mutual information
#' @export

# I tokenise the corpus, then derive a wordcount, plus counts and
# probabilities for word x and keyword y and for word x,y: x-as-member-of-class-y (the keyword in
# context window). PMI = log(p(x,y)/p(x)p(y)) For NPMI, divide by p(x)p(y).
#######################

# A function for creating a document frequency matrix (from quanteda)
make_dfm <- function(input, ngram) {
      input2 <- tokenize(input, removeNumbers = TRUE,
                         removePunct = TRUE,
                         removeSeparators = TRUE,
                         verbose = TRUE,
                         removeTwitter = TRUE,
                         ngrams = ngram,
                         concatenator = " ")
      input2 <- dfm(input2)
      return(input2)
}