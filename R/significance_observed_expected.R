#' A observed/expected word frequency function
#'
#' Takes a tm corpus and a KWIC matrix as inputs, returns a list of matrices containing observed/expected ratios for each word in the KWIC matrix
#' @param origcorpus A collocation matrix
#' @param tidy Tidy the corpus - remove punctuation, numbers, empty lines (optional) and whitespace, convert to lowercase
#' @keywords collocates concordance observed/expected
#' @export
#' @examples
#' library(tm)
#' data("crude")
#' oil_kwic <- collocate(crude,"oil", collocation_width = 6, tidy = TRUE)
#' observed_expected(crude, oil_kwic)

observed_expected <- function(origcorpus, collocates, tidy = FALSE){

###########################
      # Tests
###########################

      #Tests to ensure that the corpus and collocates matrix are actually a corpus and a collocates matrix
      # Test that origcorpus's class is a corpus
      corpus_class <- class(origcorpus)
      if(is.null(grep("Corpus", corpus_class))){
            print("You must use a 'tm' corpus for this function to work")}

      # Test that all matrices have the same number of columns and the same word running down the middle column

      if(length(unique(unlist(lapply(collocates, ncol)))) != 1){
            print("Each matrix in the collocates list must have the same number of columns")}

# Save the keyword for testing and for later use in colnames

      collocation_width2 <- ncol(collocates[[1]])
      middle_col <- (collocation_width2/2)+.5
      keyword2 <- collocates[[1]][1,middle_col]

# Test that the central word in the collocation matrices is always the same
      if(length(unique(unlist(lapply(collocates, function(x) x[,middle_col])))) != 1){
            print("The central column in each collocation matrix ought to contain the same word")
      }

# (Not fully reliable) test that the original corpus has been tidied (by searching for punctuation)

# I need to find a grep code for all punctuation


###########################
      # Tidy the original corpus if required
###########################

      if(tidy == TRUE){
            origcorpus <- tidy(origcorpus)
      }

###########################
      # Get a tdm for the original corpus
###########################
      tdm_orig <- as.matrix(TermDocumentMatrix(origcorpus))
      wordcounts_orig <- colSums(tdm_orig)

###########################
      # Get a tdm for the collocate matrix
###########################

      corpus_collocates <- Corpus(VectorSource(collocates), readerControl = list(language = "en"))
      names_colls <- names(collocates)
      tdm_colls <- as.matrix(TermDocumentMatrix(corpus_collocates))
      wordcount_colls <- colSums(tdm_colls)

###########################
      # Process Collocations Matrix
###########################

# Turn each column in the collocates matrix into a list element
      list_tdm_colls <- lapply(seq_len(ncol(tdm_colls)), function(x) as.matrix(tdm_colls[,x]))
      names(list_tdm_colls) <- names_colls

# A function to remove zero score list elements
      remove_zeros <- function(list_element){
            nonzeros <- which(list_element != 0)
            list_element2 <- list_element[nonzeros,]
            return(list_element2)
      }

      list_tdm_colls <- lapply(list_tdm_colls, function(x) as.matrix(remove_zeros(x)))

# Cbind to add percentages for the collocates and rownames as a column
      #list_tdm_colls <- lapply(list_tdm_colls, function(x) cbind(x, apply(x, 1, function(y) y/colSums(x)*100)))
      list_tdm_colls <- lapply(list_tdm_colls, function(x) (cbind(rownames(x), x)))


###########################
      # Process relevant parts of original Corpus
###########################

# Isolate the words that match each element in the collocates tdm

find_matches <- function(list_element){
      matches_across <- rownames(tdm_orig) %in% list_element
      matches_across <- tdm_orig[matches_across,]
      return(matches_across)
}

matches_across <- lapply(list_tdm_colls, function(x) find_matches(x))

# The function above returns a full matrix from the original: that is, a column for each document. So I need to isolate the relevant column for each before cbinding them to list_tdm_colls

list_matches_across <- lapply(seq_along(1:length(matches_across)), function(x) as.matrix(matches_across[[x]][,x])) # So for instance I want column 1 from list element 1 and so on.

# Divide each word frequency by the overall wordcount then multipy by
list_matches_across <- lapply(seq_along(1:length(list_matches_across)), function(x) cbind(list_matches_across[[x]], (list_matches_across[[x]]/wordcounts_orig[x])*wordcount_colls[x]))

###########################
      # Cbind the full occurrences with expected freq
###########################

list_tdm_colls <- lapply(seq_along(1:length(list_tdm_colls)), function(x) cbind(list_tdm_colls[[x]], list_matches_across[[x]][,2]))

# Cbind with a calculation dividing observed freq by expected

list_tdm_colls <- lapply(list_tdm_colls, function(x) cbind(x, as.numeric(x[,2])/as.numeric(x[,3])))
# Remove all but the count and the association
list_tdm_colls <- lapply(list_tdm_colls, function(x) cbind(x[,1:2], x[,4]))
# Sort the lists
list_tdm_colls <- lapply(list_tdm_colls, function(x) x[order(as.numeric(x[,3]), decreasing = TRUE),])

# Assign colnames, sadly through a for loop

for(i in 1:length(list_tdm_colls)){
      colnames_colls <- c(paste(names_colls[i], keyword2, "collocates", sep="_"), "count", "observed/expected ratio")
      colnames(list_tdm_colls[[i]]) <- colnames_colls
}

return(list_tdm_colls)


}

