#' Compare two sets of keyword in context matrices
#'
#' Takes a 'kwic' matrix, plus two nominated subsetting vecors, returns a list of 2 scores for significant words, sorted by frequency or through a 'term frequency-inverse document frequency' ('tf-idf') that compares normalised tf in the kwics to idf for the corpus as a whole.
#' @param corpus A tm corpus (tidied)
#' @param kwic_list A list of kwic matrices produced using the 'kwic' function in collocateR
#' @param item_selection1 First subset to be compared
#' @param item_selection2 Second subset to be compared
#' @param weighting For sorting. 'raw' = raw frequency; 'normalised' = Normalised frequency (within the KWIC selection; 'tfidf' = Term Frequency-Inverse Docmuent Frequency
#' @param remove_stops Remove stopwords
#' @param tidy Tidy the corpus
#' @import tm
#' @keywords collocates concordance
#' @export
#' @examples
#' library(tm)
#' data("crude")
#' oilkwic <- kwic(crude, "oil", collocation_width = 6, tidy = TRUE)
#' compare_kwics(crude, oilkwic, 1:10, 11:20, weighting = "normal", remove_stops = FALSE, tidy = FALSE)
#' 

# A function to distinguish between two parts of a kwic list

compare_kwics <- function(original_corpus, kwic_list, item_selection1, item_selection2, weighting = "raw", remove_stops = FALSE, tidy = FALSE){
	
      # Rough test that the kwic_list is a kwic_list
a <- sapply(kwic_list, class)
b <- which(a == "matrix")
if((length(b) == 0) == TRUE){print("The kwic list is empty")}
c <- b[1]
d <- b[2]
if(identical(
      kwic_list[[c]][1,((ncol(kwic_list[[c]])/2)+1)],
      kwic_list[[d]][1,((ncol(kwic_list[[d]])/2)+1)])
   == FALSE){
print("Are you sure you have inputted a kwic list?")
}
      # Test for weighting
if(weighting != "raw" &
  weighting != "normalised" &
  weighting != "normalized" &
  weighting != "tfidf"){print("weighting must either be 'raw' or 'normalised' or 'tfidf'")}
      
keyword <- kwic_list[[c]][1,((ncol(kwic_list[[c]])/2)+1)]
      
if(tidy == TRUE){
      original_corpus <- tidy(original_corpus)
}

      # Get original corpus Details
corpus_length <- length(original_corpus)
corpus_names <- names(original_corpus)
      # Calculate idf
doc_freq <- length(tm_filter(original_corpus, FUN = function(x)
      any(grep(keyword, content(x)))))
      idf <- as.matrix(log(corpus_length/doc_freq))
      
      # Split the list
kwic1 <- kwic_list[item_selection1]
kwic2 <- kwic_list[item_selection2]

	# Convert into corpuses
corpus1 <- Corpus(VectorSource(kwic1))
corpus2 <- Corpus(VectorSource(kwic2))
	
	# Remove stopwords if necessary
if(remove_stops == TRUE){
      corpus1 <- tm_map(corpus1, removeWords, stopwords("english"))
	corpus2 <- tm_map(corpus2, removeWords, stopwords("english"))
}
	# Create TDMs
tdm1 <- TermDocumentMatrix(corpus1)
tdm2 <- TermDocumentMatrix(corpus2)
tdm1 <- as.data.frame(as.matrix(tdm1))
tdm1rownames <- rownames(tdm1)
tdm2 <- as.data.frame(as.matrix(tdm2))
tdm2rownames <- rownames(tdm2)
	
	# If normal
if(weighting == "raw"){
      counts1 <- as.matrix(rowSums(tdm1))
      counts2 <- as.matrix(rowSums(tdm2))
	
      counts1 <- as.matrix(counts1[
            order(counts1[,1], 
                  decreasing = TRUE),])
      counts2 <- as.matrix(counts2[
            order(counts2[,1], 
                  decreasing = TRUE),])
return(list(rawcounts1, rawcounts2))}

# If tfidf
if(weighting == "tfidf"){
	wordcount1 <- colSums(tdm1)
	wordcount2 <- colSums(tdm2)
	
	tdm1_list <- as.list(tdm1)
	tdm1_list <- lapply(seq_along(1:length(tdm1_list)), function(x) 
	      tdm1_list[[x]]/wordcount1[x])
	tdm1 <- as.matrix(
	      do.call(cbind.data.frame, tdm1_list))
	counts1 <- as.matrix(
	      rowSums(tdm1))
	
	tdm2_list <- as.list(tdm2)
	tdm2_list <- lapply(seq_along(1:length(tdm2_list)), function(x)
	      tdm2_list[[x]]/wordcount2[x])
	tdm2 <- as.matrix(do.call(
	      cbind.data.frame, tdm2_list))
	counts2 <- as.matrix(
	      rowSums(tdm2))
	
	idf_counts1 <- cbind(
	      counts1, idf[which(rownames(idf) %in% rownames(tdm1)),])
	idf_counts1 <- as.matrix(
	      apply(idf_counts1, 1, prod))
	
	idf_counts2 <- cbind(
	      counts2, idf[which(rownames(idf) %in% rownames(tdm2)),])
	idf_counts2 <- as.matrix(
	      apply(idf_counts2, 1, prod))
	rownames(idf_counts1) <- tdm1rownames
	idf_counts1_sorted <- as.matrix(idf_counts1[
	      order(idf_counts1[,1],
	            decreasing = TRUE),])
	rownames(idf_counts2) <- tdm2rownames
	idf_counts2_sorted <- as.matrix(idf_counts2[
	      order(idf_counts2[,1],
	            decreasing = TRUE),])
return(list(idf_counts1_sorted, idf_counts2_sorted))}

if(weighting == "normalised" | weighting == 'normalized'){
      counts1 <- as.matrix(rowSums(tdm1))
      counts2 <- as.matrix(rowSums(tdm2))
      counts1 <- as.matrix(counts1/sum(counts1[,1]))
      counts2 <- as.matrix(counts2/sum(counts2[,1]))
      normalisedcounts1 <- as.matrix(counts1[order(counts1[,1], decreasing = TRUE),])
      normalisedcounts2 <- as.matrix(counts2[order(counts2[,1], decreasing = TRUE),])
      
      return(list(normalisedcounts1, normalisedcounts2))}
}