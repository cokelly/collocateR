#' Generates ngrams and delivers pmi scores by working from kwic file back to the full corpus (more efficient than generating large ngrams across the whole corpus)
#'
#' @param vect A vector of text files or a corpus
#' @param keyword A keyword to feed to the kwic vector
#' @param ngram The ngram types
#' @param window The width of the kwic vector
#' @importFrom quanteda tokenize
#' @import data.table
#' @importFrom stringr str_replace_all
#' @keywords kwic corpus mutual information
#' @export


# A simple function for processing each element and returning a kwic, an ngram and a pmi table

npmi_unigrams <- function(vect, keyword, ngram = 1:2, window = 5, cutoff = 3){
      # remove carriage returns and hyphens
      vect <- str_replace_all(vect, "[\n]", " ")
tidy_vect <- unlist(strsplit(vect, " "))
x <- which(tidy_vect == "")
if(length(x) > 0){
      tidy_vect <- tidy_vect[-x]
}
# Get a wordcount for the vector
wordcount <- length(tidy_vect)
# Turn vect into a single document to be processed
vect <- paste(tidy_vect, sep = " ", collapse = " ")
#isolate the keywords in context
get_kwics <- simple_kwics(vect, keyword, window)
get_kwics_simplified <- paste(get_kwics, sep = " ", collapse = " ")      
# Tokenise and count
kwics_tokenized0 <- tokenize(get_kwics, ngrams = ngram, concatenator = " ", removePunct = TRUE, removeNumbers = TRUE)
kwics_tokenized <- do.call("c", kwics_tokenized0)
counts <- data.table(table(kwics_tokenized))
colnames(counts) <- c("phrase", "count.xy")
# Remove any terms that fall below the cutoff
floor <- which(as.numeric(counts$count.xy) > cutoff)
counts <- counts[floor]
# Only preserve unique phrases in the original vector (for speed not elegance)
phrases <- counts$phrase
not_to_strip <- unique(unlist(strsplit(paste(phrases, split = " ", collapse = " "), " ")))
not_to_strip <- not_to_strip[-(which(not_to_strip == ""))]
vect2 <- unlist(strsplit(vect, " "))
to_keep <- which(vect2 %in% not_to_strip)
vect_gutted <- vect2[to_keep]
vect_gutted <- paste(vect_gutted, sep = " ", collapse = " ")
# Count them (the only way I know how but at least now much faster)
vect_tokens <- tokenize(vect_gutted, ngrams = ngram, concatenator = " ", removePunct = TRUE, removeNumbers = TRUE)
vect_counts <- data.table(table(vect_tokens))
colnames(vect_counts) <- c("phrase", "count.x")
to_retain <- which(vect_counts$phrase %in% counts$phrase)
vect_counts <- vect_counts[to_retain,]
# Get the wordcount controlling for phrase length
phrase_lengths <- sapply(phrases, function(x)
      length(unlist(strsplit(x, " "))))
word_phrase_count <- wordcount/phrase_lengths
# Get the count.y - so the occurences of the keyword
keyword_counts <- counts[which(counts == keyword),]
# Include count.x from above
counts <- data.table(cbind(word_phrase_count, counts$phrase, vect_counts$count.x, as.numeric(keyword_counts$count.xy), as.numeric(counts$count.xy)))
#Calculate pmi
get_pmi <- function(vect){
      probxy <- as.numeric(vect[5])/as.numeric(vect[1])
      probxproby <- ((as.numeric(vect[3])/as.numeric(vect[1]))*(as.numeric(vect[4])/as.numeric(vect[1])))
      pmi <- log(probxy/probxproby)
      npmi <- pmi/-log(as.numeric(probxy))
      pmi_npmi <- matrix(cbind(pmi, npmi), ncol = 2)
      return(pmi_npmi)
}
pmi_npmi <- apply(counts, 1, function(x) get_pmi(x))
pmi_npmi <- t(pmi_npmi)
counts <- data.table(cbind(counts, pmi_npmi))
colnames(counts) <- c("phrase.count", "phrase", "count.x", "count.y", "count.xy", "pmi", "npmi")
counts <- counts[order(counts$npmi, counts$count.xy, decreasing = TRUE),]
return(counts)      
}