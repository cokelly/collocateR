#' Generates ngrams and delivers pmi scores by working from kwic file back to the full corpus (more efficient than generating large ngrams across the whole corpus)
#'
#' @param vect A vector of text files or a corpus
#' @param keyword A keyword to feed to the kwic vector
#' @param ngram The ngram types
#' @param window The width of the kwic vector
#' @param cutoff Only collocation frequencies above the cutoff will be included
#' @param cached_tokens Deliver a tokenized vector for the whole corpus (good for repeating analyses)
#' @importFrom quanteda tokenize wordstem
#' @import data.table
#' @importFrom stringr str_replace_all
#' @keywords kwic corpus mutual information
#' @export


# A simple function for processing each element and returning a kwic, an ngram and a pmi table

npmi_multigrams <- function(vect, keyword, ngram = 1:2, window = 5, cutoff = 3, stem = FALSE, cached_tokens = NULL){
      if(stem == TRUE){
            vect0 <- unlist(strsplit(vect, " "))
            vect <- wordstem(tokenize(vect0))
            x <- which(vect == "character(0)")
            if(length(x) > 0){
            vect <- vect[-x]
            }
            vect <- paste(vect, sep = " ", collapse = " ")
            keyword <- wordstem(tokenize(keyword))
      }
      test1 <- grep(keyword, vect)
      if((length(test1) == 0) == TRUE){
            counts <- "No keyword was found"
      } else {
      # remove carriage returns and hyphens
      vect <- str_replace_all(vect, "[\n]", " ")
      tidy_vect <- unlist(strsplit(vect, " "))
      x <- which(tidy_vect == "")
      if(length(x) > 0){
            tidy_vect <- tidy_vect[-x]
      }
      wordcount <- length(tidy_vect)
      # Turn vect into a single document to be processed
      vect <- paste(tidy_vect, sep = " ", collapse = " ")
      process_vect <- function(vect, keyword, ngram, window){
            #isolate the keywords in context
            get_kwics <- simple_kwics(vect, keyword = keyword, window = window)
            if(length(get_kwics) != 0){
                  print("kwics acquired")
                  get_kwics_simplified <- sapply(get_kwics, function(x) paste(x, sep = " ", collapse = " "))
                  kwics_tokenized0 <- sapply(get_kwics_simplified, function(x) tokenize(x, ngrams = ngram, concatenator = " ", removePunct = TRUE, removeNumbers = TRUE))
                  kwics_tokenized0 <- as.vector(do.call("c", kwics_tokenized0))
            } else {
                  print("No keywords found")
                  kwics_tokenized0 <- NULL
            }
            
            return(kwics_tokenized0)
      }
      
      kwics_tokenized_list <- lapply(vect, function(x) process_vect(x, keyword, ngram, window))
      
      kwics_tokenized <- do.call("c", kwics_tokenized_list)
      #kwics_tokenized_for_reference <- unlist(strsplit(paste(kwics_tokenized, sep = " ", collapse = " "), " "))
      print("kwics tokenized")
      counts <- data.table(table(kwics_tokenized))
      colnames(counts) <- c("phrase", "count.xy")
      # Remove any terms that fall below the cutoff
      floor <- which(as.numeric(counts$count.xy) >= cutoff)
      counts <- counts[floor]
      if((nrow(counts) == 0) == TRUE){ # Test that the cutoff didn't remove all elements
            counts <- "No keywords with frequencies above the cutoff were found"
      } else {
      # Only preserve unique phrases in the original vector (for speed not elegance)
      phrases <- counts$phrase
      if(is.null(cached_tokens) == TRUE){
      not_to_strip <- unique(unlist(strsplit(paste(phrases, split = " ", collapse = " "), " ")))
      not_to_strip <- not_to_strip[-(which(not_to_strip == ""))]
      vect2 <- unlist(strsplit(vect, " "))
      to_keep <- which(vect2 %in% not_to_strip)
      vect_gutted <- vect2[to_keep]
      vect_gutted <- paste(vect_gutted, sep = " ", collapse = " ")
      print("irrelevant words removed from original vectors")
      # Count them (the only way I know how but at least now much faster)
      print("tokenising vectors: this might take a long time")
      vect_tokens <- tokenize(vect_gutted, ngrams = ngram, concatenator = " ", removePunct = TRUE, removeNumbers = TRUE)
            print("vector tokenised")
      } else {
      vect_tokens <- cached_tokens
      print("table-izing cached tokens: this may take a while.")
}
      vect_tokens <- as.vector(do.call("c", vect_tokens))
      tokeep <- which(vect_tokens %in% phrases)
      vect_tokens <- vect_tokens[tokeep]
      vect_counts0 <- table(vect_tokens)
      vect_counts <- data.table(vect_counts0)
      print("done generating a counts table")
      colnames(vect_counts) <- c("phrase", "count.x")
      # Get the wordcount controlling for phrase length
      phrase_lengths <- sapply(phrases, function(x)
            length(unlist(strsplit(x, " "))))
      word_phrase_count <- wordcount/phrase_lengths
      # Get the count.y - so the occurences of the keyword
      keyword_counts <- counts[which(counts == keyword),]
      # Include count.x from above
      counts <- data.table(cbind(word_phrase_count, counts$phrase, vect_counts$count.x, as.numeric(keyword_counts$count.xy), as.numeric(counts$count.xy)))
      print("calculating pmi/npmi")
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
      if((ncol(counts) == 7) == TRUE){
      colnames(counts) <- c("phrase.count", "phrase", "count.x", "count.y", "count.xy", "pmi", "npmi")
      counts <- counts[order(counts$npmi, counts$count.xy, decreasing = TRUE),]}
      }} # Square brackets from the various tests
           return(counts)      
}