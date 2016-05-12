#' Calculates a pointwise mutual information score for ngrams in a corpus: log2(prob(xy)/prob(x)prob(y))
#'
#' @param vect A vector of text files or a corpus
#' @param keyword A keyword to feed to the kwic vector
#' @param window The width of the kwic vector
#' @param ngram The ngram types
#' @param cutoff A floor for infrequent terms
#' @param parallel TOkenize using parallel package
#' @importFrom quanteda corpus kwic is.corpus tokenize ngrams
#' @import doParallel
#' @import parallel
#' @keywords kwic corpus mutual information
#' @export

pmi_score <- function(vect, keyword, window = 5, ngram = 1:4, cutoff = 3, parallel = TRUE){
      # Test to see if the vect is a corpus
      classtest <- class(vect)
      if(is.corpus(vect) == FALSE){vect <- corpus(vect)}
      
      # Tokenize with ngrams
      
      # Generic function for parallelizing any task (when possible) from https://rstudio-pubs-static.s3.amazonaws.com/169109_dcd8434e77bb43da8cf057971a010a56.html
      parallelizeTask <- function(task, ...) {
            # Calculate the number of cores
            ncores <- detectCores() - 1
            # Initiate cluster
            cl <- makeCluster(ncores)
            registerDoParallel(cl)
            #print("Starting task")
            r <- task(...)
            #print("Task done")
            stopCluster(cl)
            r
      }
      
      # A make tokens function from https://rstudio-pubs-static.s3.amazonaws.com/169109_dcd8434e77bb43da8cf057971a010a56.html
      makeTokens <- function(input, ngram) {
            tokenize(input, what = "word", removeNumbers = TRUE,
                     removePunct = TRUE, removeSeparators = TRUE,
                     removeTwitter = TRUE, removeHyphens = FALSE,
                     ngram, simplify = TRUE, concatenator = " ")
      }
      if(parallel == TRUE){
            vect_phrases <- parallelizeTask(makeTokens, vect, ngram)
      } else {
      vect_phrases <- tokenize(vect, ngrams = ngram, concatenator = " ", removeSymbols = TRUE, removeNumbers = TRUE, simplify = TRUE)
      }
      
      # Create a data frame with ngram frequency for each phrase
      vectdf <- data.frame(table(unlist(vect_phrases)), row.names = NULL) 
      # A column with the probability for each word
      word_probs <- vectdf[,2]/sum(vectdf[,2])
      vectdf <- cbind(vectdf, word_probs)
      colnames(vectdf) <- c("phrase", "doc_freq", "word_prob")
      # Create a similar dataframe for the collocations so I can narrow to relevant words and phrases
      kwic_corp0 <- kwic_to_vect(vect, keywords = keyword, window = window)
      kwic_corp <- corpus(kwic_corp0)
      kwic_phrases <- tokenize(kwic_corp, ngrams = ngram, concatenator = " ", removeSymbols = TRUE, removeNumbers = TRUE)
      kwicdf <- data.frame(table(unlist(kwic_phrases)), row.names = NULL)
      kwic_probs <- kwicdf[,2]/sum(kwicdf[,2])
      kwicdf <- cbind(kwicdf, kwic_probs)
      colnames(kwicdf) <- c("phrase", "kwic_freq", "kwic_prob")
      # Merge both dfs
      
      ## THERE IS SOMETHING WRONG WITH THE BELOW THAT IS FUCKING EVERYTHING UP.
      
      overalldf <- merge(vectdf, kwicdf, by = "phrase", all.x = FALSE, all.y = TRUE)
      # retain the keyword probability
      keyword_probs <- overalldf[which(overalldf[,1] == keyword),]
      #remove words and phrases that fall below the cutoff
      floor <- which(overalldf$kwic_freq < cutoff)
      overalldf2 <- overalldf[-floor,]
      # Mutual information measure = log2(prob(xy)/prob(x)prob(y))
      #Calculate prob(xy)
      kwic_probs_abridged <- overalldf$kwic_prob
      probxy <- kwic_probs_abridged/keyword_probs$kwic_prob
      #Calculate prob(x)prob(y)
      doc_probs_abridged <- overalldf$word_prob
      probxproby <- doc_probs_abridged*keyword_probs$word_prob
      #calculate mutual information
      pmi <- log2(probxy/probxproby)
      # Add to df
      overalldf <- data.frame(cbind(overalldf, pmi))
      #colnames(overalldf) <- c("word/phrase", "doc_freq", "kwic_freq", "pmi")
      overalldf <- overalldf[order(overalldf$pmi, decreasing = TRUE),]
      return(overalldf)
}