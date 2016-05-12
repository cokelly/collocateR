#' Calculates a pointwise mutual information score for ngrams in a corpus: log2(prob(xy)/prob(x)prob(y))
#'
#' @param vect A vector of text files or a corpus
#' @param keyword A keyword to feed to the kwic vector
#' @param window The width of the kwic vector
#' @param ngram The ngram types
#' @param cutoff A floor for infrequent terms
#' @importFrom quanteda corpus kwic is.corpus tokenize ngrams
#' @keywords kwic corpus mutual information
#' @export

pmi_score <- function(vect, keyword, window = 5, ngram = 1:4, cutoff = 3){
      # Test to see if the vect is a corpus
      classtest <- class(vect)
      if(is.corpus(vect) == FALSE){vect <- corpus(vect)}
      # Tokenize with negrams
      vect_phrases <- tokenize(vect, ngrams = ngram, concatenator = " ", removeSymbols = TRUE, removeNumbers = TRUE)
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
      
      overalldf <- merge(kwicdf, vectdf, by = "phrase", all.x = TRUE, all.y = TRUE)
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
      # #overalldf <- overalldf[order(overalldf$pmi, decreasing = TRUE),]
 return(overalldf)
}