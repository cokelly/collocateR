#' Calculates a pointwise mutual information score for ngrams in a corpus: log2(prob(xy)/prob(x)prob(y))
#'
#' @param vect A vector of text files or a corpus
#' @param keyword A keyword to feed to the kwic vector
#' @param window The width of the kwic vector
#' @param ngram The ngram types
#' @param cutoff A floor for infrequent terms
#' @param parallel TOkenize using parallel package
#' @importFrom quanteda corpus kwic is.corpus tokenize ngrams wordstem
#' @import data.table
#' @import parallel
#' @keywords kwic corpus mutual information
#' @export

pmi_score <- function(vect, keyword, window = 5, ngram = 1, cutoff = 3, parallel = TRUE){
      # Test to see if the vect is a corpus
      if(!isTRUE(is.corpus(vect))){vect.corp <- corpus(vect)}
      # Create parallel cores if required
      if(isTRUE(parallel)){
            ncores <- detectCores() -1
            cl <- makeCluster(ncores)
      }
      # Tokenize with ngrams
      # A make tokens function from https://rstudio-pubs-static.s3.amazonaws.com/169109_dcd8434e77bb43da8cf057971a010a56.html
      makeTokens <- function(input, ngram) {
            tokenize(input, what = "word", removeNumbers = TRUE,
                     removePunct = TRUE, removeSeparators = TRUE,
                     removeTwitter = TRUE, removeHyphens = FALSE,
                     n = ngram, simplify = TRUE, concatenator = " ")
      }
      print("Tokenizing the corpus: this might take a long time")
            vect_phrases <- makeTokens(vect.corp, ngram)
print("Finished tokenizing the corpus")
      # Create a data frame with ngram frequency for each phrase
      vectdf <- as.data.table(table(unlist(vect_phrases)), row.names = NULL) 
      colnames(vectdf) <- c("phrase", "doc_freq")
      # Process to create a column of phrase lengths
      phrases <- vectdf$phrase
      if(isTRUE(parallel)){
      phrase_lengths <- unlist(parLapply(cl, phrases, function(x) length(unlist(strsplit(x, " ")))))
      } else {
            phrase_lengths <- unlist(lapply(phrases, function(x) length(unlist(split(x, " ")))))
      }
      vectdf <- cbind(phrase_lengths, vectdf)
      # Split the data table into a list by each length
      vectdf_list <- split(vectdf, vectdf$phrase_lengths)
      # A column with the probability for each word or phrase in the context of phrase length
      word_probs <- lapply(vectdf_list, function(x) x$doc_freq/(sum(x$doc_freq)))
      # Cbind along both lists
      vectdf_list <- lapply(seq_along(1:length(vectdf_list)), function(x) cbind(vectdf_list[[x]], word_probs[[x]]))
      # Bring into a single data table and set key
      vectdf <- do.call("rbind", vectdf_list)
      colnames(vectdf) <- c("phrase_length", "phrase", "doc_freq", "word_prob")
      setkey(vectdf, phrase)
      # Create a similar dataframe for the collocations so I can narrow to relevant words and phrases
      generate_kwics <- function(vect, keyword, window){
            if(length(vect == 1)){
                  vect <- unlist(strsplit(vect, " "))
            }
            keyword_locs <- which(vect == "rights")
            lower_bound <- keyword_locs-window
            lower_bound <- ifelse(lower_bound < 1, NA, lower_bound)
            upper_bound <- keyword_locs+window
            upper_bound <- ifelse(upper_bound > length(vect), NA, upper_bound)
            ranges_list <- lapply(seq_along(1:length(lower_bound)), function(x) lower_bound[x]:upper_bound[x])
            ranges <- unique(unlist(ranges_list))
            kwic_collocates <- vect[ranges]
            return(kwic_collocates)
      }
         kwic_corp0 <- generate_kwics(vect, keyword, window)   
         kwic_corp0 <- unlist(kwic_corp0)
      kwic_corp0 <- paste(kwic_corp0, collapse = " ")
      # keyword_locs <- grep(keyword, vect)
      # }
      # kwic_corp0 <- kwic_to_vect(vect, keywords = keyword, window = window)
      kwic_corp <- corpus(kwic_corp0)
      # Tokenize
      kwic_phrases <- makeTokens(kwic_corp, ngram)
      # Create data table with frequencies
      kwicdf <- as.data.table(table(unlist(kwic_phrases)), row.names = NULL)
      colnames(kwicdf) <- c("phrase", "kwic_freq")
      
      # Process to create a column of phrase lengths
      kwic_phrase_col <- kwicdf$phrase
      if(isTRUE(parallel)){
            kwic_phrase_lengths <- unlist(parLapply(cl, kwic_phrase_col, function(x) length(unlist(strsplit(x, " ")))))
      } else {
            kwic_phrase_lengths <- unlist(lapply(kwic_phrases_col, function(x) length(unlist(strsplit(x, " ")))))
      }
      kwicdf <- cbind(kwic_phrase_lengths, kwicdf)
      # Split the data table into a list by each length
      kwicdf_list <- split(kwicdf, kwicdf$kwic_phrase_lengths)
      # A column with the probability for each word or phrase in the context of phrase length
      kwic_probs <- lapply(kwicdf_list, function(x) x$kwic_freq/(sum(x$kwic_freq)))
      # Cbind along both lists
      kwicdf_list <- lapply(seq_along(1:length(kwicdf_list)), function(x) cbind(kwicdf_list[[x]], kwic_probs[[x]]))
      # Bring into a single data table and set the key
      kwicdf <- do.call("rbind", kwicdf_list)
      colnames(kwicdf) <- c("phrase_length", "phrase", "kwic_freq", "kwic_prob")
      setkey(kwicdf, phrase)
      # Merge both dfs
      overalldf0 <- vectdf[.(kwicdf)]
      overalldf <- as.data.table(list(overalldf0$phrase, as.numeric(overalldf0$doc_freq), as.numeric(overalldf0$kwic_freq), as.numeric(overalldf0$word_prob), as.numeric(overalldf0$kwic_prob)))
      colnames(overalldf) <- c("phrase", "word_freq", "kwic_freq", "word_prob", "kwic_prob")
      setkey(overalldf, phrase)
      # retain the keyword probability
      keyword_probs <- overalldf[which(overalldf$phrase == keyword),]
      # #remove words and phrases that fall below the cutoff
      floor <- which(overalldf$kwic_freq < cutoff)
      overalldf2 <- overalldf[-floor,]
      
      # Because of overlaps, some words are reported as appearing 
      
      # Mutual information measure = log2(prob(xy)/prob(x)prob(y))
      #Calculate prob(xy)
      # kwic_probs_abridged <- overalldf2$kwic_freq
      probxy <- overalldf2$kwic_prob
      # probxy <- kwic_probs_abridged/keyword_probs$kwic_prob
      # #Calculate prob(x)prob(y)
      doc_probs_abridged <- overalldf2$word_prob
      probxproby <- doc_probs_abridged*keyword_probs$word_prob
      #calculate mutual information
      pmi <- log2(probxy/probxproby)
      # # # Add to df
      overalldf2 <- cbind(overalldf2, pmi)
      overalldf2 <- overalldf2[order(overalldf2$pmi, overalldf2$kwic_freq, decreasing = TRUE),]
      # Stop the cluster
      if(isTRUE(parallel)){stopCluster(cl)}
      return(overalldf2)
}