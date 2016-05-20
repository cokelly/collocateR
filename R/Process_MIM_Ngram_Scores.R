#' Calculates a pointwise mutual information score for ngrams in a corpus: log2(prob(xy)/prob(x)prob(y))
#'
#' @param vect A vector of text files or a corpus
#' @param keyword A keyword to feed to the kwic vector
#' @param window The width of the kwic vector
#' @param ngram The ngram types
#' @param cutoff A floor for infrequent terms
#' @importFrom quanteda corpus kwic is.corpus tokenize ngrams wordstem
#' @import data.table
#' @keywords kwic corpus mutual information
#' @export

pmi_score <- function(vect, keyword, window = 5, ngram = 1, cutoff = 3, normalised = TRUE){
  
  # Make a dfm with ngrams
  
      make_dfm <- function(input, ngram) {
            dfm(input, removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE, verbose = TRUE, removeTwitter = TRUE, ngrams = ngram, concatenator = " ")
      }
      print("Processing the corpus: this might take a long time")
            vect_phrases <- make_dfm(vect, ngram)
      # Create a data table with ngram frequency for each phrase
            vect_sums <- colSums(vect_phrases)
            vectdf <- data.table(phrase = names(vect_sums), corpus_freq = vect_sums)
      print("Finished processing the corpus")
      # Process to create a column of phrase lengths
      phrases <- vectdf$phrase
      phrase_lengths <- sapply(phrases, function(x) length(unlist(strsplit(x, " "))))
      vectdf <- cbind(phrase_lengths, vectdf)
      # Split the data table into a list by each length
      vectdf_list <- split(vectdf, vectdf$phrase_lengths)
      # A column with the probability for each word or phrase in the context of phrase length
      word_probs <- lapply(vectdf_list, function(x) x$corpus_freq/(sum(x$corpus_freq)))
      # Cbind along both lists
      vectdf_list <- lapply(seq_along(1:length(vectdf_list)), function(x) cbind(vectdf_list[[x]], word_probs[[x]]))
      # Bring into a single data table and set key
      vectdf <- do.call("rbind", vectdf_list)
      colnames(vectdf) <- c("phrase_length", "phrase", "corpus_freq", "word_prob")
      setkey(vectdf, phrase)
      # Create a similar dataframe for the collocations so I can narrow to relevant words and phrases
      print("Generating scores for the keyword in context terms")
      #############
      #THE PROBLEM IS IN THE FUNCTION BELOW. IN COMBINING INTO SINGLE VECTORS I AM INTRODUCING BIGRAMS THAT DON'T EXIST. CHOICES: MANUALLY REMOVE LATER OR REWRITE THE FUNCTION
      ############
      generate_kwics <- function(vect0, keyword, window){
            if(length(vect0 == 1)){
                  vect0 <- unlist(strsplit(vect0, " "))
            }
            # Remove blank locates
            x <- which(vect0 == "")
            vect0 <- vect0[-x]
            # Get the keyword
            keyword_locs <- which(vect0 == keyword)
            # Get the lower and upper bounds to the window for each instance of the keyword
            lower_bound <- keyword_locs-window
            lower_bound <- ifelse(lower_bound < 1, NA, lower_bound)
            upper_bound <- keyword_locs+window
            upper_bound <- ifelse(upper_bound > length(vect0), NA, upper_bound)
            # create a list of ranges
            ranges_list <- lapply(seq_along(1:length(lower_bound)), function(x) lower_bound[x]:upper_bound[x])
            # Get rid of overlaps
            ranges_matrix0 <- do.call("rbind", ranges_list)
            # A function to find dups by comparing each row
            erase_dups <- function(vector1, vector2){
                  truefalse_vector <- vector1 %in% vector2
                  vector1[truefalse_vector == TRUE] <- NA
                  return(vector1)
            }
            # Where are the overlaps?
            ranges_matrix <- t(sapply(seq_along(1:(nrow(ranges_matrix0)-1)), function(x) erase_dups(ranges_matrix0[x,], ranges_matrix0[x+1,])))
            # Restore the final row
            ranges <- rbind(ranges_matrix, ranges_matrix0[max(nrow(ranges_matrix0)),])
            #Isolate the words
            kwic_collocates <- matrix(vect0[ranges], ncol = ncol(ranges))
            return(kwic_collocates)
      }
      kwic_corp0 <- generate_kwics(vect, keyword, window)
      # Tokenize
      kwic_phrases0 <- make_dfm(kwic_corp0, ngram)
      kwic_phrases0 <- as.matrix(kwic_phrases0)
      # Create data table with frequencies
      kwic_phrases <- colSums(kwic_phrases0)
      kwicdf <- data.table(phrase = names(kwic_phrases), kwic_freq = kwic_phrases)
      
      # Process to create a column of phrase lengths
      kwic_phrase_col <- kwicdf$phrase
      kwic_phrase_lengths <- unlist(lapply(kwic_phrase_col, function(x) length(unlist(strsplit(x, " ")))))
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
      overalldf <- as.data.table(do.call("cbind", list(overalldf0$phrase_length, overalldf0$phrase, as.numeric(overalldf0$corpus_freq), as.numeric(overalldf0$kwic_freq), as.numeric(overalldf0$word_prob), as.numeric(overalldf0$kwic_prob))))
      colnames(overalldf) <- c("phrase_length", "phrase", "word_freq", "kwic_freq", "word_prob", "kwic_prob")
      # setkey(overalldf, phrase)
      # I DON'T NEED THIS ANYMORE Remove any terms with corpus_freq = NA. These arise from the awkward joins in the kwic corpus (where I blended kwic rows into a single vector)
      # kwic_joins <- which(is.na(overalldf$phrase_length))
      # overalldf <- overalldf[-kwic_joins,]
      # retain the keyword probability
      keyword_probs <- overalldf[which(overalldf$phrase == keyword),]
      # #remove words and phrases that fall below the cutoff
      floor <- which(overalldf$kwic_freq < cutoff)
      overalldf2 <- overalldf[-floor,]
      
      # normalised Mutual information measure = (log(prob(xy)/prob(x)prob(y)))/log(probxy)
      #Calculate prob(xy)
      probxy <- as.numeric(overalldf2$kwic_prob)
      # #Calculate prob(x)prob(y)
      doc_probs_abridged <- as.numeric(overalldf2$word_prob)
      probxproby <- doc_probs_abridged*as.numeric(keyword_probs$word_prob)
      #calculate mutual information
      pmi <- log(probxy/probxproby)
      if(isTRUE(normalised) == TRUE){
            npmi <- pmi/(-(log(probxproby)))
             # Add to df
            overalldf2 <- cbind(overalldf2, npmi)
            overalldf2 <- overalldf2[order(overalldf2$npmi, overalldf2$kwic_freq, decreasing = TRUE),]
      } else {
        # Add to df
        overalldf2 <- cbind(overalldf2, pmi)
        overalldf2 <- overalldf2[order(overalldf2$pmi, overalldf2$kwic_freq, decreasing = TRUE),]
      }
# Tidy up the NAs which are a legacy of how I calculated the kwics. Note the NAs come down here are characters
      nas <- which(overalldf2$phrase == "NA")
      if(length(nas != 0)){
        overalldf2 <- overalldf2[-nas,]
      }
      
      return(overalldf2)
}