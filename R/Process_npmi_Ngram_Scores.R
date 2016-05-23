#' Calculates a pointwise mutual information score for ngrams in a corpus, normalised if requested: log2(prob(xy)/prob(x)prob(y))
#'
#' @param vect A vector of text files or a corpus
#' @param keyword A keyword to feed to the kwic vector
#' @param window The width of the kwic vector
#' @param ngram The ngram types
#' @param cutoff A floor for infrequent terms
#' @import quanteda
#' @import data.table
#' @keywords kwic corpus mutual information
#' @export

# I tokenise the corpus, then derive a wordcount, plus counts and
# probabilities for word x and keyword y and for word x,y: x-as-member-of-class-y (the keyword in
# context window). PMI = log(p(x,y)/p(x)p(y)) For NPMI, divide by p(x)p(y).
#######################
#Token the whole corpus
pmi_score <- function(vect, 
                      keyword,
                      window = 5,
                      ngram = 1,
                      cutoff = 3,
                      normalised = TRUE){
  
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
print("Processing the corpus: this might take a long time")
# Run the corpus through the function
vect_phrases <- make_dfm(vect, ngram)
#######################
# Create a data table with ngram frequency for each phrase
vect_sums <- colSums(vect_phrases)
vectdf <- data.table(phrase = names(vect_sums),
                     count.x = vect_sums)
# Overall wordcount for calculating probabilities
wordcount <- sum(vect_sums)
print("Finished processing the corpus")
# Process to create a column of phrase lengths (to control phrase length in
# calculating probability)
phrases <- vectdf$phrase
phrase_lengths <- sapply(phrases, function(x)
      length(unlist(strsplit(x, " "))))
vectdf <- cbind(phrase_lengths, vectdf)
# Split the data table into a list by each length
vectdf_list <- split(vectdf, vectdf$phrase_lengths)
# A column with the probability for each word or phrase in the context of
# phrase length
x_probs <- lapply(vectdf_list, function(x) 
      x$count.x/(wordcount/x$phrase_lengths))
# Cbind along both lists
vectdf_list <- lapply(seq_along(1:length(vectdf_list)), function(x) 
      cbind(vectdf_list[[x]], x_probs[[x]]))
# Bring into a single data table and set key
vectdf <- do.call("rbind", vectdf_list)
colnames(vectdf) <- c("phrase_length",
                      "phrase", 
                      "count.x",
                      "prob.x")
setkey(vectdf, phrase)
#######################
# Create a similar dataframe for the collocations so I can narrow to relevant words and phrases
print("Generating scores for the keyword in context terms")
# Isolate kwics, controlling for overlaps
kwic_corp00 <- lapply(vect, function(x) simple_kwics(x, keyword, window))
kwic_corp0 <- unlist(kwic_corp00)
# Return an error message if the keyword is not found in the corpus
      nas <- which(is.na(kwic_corp0))
      if(length(nas) > 0){
            kwic_corp0 <- kwic_corp0[-nas]
            }
      if(length(kwic_corp0) == 0){
            print("The keyword was not found in the corpus")
            } else {
      # Continue with the function
      # Tokenize
      kwic_phrases0 <- make_dfm(kwic_corp0, ngram)
      kwic_phrases0 <- as.matrix(kwic_phrases0)
      # Create data table with frequencies
      kwic_phrases <- colSums(kwic_phrases0)
      kwicdf <- data.table(phrase = names(kwic_phrases), count.xy = kwic_phrases)
      # Process to create a column of phrase lengths
      kwic_phrase_col <- kwicdf$phrase
      kwic_phrase_lengths <- unlist(lapply(kwic_phrase_col, function(x) 
            length(unlist(strsplit(x, " ")))))
      kwicdf <- cbind(kwic_phrase_lengths, kwicdf)
      # Split the data table into a list by each length
      kwicdf_list <- split(kwicdf, kwicdf$kwic_phrase_lengths)
      # A column with the probability for each word or phrase in the context of phrase length
      kwic_probs <- lapply(kwicdf_list, function(x)
            x$count.xy/(wordcount/x$kwic_phrase_lengths))
      # Cbind along both lists
      kwicdf_list <- lapply(seq_along(1:length(kwicdf_list)), function(x)
            cbind(kwicdf_list[[x]], kwic_probs[[x]]))
      # Bring into a single data table and set the key
      kwicdf <- do.call("rbind", kwicdf_list)
      colnames(kwicdf) <- c("phrase_length", 
                            "phrase",
                            "count.xy",
                            "prob.xy")
      setkey(kwicdf, phrase)
      #######################
      # Merge both dfs
      overalldf0 <- vectdf[.(kwicdf)]
      # Get count and prob y
      loc.y.num <- which(vectdf$phrase == keyword)
      loc.y <- vectdf[loc.y.num,]
      # Create a tidy data table
      overalldf <- as.data.table(do.call("cbind",
                                         list(as.numeric(overalldf0$phrase_length),
                                              as.numeric(wordcount),
                                              overalldf0$phrase, 
                                              as.numeric(overalldf0$count.x),
                                              as.numeric(overalldf0$prob.x),
                                              count.y = loc.y$count.x,
                                              prob.y = loc.y$prob.x,  
                                              as.numeric(overalldf0$count.xy), 
                                              as.numeric(overalldf0$prob.xy))))
      colnames(overalldf) <- c("phrase_length",
                               "wordcount",
                               "phrase",
                               "count.x",
                               "prob.x",
                               "count.y",
                               "prob.y",
                               "count.xy",
                               "prob.xy")
      setkey(overalldf, phrase)
      #######################
      # Cutoff infrequent words
      if(cutoff > 1){
            floor <- which(as.numeric(overalldf$count.xy) <= cutoff)
            if(length(floor) > 0){
            overalldf2 <- overalldf[-floor,]
            }} else {
                  overalldf2 <- overalldf
                  }
      #######################
      # normalised Mutual information measure = (log(prob(xy)/prob(x)prob(y)))/log(probxy)
      probxy <- as.numeric(overalldf2$prob.xy)
      probxproby <- as.numeric(overalldf2$prob.x)*as.numeric(overalldf2$prob.y)
      #calculate mutual information and cbind to data table
      pmi <- log(probxy/probxproby)
      overalldf2 <- cbind(overalldf2, pmi)
      #######################
      # If normalised is true calculate npmi and cbind
      if(isTRUE(normalised) == TRUE){
            npmi <- pmi/(-(log(probxy)))
            # Add to df
            overalldf2 <- data.table(cbind(overalldf2$phrase_length, overalldf2$wordcount, overalldf2$phrase, overalldf2$count.x, overalldf2$count.y, overalldf2$count.xy, overalldf2$pmi))
            colnames(overalldf2) <- c("phrase_length", "wordcount", "phrase", "count.x", "count.y", "count.xy", "pmi")
            overalldf2 <- data.table(cbind(overalldf2, npmi))
            overalldf2 <- overalldf2[order(overalldf2$npmi,
                                           overalldf2$pmi,
                                           overalldf$count.y,
                                           decreasing = TRUE),]
            } else {
                  # Add to df without calculating npmi
                  overalldf2 <- cbind(overalldf2, pmi)
                  overalldf2 <- cbind(overalldf2$phrase_length, overalldf2$wordcount, overalldf2$phrase, overalldf2$count.x, overalldf2$count.y, overalldf2$count.xy, overalldf2$pmi)
                  overalldf2 <- overalldf2[order(overalldf2$pmi,
                                                 overalldf2$count.y,
                                                 decreasing = TRUE),]
                  }
      #######################
      # Tidy up the NAs which are a legacy of how I calculated the kwics. Note the NAs come down here are  characters
      nas <- which(overalldf2$phrase == "NA")
      if(length(nas != 0)){
           overalldf2 <- overalldf2[-nas,]
           }
#######################
      # Return results
      return(overalldf2)
      }
# End of the if statement
}