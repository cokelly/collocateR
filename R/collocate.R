#' A collocation function
#'
#' Takes a tm corpus as input, returns a list of matrices containing collocates as output.
#' @param corpus A tm corpus
#' @param keyword The keyword that you wish to find collocates for
#' @param collocation_width The number of neighbouring words on each side
#' @param tidy Tidy the corpus - remove punctuation, numbers, empty lines (optional) and whitespace, convert to lowercase
#' @param remove_empty_lines Remove empty lines from the corpus (default: TRUE)
#' @keywords collocates concordance
#' @export
#' @examples
#' library(tm)
#' data("crude")
#' collocate(crude,"oil", collocation_width = 6, tidy = TRUE)


collocate <- function(corpus, keyword, collocation_width = 4, tidy = TRUE) {
    require(tm)
    if (tidy == TRUE) {
        corpus <- tidy(corpus)
    }
    # Adding extra data that will be requested in te function
    keyword <- keyword
    collocation_width <- collocation_width
    #################################
    #turn the corpus into a list of character vectors
    vectors <- sapply(corpus, "content")
    # split the vectors so that I can locate any specific keyword
    vectors <- sapply(vectors, function(x) strsplit(x, split = " "))
    vector_names <- names(vectors)
    # Isolate if any words come within a collocation width of the text
    # beginnings or ends returns a list of lists including keyword locations,
    # plus a list of 'early' and 'late' words (and a matrix)

        #################################

collocations_calc <- function(input_vector, ...) {
        doc_length <- length(input_vector)
        keyword_locations <- which(input_vector == keyword)
        #################################
        if (length(keyword_locations) == 0) {
            collocates.df <- "The keyword does not appear in this document"
        } else {
            # Get collocation width words before and after the keyword
            early <- if (any(keyword_locations <= collocation_width)) {
                which(keyword_locations <= collocation_width)
            }
            doc_range <- (doc_length) - (collocation_width)
            late <- if (any(keyword_locations >= doc_range)) {
                which(keyword_locations >= doc_range)
            }
            ################################# If there are no keywords too close to the front or back of the document
            if (is.null(early) & is.null(late)) {
                collocates.df <- normal_collocates(collocation_width, input_vector,
                  keyword_locations, keyword)
                ################################# Allocate names to each column
                left_cols <- paste(seq_along(1:collocation_width), "L", sep = "")
                left_cols <- rev(left_cols)
                right_cols <- paste(seq_along(1:collocation_width), "R", sep = "")
                vector_names_final <- c(left_cols, paste("keyword"), right_cols)
                colnames(collocates.df) <- vector_names_final
                #################################
                return(collocates.df)
            }
            ################################# if there are keywords too close to the front work through the early
            ################################# collocates function, then remove the early word and collocate the
            ################################# remainder, then combine for the final table.
            if (!is.null(early) & is.null(late)) {
                earlycollocates.df <- early_collocates(collocation_width,
                  early, input_vector, keyword, keyword_locations)

                # remove the early word(s) from the overall vector of keyword locations
                keyword_locations <- keyword_locations[-early]  # remove the early word(s)
                # Source normal for the rest if that leaves nothing just return the early
                # matrix
                if (length(keyword_locations) == 0) {
                  collocates.df <- earlycollocates.df
                  ################# Otherwise run the remainder through the normal function
                } else {
                  collocates.df <- normal_collocates(collocation_width, input_vector,
                    keyword_locations, keyword)
                  collocates.df <- rbind(earlycollocates.df, collocates.df)  # rbind the two matrices and return them
                }
                ################################# Allocate names to each column
                left_cols <- paste(seq_along(1:collocation_width), "L", sep = "")
                left_cols <- rev(left_cols)
                right_cols <- paste(seq_along(1:collocation_width), "R", sep = "")
                vector_names_final <- c(left_cols, paste("keyword"), right_cols)
                colnames(collocates.df) <- vector_names_final
                #################################
                return(collocates.df)
            }
            ################################# if there are keywords too close to the back work through the late
            ################################# collocates function, then remove the late word and collocate the
            ################################# remainder, then combine for the final table.
            if (is.null(early) & !is.null(late)) {
                latewords <- keyword_locations[late]
                keyword_locations2 <- keyword_locations[-late]
                #################
                latecollocates.df <- late_collocates(collocation_width, doc_length,
                  input_vector, keyword, keyword_locations, late)
                # I NEED TO FIGURE OUT LATE WORDS MATRIX if that leaves nothing just
                # return the early matrix
                if (length(keyword_locations2) == 0) {
                  collocates.df <- latecollocates.df
                  ################# Otherwise run the remainder through the normal function
                } else {
                  collocates.df <- normal_collocates(collocation_width, input_vector,
                    keyword_locations2, keyword)
                  collocates.df <- rbind(latecollocates.df, collocates.df)  #
                }
                ################################# Allocate names to each column
                left_cols <- paste(seq_along(1:collocation_width), "L", sep = "")
                left_cols <- rev(left_cols)
                right_cols <- paste(seq_along(1:collocation_width), "R", sep = "")
                vector_names_final <- c(left_cols, paste("keyword"), right_cols)
                colnames(collocates.df) <- vector_names_final
                #################################
                return(collocates.df)
            }
            ################################# if there are keywords too close at both ends through the early
            ################################# collocates function, then remove the early word, then work through the
            ################################# late collocate function and remove the late word, then collocate the
            ################################# remainder, then combine for the final table.
            if (!is.null(early) & !is.null(late)) {
                earlywords <- keyword_locations[early]
                latewords <- keyword_locations[late]
                keyword_locations2 <- keyword_locations[-early]
                keyword_locations2 <- keyword_locations2[-(late - 1)]

                # Work through early function
                earlycollocates.df <- early_collocates(collocation_width,
                  early, input_vector, keyword, keyword_locations)
                # Work through late function
                latecollocates.df <- late_collocates(collocation_width, doc_length,
                  input_vector, keyword, keyword_locations, late)
                # I NEED TO FIGURE OUT LATE WORDS MATRIX

                # Work through normal function if that leaves nothing just return the
                # early matrix
                if (length(keyword_locations2) == 0) {
                  collocates.df <- rbind(earlycollocates.df, latecollocates.df)
                  ################# Otherwise run the remainder through the normal function
                } else {
                  collocates.df <- normal_collocates(collocation_width, input_vector,
                    keyword_locations2, keyword)
                  # Combine
                  collocates.df <- rbind(earlycollocates.df, collocates.df,
                    latecollocates.df)
                }
                ################# Allocate names to each column
                left_cols <- paste(seq_along(1:collocation_width), "L", sep = "")
                left_cols <- rev(left_cols)
                right_cols <- paste(seq_along(1:collocation_width), "R", sep = "")
                vector_names_final <- c(left_cols, paste("keyword"), right_cols)
                colnames(collocates.df) <- vector_names_final
                #################
                return(collocates.df)
            }
        }  # a closing bracket from the if statement at the start looking to circumvent searches for the word where it does not appear in the document
    }
    #################################
    collocates <- lapply(vectors, function(x) collocations_calc(x))  # apply the function
    names(collocates) <- vector_names
    #################################
    return(collocates)
}
