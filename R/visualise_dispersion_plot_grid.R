#' Take a dataframe of word locations and create a faceted dispersion plot
#'
#' Take a dataframe of word locations and create a faceted dispersion plot
#' @param vectors Vectors of text files
#' @param keyword A keyword
#' @param panel_labels The labels for each vector
#' @param plottitle A title for the plot
#' @importFrom cowplot background_grid
#' @import ggplot2
#' @keywords ggplot dispersion plot
#' @export
#' @examples
#' test <- faceted_dispersion_plot(csr2, "risk", years, plottitle = "Word locations for 'risk' in BP's CSR Reports 1998-2014")

faceted_dispersion_plot <- function(vectors, keyword, panel_labels = names(vectors), plottitle = NULL){
      vectors <- as.list(vectors)
      # Dealing with panel names
      #Double if statement Test if there are the right number of panel labels
      if(!identical(length(panel_labels), length(vectors))){
            if(!is.null(panel_labels)){
                  print("The number of panel labels must match the number of vectors")
                  }}
      ifelse(is.null(panel_labels), vectornames <- names(vectors), vectornames <- panel_labels)
      
      # Dealing with plot title
      
      # Create a cascade of dispersion plots
      dfs0 <- lapply(seq_along(1:length(vectors)), function(x) word_locations(vectors[[x]], keyword, vectornames[x]))
      maxlengths <- max(unlist(lapply(dfs0, nrow)))
      full_df <- do.call("rbind", dfs0)
      full_df[,1] <- as.factor(full_df[,1])
      full_df[,2] <- as.numeric(full_df[,2])
      full_df[,3] <- as.numeric(full_df[,3])
      # Plot a dispersion plot using ggplot2, geom_segment and facet_wrap
      disp_plot <- ggplot(full_df, aes(x = word, y = matchwords)) +
            geom_segment(aes(xend = word, yend = 0)) +
            theme(
                  axis.text.x = element_blank()
                  ,axis.title.x = element_blank()
                  ,axis.ticks = element_blank()
                  ,axis.text.y = element_blank()
                  ,axis.title.y = element_blank()
                  ,axis.line = element_blank()
                  ,panel.border = element_blank()
            ) +
            geom_hline(yintercept = 1, linetype = 1, size = .2, alpha = .2) +
            geom_hline(yintercept = 0, linetype = 1, size = .2, alpha = .2) + 
            geom_vline(xintercept = 0, linetype = 1, size = .2, alpha = .2) +
            scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
            # theme(
            #       panel.grid.major = element_line(),
            #       panel.grid.major.y = element_blank(),
            #       panel.grid.minor = element_blank()
            # ) +
            facet_grid(file ~ ., scales = "free", space = "free") +
            theme(strip.text.y = element_text(angle = 0, size = 8))
# Include the plot title if it is requested
      if(!is.null(plottitle)){disp_plot <- disp_plot + ggtitle(plottitle)}
      
      return(disp_plot)
      }