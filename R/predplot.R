

#' Title
#'
#' @param predictionresults Output from the results function.
#'
#' @return A plot
#' @export
#' @import ggplot2
#'
#' @examples
predplot <- function(predictionresults = results){

  end <- colnames(results[[2]])
  diff <- (unlist(results[2]))
  resulted <- (unlist(results[1]))

  forplot <- as.data.frame(   cbind(end, diff  , resulted  )   )
  forplot$diff <- as.numeric(as.character(forplot$diff))
  forplot$resulted <- as.numeric(as.character(forplot$resulted))


  forplot1 <- ggplot(data = forplot, aes(end, diff, fill = factor(resulted)   )  ) +
    geom_bar(aes(x = end, y = diff),stat='identity') +
    scale_y_continuous(limits=c(-0.5,0.75)) +
    geom_bar(forplot, mapping = aes(end) ,alpha=0, size=1, color="black", stat='identity')+
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    labs(title = "Differences across endpoints", x = "", y = "Standardized median difference", fill = "Prediction\nresults") +
    scale_fill_manual(labels = c("Incorrect", "Correct"), values = c("White", "Black")) +
    theme(plot.title = element_text(hjust = 0.5))


  +
    geom_segment(aes(x = 8.5, y = 0.46, xend = 9.5, yend = 0.46),size=1, linetype = 3)+
    geom_segment(aes(x = 8.5, y = -0.46, xend = 9.5, yend = -0.46),size=1,  linetype = 3)+
    geom_point(aes(x=9, y=0), shape = 1, fill = "white",size = 6, stroke = 1)

  class(forplot$end)

  output(forplot1)

}
