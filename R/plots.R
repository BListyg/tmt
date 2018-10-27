library(ggplot2)

#' Create ggplot histogram
#'
#' @param df Dataframe
#' @param x Variable to use when creating histogram
#' @param title Title for the plot
#' @import ggplot2
#' @export
plot_histogram <- function(df, x, title) {

  p <- ggplot(data=df, aes(x=df[[x]])) +
    geom_histogram(breaks=seq(1, 5, by = 0.5),
                   binwidth=0.2,
                   color="black",
                   fill="white") +
    ggtitle(title) +
    theme(plot.title = element_text(lineheight=.8,
                                    face="bold",
                                    hjust = 0.5),
          legend.position = "none",
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

  return(p)

}
