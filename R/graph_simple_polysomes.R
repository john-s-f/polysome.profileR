#' Graph a single polysome profile
#'
#' This function graphs data from a single sample with no fraction markings. It takes a data frame created by read_polysomes
#' and the minimum distance from the top of the gradient at which you want to start graphing from.
#'
#' @param x data frame created by read_polysomes function.
#' @param min_position minimum position from top of gradient at which to start graphing from
#' @export
graph_simple_polysomes <- function(x, min_position){
  # filter the df to a min position
  df <- x %>%
    dplyr::filter(position_mm >= min_position)

  p <- df %>%
    ggplot2::ggplot(., ggplot2::aes(position_mm, absorbance))+
    ggplot2::geom_line(size = .6)+
    ggplot2::theme_classic()+
    ggplot2::theme(text = ggplot2::element_text(size = 14))+
    ggplot2::labs(x = "Position from top of gradient (mm)",
                  y = "Absorbance",
                  title = as.character(unique(df$filename)))

  return(p)
}
