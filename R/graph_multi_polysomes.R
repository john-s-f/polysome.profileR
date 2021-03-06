#' Graph multiple polysome profiles on one graph
#'
#' This function takes a list of data frames generated by using read_polysomes with lapply and
#' graphs each sample as a different color on a single graph.
#'
#' @param x list of data frames created by read_polysomes function.
#' @param min_position minimum position from top of gradient at which to start graphing from
#' @export
graph_multi_polysomes <- function(x, min_position){
  dplyr::bind_rows(x) %>%
    dplyr::filter(position_mm >= min_position) %>%
    ggplot2::ggplot(., ggplot2::aes(position_mm, absorbance, color = filename))+
    ggplot2::geom_line(size = 1, alpha = .75)+
    ggplot2::theme_classic()+
    ggplot2::theme(text = ggplot2::element_text(size = 13))+
    ggplot2::labs(x = "Position from top of gradient (mm)",
                  y = "Absorbance")+
    ggplot2::scale_color_brewer(palette = "Set1", name = NULL)
}
