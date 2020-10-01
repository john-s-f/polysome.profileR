#' Graph a single polysome profile
#'
#' This function graphs data from a single sample with fractions shown in alternating colors. It takes a data frame created by read_polysomes
#' and the minimum distance from the top of the gradient at which you want to start graphing from.
#'
#' @param x data frame created by read_polysomes function.
#' @param min_position minimum position from top of gradient at which to start graphing from
#' @export
graph_single_polysomes <- function(x, min_position){
  # filter the df to a min position
  df <- x %>%
    dplyr::filter(position_mm >= min_position)

  # the colors used to define the fractions on the graph, alternating
  colors <- rep(c("#d93600","#006f91"), length(unique(df$fraction_number)))

  # find and make a df for the positions of the labels for the fractions
  labels.df <- df %>%
    split(.$fraction_number) %>%
    purrr::map(function(x){
      x$position_mm[floor(nrow(x)/2)]
    }) %>%
    unlist() %>%
    tibble::enframe(name = "fraction_number", value = "position_mm") %>%
    dplyr::left_join(df, by = c("fraction_number", "position_mm")) %>%
    dplyr::select(fraction_number, position_mm, absorbance) %>%
    dplyr::mutate(absorbance = 0)

  # make the graph
  p <- df %>%
    ggplot2::ggplot(., ggplot2::aes(position_mm, absorbance, color = fraction_number))+
    ggplot2::geom_line(size = 1)+
    ggplot2::geom_text(inherit.aes = FALSE, data = labels.df, ggplot2::aes(position_mm, absorbance, label = fraction_number, color = fraction_number))+
    ggplot2::theme_classic()+
    ggplot2::theme(text = ggplot2::element_text(size = 13))+
    ggplot2::scale_color_manual(values = colors, guide = FALSE)+
    ggplot2::labs(x = "Position from top of gradient (mm)",
                  y = "Absorbance",
                  title = df$filename)

  return(p)
}
