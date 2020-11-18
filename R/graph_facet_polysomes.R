#' Graph multiple polysome profiles on separate graphs, but in one plot.
#'
#' This uses facet_wrap to graph many samples on the same plot but in different panels. It takes a list of read_polysome data frames
#' and a minimum position. It labels fraction positions. It has fixed scales.
#'
#' @param x list of data frames created by using read_polysomes with lapply.
#' @param min_position minimum position from top of gradient at which to start graphing from
#' @param ncols number of columns you want from the facets, an integer
#' @param scale_type one of free, free_x, free_y as a string, really you should only use free_y
#' @export
graph_facet_polysomes <- function(x, min_position, ncols, scale_type){
  # row bind the list and filter the df to a min position
  df <- dplyr::bind_rows(x) %>%
    dplyr::filter(position_mm >= min_position)

  # the colors used to define the fractions on the graph, alternating
  colors <- rep(c("#d93600","#006f91"), length(unique(df$fraction_number)))

  # find and make a df for the positions of the labels for the fractions
  labels.df <- df %>%
    base::split(list(.$fraction_number, .$filename), drop = TRUE) %>%
    purrr::map(function(x){
      x$position_mm[floor(nrow(x)/2)]
    }) %>%
    base::unlist() %>%
    tibble::enframe(name = "fraction_number", value = "position_mm") %>%
    tidyr::separate(fraction_number, into = c("fraction_number", "filename"), sep = "\\.") %>%
    dplyr::left_join(df, by = c("fraction_number", "position_mm", "filename")) %>%
    dplyr::select(filename, fraction_number, position_mm, absorbance) %>%
    dplyr::mutate(absorbance = 0)

  # make the graph
  p <- df %>%
    ggplot2::ggplot(., ggplot2::aes(position_mm, absorbance, color = fraction_number))+
    ggplot2::geom_line(size = 1)+
    ggplot2::geom_text(inherit.aes = FALSE, data = labels.df, ggplot2::aes(position_mm, absorbance, label = fraction_number, color = fraction_number))+
    ggplot2::theme_bw()+
    ggplot2::theme(text = ggplot2::element_text(size = 13),
                   panel.grid.minor = ggplot2::element_blank())+
    ggplot2::scale_color_manual(values = colors, guide = FALSE)+
    ggplot2::labs(x = "Position from top of gradient (mm)",
                  y = "Absorbance")+
    ggplot2::facet_wrap(~filename, ncol = ncols, scales = scale_type)

  return(p)
}
