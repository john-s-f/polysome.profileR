#' Get metadata from polysome file
#'
#' This function retrieves the metadata from the polysome csv. Warning: the metadata for these files is only
#' partially controlled by the machine, the user enters some of it, hence it may not be correct. Please keep
#' careful notes or ensure that you entered it correctly on the machine. Additionally, in the event of manual
#' polysome collection, the fraction volume and number of samples will be intentionally wrong.
#'
#' @param x file path to a polysome csv file.
#' @export

read_metadata <- function(x){
  metadata <- readr::read_lines(x)

  meta.list <- list("experiment" = metadata[1:5],
                    "centrifuge_settings" = metadata[10:18],
                    "fractionation_settings" = metadata[21:33],
                    "scan_settings" = metadata[36:43])

  return(meta.list)
}
