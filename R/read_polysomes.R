#' Read a polysome profiling csv from the BioComp machine
#'
#' This function loads a csv generated from the biocomp machine, discarding any metadata in the process.
#' There is a separate function read_metadata for getting the metadata.
#' It then cleans the data to make it more amenable to the computer. It does not remove or alter data.
#' It will add a filename column to the data, which is derived from the last element in the path.
#' This function takes a single path to a csv. Use with lapply to make a list of files.
#'
#' The machine only puts the fraction number in the row where the signal is sent to the fractionator, meaning
#' that every row with a fraction number separates two fractions. For example, the first fraction is all rows
#' above the row with a 1 in it, and then fraction 2 is between 2 and 3, and so on. The last fraction is called "A"
#' for "air". This function copies this column to fraction_points and fills the fraction column with values,
#' converting the A to a number in the process.
#'
#' This function throws a warning if one of the values in the
#' fraction_number column was duplicated, as may be the case with the A fraction, perhaps because you let
#' go of the button and pressed it again when you were getting the last bit out of the machine.
#'
#' @param filepath Path to the input file.
#' @export
read_polysomes <- function(filepath){
  # get the file name
  fname <- tail(unlist(strsplit(filepath, split = "/")), n = 1) %>%
    gsub(".csv", "", .)

  # read the file in, skipping the headers
  df <- readr::read_csv(filepath, skip = 45) %>%
    # rename the columns to programming friendly
    dplyr::rename("sample" = "Sample",
                  "source" = "Source",
                  "position_mm" = "Position(mm)",
                  "absorbance" = "Absorbance",
                  "fraction_number" = "Fraction Number",
                  "fraction_volume_ml" = "Fraction Volume(ml)") %>%
  # copy the frac numb col to a new col
  dplyr::mutate(fraction_point = fraction_number,
                filename = fname) %>%
  # fill original frac numb col
  tidyr::fill(fraction_number, .direction = "updown")

  # Change the A (for air) in frac numb to a number
  # Figure out what number it should be
  max.frac.numb <- max(as.numeric(unique(df$fraction_number[df$fraction_number %in% 1:200]))) + 1

  # Change it in both columns and convert to numeric
  df$fraction_number[df$fraction_number == "A"] <- max.frac.numb

  df$fraction_point[df$fraction_point == "A"] <- max.frac.numb

  df$fraction_number <- as.numeric(df$fraction_number)

  df$fraction_point <- as.numeric(df$fraction_point)

  # factor fraction number in numerical order
  df$fraction_number <- factor(df$fraction_number, levels = sort(unique(df$fraction_number)))

  # generate a warning if one of the fractions appears multiple times as is sometimes the case with the A fraction
  fracs <- df$fraction_point[!is.na(df$fraction_point)]

  if (any(duplicated(fracs))){
    warning("**One of the fractions appears to be duplicated, you should inspect the Fraction Number column in the original file. Proceeding regardless.**")
  }

  return(df)
}
