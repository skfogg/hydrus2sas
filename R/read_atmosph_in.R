#' Read HYDRUS-1D ATMOSPH.IN input file
#'
#' Parses the ATMOSPH.IN input file used by HYDRUS-1D, which defines
#' atmospheric boundary conditions for each time step.  Returns one row per
#' atmospheric record.
#'
#' Column names are derived from the header line in the file and cleaned to
#' snake_case.  The \code{RootDepth} token, which appears in the header but
#' has no corresponding data column, is automatically dropped.  The number of
#' data columns varies between simulations (e.g. simulations with heat or
#' solute transport include additional columns such as \code{t_top},
#' \code{t_bot}, \code{ampl}, \code{c_top}, \code{c_bot}).
#'
#' @param hydrus_output_path Path to the directory containing ATMOSPH.IN.
#'
#' @return A data frame with one row per atmospheric record.  Always contains
#'   \code{t_atm}, \code{prec}, \code{r_soil}, \code{r_root}, \code{h_crit_a},
#'   \code{r_b}, \code{h_b}, \code{ht}; may contain additional columns
#'   depending on simulation settings.
#'
#' @export
read_atmosph_in <- function(hydrus_output_path) {
  file <- paste0(hydrus_output_path, "/ATMOSPH.IN")
  if (!file.exists(file)) stop("ATMOSPH.IN was not found in ", hydrus_output_path)
  lines <- readLines(file, warn = FALSE)

  # Find the column header line (contains "tAtm")
  hdr_idx <- grep("tAtm", lines)[1]

  # Extract and clean column names from the header line
  raw_names   <- strsplit(trimws(lines[hdr_idx]), "\\s+")[[1]]
  clean_names <- gsub("_+$|^_+", "", gsub("[^a-z0-9]+", "_", tolower(raw_names)))

  # Data starts on the line immediately after the header
  data_lines <- lines[(hdr_idx + 1):(grep("end", lines)-1)]
  # data_lines <- data_lines[trimws(data_lines) != "end"]
  # data_lines <- data_lines[nzchar(trimws(data_lines))]

  df <- read.table(
    text             = paste(data_lines, collapse = "\n"),
    header           = FALSE,
    stringsAsFactors = FALSE
  )

  # RootDepth appears in the header but not in the data; drop trailing header
  # tokens that exceed the actual column count
  n_data <- ncol(df)
  names(df) <- clean_names[seq_len(n_data)]

  df
}
