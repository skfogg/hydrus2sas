#' Read HYDRUS-1D A_Level.out file
#'
#' Parses the A_Level.out output file produced by HYDRUS-1D and returns a
#' data frame with one row per time step.
#'
#' @param hydrus_output_path Path to the directory with HYDRUS output.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{time}{Simulation time [T]}
#'     \item{sum_r_top}{Cumulative flux at top boundary [L]}
#'     \item{sum_r_root}{Cumulative root water uptake [L]}
#'     \item{sum_v_top}{Cumulative actual flux at top [L]}
#'     \item{sum_v_root}{Cumulative actual root uptake [L]}
#'     \item{sum_v_bot}{Cumulative flux at bottom boundary [L]}
#'     \item{h_top}{Pressure head at top node [L]}
#'     \item{h_root}{Mean pressure head in root zone [L]}
#'     \item{h_bot}{Pressure head at bottom node [L]}
#'     \item{a_level}{A-level index (integer)}
#'   }
#'
#' @export
read_a_level <- function(hydrus_output_path) {
  file <- paste0(hydrus_output_path, "/A_Level.out")
  if (!file.exists(file)) stop("A_Level.out was not found in ", hydrus_output_path)
  lines <- readLines(file, warn = FALSE)

  # Find the header line containing sum(rTop)
  header_idx <- grep("sum\\(rTop\\)", lines)[1]

  # Data starts after the header line and the units line below it
  data_start <- header_idx + 2

  data_lines <- lines[data_start:length(lines)]
  data_lines <- data_lines[nzchar(trimws(data_lines))]

  df <- read.table(
    text    = paste(data_lines, collapse = "\n"),
    header  = FALSE,
    stringsAsFactors = FALSE
  )

  col_names <- c(
    "time", "sum_r_top", "sum_r_root", "sum_v_top", "sum_v_root",
    "sum_v_bot", "h_top", "h_root", "h_bot", "a_level"
  )

  # Assign column names up to the number of columns present
  names(df)[seq_along(col_names)] <- col_names[seq_len(ncol(df))]

  df
}
