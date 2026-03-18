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
#'     \item{sum_r_top}{Cumulative potential surface flux at top boundary [L]. infiltration -/evaporation +}
#'     \item{sum_r_root}{Cumulative potential transpiration [L]}
#'     \item{sum_v_top}{Cumulative actual flux at surface [L]}
#'     \item{sum_v_root}{Cumulative actual transpiration [L]}
#'     \item{sum_v_bot}{Cumulative flux at bottom boundary [L]. inflow +/outflow -.}
#'     \item{h_top}{Pressure head at top node [L]}
#'     \item{h_root}{Mean pressure head in root zone [L]}
#'     \item{h_bot}{Pressure head at bottom node [L]}
#'     \item{a_level}{A-level index (integer; current variable boundary condition number)}
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

  text_data <- paste(data_lines, collapse = "\n")
  df <- as.data.frame(readr::read_fwf(
    I(text_data),
    col_positions = readr::fwf_positions(start = c(3,14,28,42,56,69,83,99,107,120),
                                         end = c(12,26,40,54,68,82,93,104,115,NA)),
    show_col_types = FALSE
  ))

  col_names <- c(
    "time", "sum_r_top", "sum_r_root", "sum_v_top", "sum_v_root",
    "sum_v_bot", "h_top", "h_root", "h_bot", "a_level"
  )

  names(df)[seq_len(min(ncol(df), length(col_names)))] <-
    col_names[seq_len(min(ncol(df), length(col_names)))]

  df
}
