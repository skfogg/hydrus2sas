#' Read HYDRUS-1D Run_Inf.out file
#'
#' Parses the Run_Inf.out output file produced by HYDRUS-1D and returns a
#' data frame with one row per time step, recording iteration and convergence
#' information.
#'
#' @param hydrus_output_path Path to the directory with HYDRUS output.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{t_level}{Time-level index (integer)}
#'     \item{time}{Simulation time [T]}
#'     \item{dt}{Current time step size [T]}
#'     \item{iter}{Number of iterations in this step}
#'     \item{it_cum}{Cumulative iteration count}
#'     \item{kod_t}{Code for top boundary condition}
#'     \item{kod_b}{Code for bottom boundary condition}
#'     \item{convergency}{Convergence flag (\code{"T"} or \code{"F"})}
#'   }
#'
#' @export
read_run_inf <- function(hydrus_output_path) {
  file <- paste0(hydrus_output_path, "/Run_Inf.out")
  if (!file.exists(file)) stop("Run_Inf.out was not found in ", hydrus_output_path)
  lines <- readLines(file, warn = FALSE)

  # Find the header line containing "TLevel"
  header_idx <- grep("TLevel", lines)[1]

  # Data starts on the line immediately after the header
  data_start <- header_idx + 1

  data_lines <- lines[data_start:length(lines)]
  data_lines <- data_lines[nzchar(trimws(data_lines))]

  df <- read.table(
    text         = paste(data_lines, collapse = "\n"),
    header       = FALSE,
    stringsAsFactors = FALSE,
    colClasses   = c(
      "integer",   # TLevel
      "numeric",   # Time
      "numeric",   # dt
      "integer",   # Iter
      "integer",   # ItCum
      "integer",   # KodT
      "integer",   # KodB
      "character"  # Convergency
    )
  )

  names(df) <- c(
    "t_level", "time", "dt", "iter", "it_cum", "kod_t", "kod_b",
    "convergency"
  )

  df
}
