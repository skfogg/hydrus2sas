#' Read HYDRUS-1D Uptake.out (particle tracking root-water-uptake) file
#'
#' Parses the Uptake.out output file produced by HYDRUS-1D particle tracking.
#' Each row in the file records the time, the current number of particles
#' (\code{NP}), the index of the topmost and bottommost particles, and one
#' uptake value per particle.  Because \code{NP} varies between rows, the
#' function parses line-by-line and returns a long-format data frame with one
#' row per (time, particle) combination.
#'
#' @param hydrus_output_path Path to the directory with HYDRUS output.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{time}{Simulation time [T]}
#'     \item{n_top}{Index of the topmost particle}
#'     \item{n_bot}{Index of the bottommost particle}
#'     \item{particle_index}{Position index of the particle (1-based)}
#'     \item{uptake}{Root water uptake value for this particle}
#'   }
#'
#' @export
read_uptake <- function(hydrus_output_path) {
  file <- paste0(hydrus_output_path, "/Uptake.out")
  if (!file.exists(file)) stop("Uptake.out was not found in ", hydrus_output_path)
  lines <- readLines(file, warn = FALSE)

  # Remove blank lines
  lines <- lines[nzchar(trimws(lines))]

  # Find the header line
  header_idx <- grep("^\\s*Time\\s+NP\\s+nTop", lines)[1]

  # Data lines start immediately after the header
  data_lines <- lines[(header_idx + 1L):length(lines)]
  data_lines <- data_lines[nzchar(trimws(data_lines))]

  rows <- lapply(data_lines, function(ln) {
    tokens <- strsplit(trimws(ln), "\\s+")[[1]]

    time_val  <- as.numeric(tokens[1])
    np_val    <- as.integer(tokens[2])
    n_top_val <- as.integer(tokens[3])
    n_bot_val <- as.integer(tokens[4])

    # Uptake values: tokens 5 through 4 + NP
    if (np_val > 0L && length(tokens) >= 4L + np_val) {
      uptake_vals <- as.numeric(tokens[5:(4L + np_val)])
    } else {
      uptake_vals <- numeric(0)
    }

    if (length(uptake_vals) == 0L) return(NULL)

    data.frame(
      time           = time_val,
      n_top          = n_top_val,
      n_bot          = n_bot_val,
      particle_index = seq_along(uptake_vals),
      uptake         = uptake_vals,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
