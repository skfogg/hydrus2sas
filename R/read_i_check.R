#' Read HYDRUS-1D I_Check.out file
#'
#' Parses the I_Check.out output file produced by HYDRUS-1D, which records the
#' initial nodal state (depth, pressure head, material index, etc.) as written
#' by the pre-simulation check.  Returns one row per node.
#'
#' The file may contain additional columns beyond the 9 base columns (e.g.\
#' temperature, solute concentrations, sorbed concentrations) depending on the
#' simulation settings.  Extra columns are named \code{extra_1},
#' \code{extra_2}, etc.
#'
#' @param hydrus_output_path Path to the directory with HYDRUS output.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{node}{Node index (integer)}
#'     \item{x}{Node coordinate [L]}
#'     \item{h_old}{Initial pressure head [L]}
#'     \item{mat_n}{Material number (integer)}
#'     \item{lay_n}{Layer number (integer)}
#'     \item{beta}{Root-uptake stress response parameter [-]}
#'     \item{ah}{Scaling factor for pressure head [-]}
#'     \item{ak}{Scaling factor for hydraulic conductivity [-]}
#'     \item{ath}{Scaling factor for water content [-]}
#'     \item{extra_1, extra_2, ...}{Any additional columns present (e.g.
#'       temperature, concentration, sorption)}
#'   }
#'
#' @export
read_i_check <- function(hydrus_output_path) {
  file <- paste0(hydrus_output_path, "/I_Check.out")
  if (!file.exists(file)) stop("I_Check.out was not found in ", hydrus_output_path)
  lines <- readLines(file, warn = FALSE)

  # Find the header line containing "hOld"
  header_idx <- grep("hOld", lines)[1]

  # Data starts on the line immediately after the header
  data_start <- header_idx + 1

  data_lines <- lines[data_start:length(lines)]
  data_lines <- data_lines[nzchar(trimws(data_lines))]

  df <- read.table(
    text    = paste(data_lines, collapse = "\n"),
    header  = FALSE,
    stringsAsFactors = FALSE
  )

  base_names <- c(
    "node", "x", "h_old", "mat_n", "lay_n", "beta", "ah", "ak", "ath"
  )

  n_base  <- length(base_names)
  n_extra <- ncol(df) - n_base

  extra_names <- character(0)
  if (n_extra > 0L) {
    extra_names <- paste0("extra_", seq_len(n_extra))
  }

  names(df) <- c(base_names[seq_len(min(ncol(df), n_base))], extra_names)

  df
}
