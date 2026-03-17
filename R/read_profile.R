#' Read HYDRUS-1D Profile.out file
#'
#' Parses the Profile.out output file produced by HYDRUS-1D, which lists the
#' van Genuchten soil hydraulic parameters assigned to each node.  Returns one
#' row per node.
#'
#' @param hydrus_output_path Path to the directory with HYDRUS output.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{node}{Node index (integer)}
#'     \item{depth}{Node depth [L]}
#'     \item{thr}{Residual water content [-]}
#'     \item{ths}{Saturated water content [-]}
#'     \item{hs}{Air-entry pressure head [L]}
#'     \item{ks}{Saturated hydraulic conductivity [L/T]}
#'     \item{ks_ks_top}{Ratio of Ks to Ks at top node [-]}
#'     \item{beta}{Tortuosity/connectivity parameter [-]}
#'     \item{ah}{Scaling factor for pressure head [-]}
#'     \item{ak}{Scaling factor for hydraulic conductivity [-]}
#'     \item{ath}{Scaling factor for water content [-]}
#'   }
#'
#' @export
read_profile <- function(hydrus_output_path) {
  file <- paste0(hydrus_output_path, "/Profile.out")
  if (!file.exists(file)) stop("Profile.out was not found in ", hydrus_output_path)
  lines <- readLines(file, warn = FALSE)

  # Find the header line containing "THr"
  header_idx <- grep("THr", lines)[1]

  # Data starts on the line immediately after the header
  data_start <- header_idx + 1

  data_lines <- lines[data_start:length(lines)]
  data_lines <- data_lines[nzchar(trimws(data_lines))]

  df <- read.table(
    text    = paste(data_lines, collapse = "\n"),
    header  = FALSE,
    stringsAsFactors = FALSE
  )

  col_names <- c(
    "node", "depth", "thr", "ths", "hs", "ks", "ks_ks_top",
    "beta", "ah", "ak", "ath"
  )

  names(df)[seq_len(min(ncol(df), length(col_names)))] <-
    col_names[seq_len(min(ncol(df), length(col_names)))]

  df
}
