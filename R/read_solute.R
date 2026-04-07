#' Read a HYDRUS-1D solute transport output file (solute\emph{N}.out)
#'
#' Parses the \code{soluteN.out} file produced by HYDRUS-1D, which records
#' time-level solute fluxes and concentrations at the domain boundaries.
#' The solute number \code{N} is supplied via the \code{solute} argument.
#'
#' @param hydrus_output_path Path to the directory with HYDRUS output.
#' @param solute Integer solute index (default \code{1}).  Selects the file
#'   \code{solute\{solute\}.out}.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{time}{Simulation time [T]}
#'     \item{cv_top}{Solute flux at top boundary [M/L2/T]}
#'     \item{cv_bot}{Solute flux at bottom boundary [M/L2/T]}
#'     \item{sum_cv_top}{Cumulative solute flux at top boundary [M/L2]}
#'     \item{sum_cv_bot}{Cumulative solute flux at bottom boundary [M/L2]}
#'     \item{cv_ch0}{Cumulative solute change (zero-order) [M/L2]}
#'     \item{cv_ch1}{Cumulative solute change (first-order) [M/L2]}
#'     \item{c_top}{Concentration at top node [M/L3]}
#'     \item{c_root}{Mean concentration in root zone [M/L3]}
#'     \item{c_bot}{Concentration at bottom node [M/L3]}
#'     \item{cv_root}{Solute uptake by roots [M/L2/T]}
#'     \item{sum_cv_root}{Cumulative solute uptake by roots [M/L2]}
#'     \item{sum_cv_neql}{Cumulative non-equilibrium solute exchange [M/L2]}
#'     \item{t_level}{T-level index (integer)}
#'     \item{v_frac_s}{Volumetric fraction of immobile water [-]}
#'     \item{cv_frac_s}{Cumulative solute in immobile fraction [M/L2]}
#'   }
#'
#' @export
read_solute <- function(hydrus_output_path, solute = 1) {
  file <- paste0(hydrus_output_path, "/solute", solute, ".out")
  if (!file.exists(file)) stop("solute", solute, ".out was not found in ", hydrus_output_path)
  lines <- readLines(file, warn = FALSE)

  header_idx <- grep("cvTop", lines)[1]
  data_start <- header_idx + 2

  data_lines <- lines[data_start:length(lines)]
  data_lines <- data_lines[trimws(data_lines) != "end"]
  data_lines <- data_lines[nzchar(trimws(data_lines))]

  df <- read.table(
    text             = paste(data_lines, collapse = "\n"),
    header           = FALSE,
    stringsAsFactors = FALSE
  )

  names(df) <- c(
    "time", "cv_top", "cv_bot", "sum_cv_top", "sum_cv_bot",
    "cv_ch0", "cv_ch1", "c_top", "c_root", "c_bot",
    "cv_root", "sum_cv_root", "sum_cv_neql", "t_level",
    "v_frac_s", "cv_frac_s"
  )

  df
}


#' Read a HYDRUS-1D mobile-phase solute transport output file (solutef\emph{N}.out)
#'
#' Parses the \code{solutefN.out} file produced by HYDRUS-1D, which records
#' time-level solute fluxes and concentrations for the mobile (free) water
#' fraction.  The solute number \code{N} is supplied via the \code{solute}
#' argument.
#'
#' @param hydrus_output_path Path to the directory with HYDRUS output.
#' @param solute Integer solute index (default \code{1}).  Selects the file
#'   \code{solutef\{solute\}.out}.
#'
#' @return A data frame with the same columns as \code{\link{read_solute}}.
#'
#' @export
read_solutef <- function(hydrus_output_path, solute = 1) {
  file <- paste0(hydrus_output_path, "/solutef", solute, ".out")
  if (!file.exists(file)) stop("solutef", solute, ".out was not found in ", hydrus_output_path)
  lines <- readLines(file, warn = FALSE)

  header_idx <- grep("cvTop", lines)[1]
  data_start <- header_idx + 2

  data_lines <- lines[data_start:length(lines)]
  data_lines <- data_lines[trimws(data_lines) != "end"]
  data_lines <- data_lines[nzchar(trimws(data_lines))]

  df <- read.table(
    text             = paste(data_lines, collapse = "\n"),
    header           = FALSE,
    stringsAsFactors = FALSE
  )

  names(df) <- c(
    "time", "cv_top", "cv_bot", "sum_cv_top", "sum_cv_bot",
    "cv_ch0", "cv_ch1", "c_top", "c_root", "c_bot",
    "cv_root", "sum_cv_root", "sum_cv_neql", "t_level",
    "v_frac_s", "cv_frac_s"
  )

  df
}


#' Read a HYDRUS-1D immobile-phase solute transport output file (solutem\emph{N}.out)
#'
#' Parses the \code{solutemN.out} file produced by HYDRUS-1D, which records
#' time-level solute fluxes and concentrations for the immobile water fraction.
#' The solute number \code{N} is supplied via the \code{solute} argument.
#'
#' @param hydrus_output_path Path to the directory with HYDRUS output.
#' @param solute Integer solute index (default \code{1}).  Selects the file
#'   \code{solutem\{solute\}.out}.
#'
#' @return A data frame with the same columns as \code{\link{read_solute}}.
#'
#' @export
read_solutem <- function(hydrus_output_path, solute = 1) {
  file <- paste0(hydrus_output_path, "/solutem", solute, ".out")
  if (!file.exists(file)) stop("solutem", solute, ".out was not found in ", hydrus_output_path)
  lines <- readLines(file, warn = FALSE)

  header_idx <- grep("cvTop", lines)[1]
  data_start <- header_idx + 2

  data_lines <- lines[data_start:length(lines)]
  data_lines <- data_lines[trimws(data_lines) != "end"]
  data_lines <- data_lines[nzchar(trimws(data_lines))]

  df <- read.table(
    text             = paste(data_lines, collapse = "\n"),
    header           = FALSE,
    stringsAsFactors = FALSE
  )

  names(df) <- c(
    "time", "cv_top", "cv_bot", "sum_cv_top", "sum_cv_bot",
    "cv_ch0", "cv_ch1", "c_top", "c_root", "c_bot",
    "cv_root", "sum_cv_root", "sum_cv_neql", "t_level",
    "v_frac_s", "cv_frac_s"
  )

  df
}
