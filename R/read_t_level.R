#' Read HYDRUS-1D T_Level.out file
#'
#' Parses the T_Level.out output file produced by HYDRUS-1D and returns a
#' data frame with one row per time step.  Any cumulative observation-node
#' flux columns beyond the 22 fixed base columns are appended as
#' \code{cum_flux_obs_1}, \code{cum_flux_obs_2}, etc.
#'
#' @param hydrus_output_path Path to the directory with HYDRUS output.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{time}{Simulation time [T]}
#'     \item{r_top}{Flux at top boundary [L/T]}
#'     \item{r_root}{Root water uptake rate [L/T]}
#'     \item{v_top}{Actual flux at top [L/T]}
#'     \item{v_root}{Actual root uptake rate [L/T]}
#'     \item{v_bot}{Flux at bottom boundary [L/T]}
#'     \item{sum_r_top}{Cumulative top flux [L]}
#'     \item{sum_r_root}{Cumulative root uptake [L]}
#'     \item{sum_v_top}{Cumulative actual top flux [L]}
#'     \item{sum_v_root}{Cumulative actual root uptake [L]}
#'     \item{sum_v_bot}{Cumulative bottom flux [L]}
#'     \item{h_top}{Pressure head at top node [L]}
#'     \item{h_root}{Mean pressure head in root zone [L]}
#'     \item{h_bot}{Pressure head at bottom node [L]}
#'     \item{run_off}{Surface runoff rate [L/T]}
#'     \item{sum_run_off}{Cumulative surface runoff [L]}
#'     \item{storage}{Water storage in profile [L]}
#'     \item{sum_infil}{Cumulative infiltration [L]}
#'     \item{sum_evap}{Cumulative evaporation [L]}
#'     \item{t_level}{T-level index (integer)}
#'     \item{cum_w_trans}{Cumulative water transpiration [L]}
#'     \item{snow_layer}{Snow layer depth [L]}
#'     \item{cum_flux_obs_1, cum_flux_obs_2, ...}{Cumulative flux at observation
#'       nodes (variable number)}
#'   }
#'
#' @export
read_t_level <- function(hydrus_output_path) {
  file <- paste0(hydrus_output_path, "/T_Level.out")
  if (!file.exists(file)) stop("T_Level.out was not found in ", hydrus_output_path)
  lines <- readLines(file, warn = FALSE)

  # Find the header line containing "rTop"
  header_idx <- grep("rTop", lines)[1]

  # Data starts after the header line and the units line below it
  data_start <- header_idx + 2

  data_lines <- lines[data_start:length(lines)]

  # Remove the terminal "end" line and any blank lines
  data_lines <- data_lines[trimws(data_lines) != "end"]
  data_lines <- data_lines[nzchar(trimws(data_lines))]

  df <- read.table(
    text    = paste(data_lines, collapse = "\n"),
    header  = FALSE,
    stringsAsFactors = FALSE
  )

  base_names <- c(
    "time", "r_top", "r_root", "v_top", "v_root", "v_bot",
    "sum_r_top", "sum_r_root", "sum_v_top", "sum_v_root", "sum_v_bot",
    "h_top", "h_root", "h_bot", "run_off", "sum_run_off", "storage",
    "sum_infil", "sum_evap", "t_level", "cum_w_trans", "snow_layer"
  )

  n_base  <- length(base_names)
  n_extra <- ncol(df) - n_base

  extra_names <- character(0)
  if (n_extra > 0) {
    extra_names <- paste0("cum_flux_obs_", seq_len(n_extra))
  }

  names(df) <- c(base_names, extra_names)

  df
}
