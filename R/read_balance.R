#' Extract the first numeric value from a labelled Balance.out line
#'
#' Strips the leading label and bracketed unit tag from a line and returns
#' the first numeric token.  Used internally by \code{\link{read_balance}}.
#'
#' @param line A single character string from \code{Balance.out}.
#'
#' @return A single numeric value, or \code{NA_real_} if no numeric token is
#'   found.
#'
#' @keywords internal
.extract_balance_num <- function(line) {
  stripped <- sub("^[^[]*\\[[^]]*\\]\\s*", "", trimws(line))
  as.numeric(strsplit(stripped, "\\s+")[[1]])[1]
}


#' Read HYDRUS-1D Balance.out file
#'
#' Parses the Balance.out output file produced by HYDRUS-1D and returns a
#' data frame with one row per time block (per sub-region where applicable).
#' The water-balance error columns (\code{wat_bal_t}, \code{wat_bal_r}) are
#' \code{NA} for the initial (time = 0) block because HYDRUS-1D does not write
#' them there.
#'
#' @param hydrus_output_path Path to the directory with HYDRUS output.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{time}{Simulation time [T]}
#'     \item{sub_region}{Sub-region number (integer)}
#'     \item{length}{Profile length [L]}
#'     \item{w_volume}{Water volume [L]}
#'     \item{in_flow}{Inflow rate [L/T]}
#'     \item{h_mean}{Mean pressure head [L]}
#'     \item{top_flux}{Flux at top boundary [L/T]}
#'     \item{bot_flux}{Flux at bottom boundary [L/T]}
#'     \item{wat_bal_t}{Absolute water balance error [L] (\code{NA} for
#'       initial block)}
#'     \item{wat_bal_r}{Relative water balance error [\%] (\code{NA} for
#'       initial block)}
#'   }
#'
#' @export
read_balance <- function(hydrus_output_path) {
  file <- paste0(hydrus_output_path, "/Balance.out")
  if (!file.exists(file)) stop("Balance.out was not found in ", hydrus_output_path)
  lines <- readLines(file, warn = FALSE)
  n     <- length(lines)

  # Find the start of each time block
  time_starts <- grep("^\\s*Time\\s+\\[T\\]", lines)

  rows <- lapply(time_starts, function(ts) {
    window_end <- min(ts + 25L, n)
    blk        <- lines[ts:window_end]

    # Time value
    time_val <- .extract_balance_num(blk[1])

    # Sub-region number
    sr_idx <- grep("Sub-region num\\.", blk)[1]
    sub_region <- if (!is.na(sr_idx)) {
      as.integer(sub(".*Sub-region num\\.\\s*", "", blk[sr_idx]))
    } else {
      NA_integer_
    }

    # Helper: find the first match and extract value
    get_val <- function(pattern) {
      idx <- grep(pattern, blk)[1]
      if (is.na(idx)) NA_real_ else .extract_balance_num(blk[idx])
    }

    length_val  <- get_val("^\\s*Length\\s+\\[")
    w_volume    <- get_val("^\\s*W-volume\\s+\\[")
    in_flow     <- get_val("^\\s*In-flow\\s+\\[")
    h_mean      <- get_val("^\\s*h Mean\\s+\\[")
    top_flux    <- get_val("^\\s*Top Flux\\s+\\[")
    bot_flux    <- get_val("^\\s*Bot Flux\\s+\\[")
    wat_bal_t   <- get_val("^\\s*WatBalT\\s+\\[")
    wat_bal_r   <- get_val("^\\s*WatBalR\\s+\\[")

    data.frame(
      time       = time_val,
      sub_region = sub_region,
      length     = length_val,
      w_volume   = w_volume,
      in_flow    = in_flow,
      h_mean     = h_mean,
      top_flux   = top_flux,
      bot_flux   = bot_flux,
      wat_bal_t  = wat_bal_t,
      wat_bal_r  = wat_bal_r,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
