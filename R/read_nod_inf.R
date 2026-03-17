#' Read HYDRUS-1D Nod_Inf.out file
#'
#' Parses the Nod_Inf.out output file produced by HYDRUS-1D, which contains
#' nodal state variables at multiple saved time levels.  Returns a single
#' long-format data frame combining all time blocks.
#'
#' @param hydrus_output_path Path to the directory with HYDRUS output
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{node}{Node index (integer)}
#'     \item{depth}{Node depth [L]}
#'     \item{head}{Pressure head [L]}
#'     \item{moisture}{Volumetric water content [-]}
#'     \item{k}{Hydraulic conductivity [L/T]}
#'     \item{c}{Specific water capacity [1/L]. The water age [T] is
#'              written instead when particle tracking is considered.}
#'     \item{flux}{Nodal value of Darian velocity [L/T]}
#'     \item{sink}{Nodal value of root water uptake [1/T]}
#'     \item{kappa}{Kappa [-]}
#'     \item{v_ks_top}{v/KsTop ratio [-]}
#'     \item{temp}{Temperature [C]}
#'     \item{time}{Simulation time at which values were recorded [T]}
#'   }
#'
#' @export
read_nod_inf <- function(hydrus_output_path) {
  file <- paste0(hydrus_output_path, "/Nod_Inf.out")
  if (!file.exists(file)) stop("Nod_Inf.out was not found in ", hydrus_output_path)
  lines <- readLines(file, warn = FALSE)

  # Locate all "Time:" lines and extract the numeric time value
  time_idx <- grep("^\\s*Time:\\s", lines)
  time_vals <- as.numeric(
    sub("^\\s*Time:\\s+", "", lines[time_idx])
  )

  # Locate all "end" markers
  end_idx <- which(trimws(lines) == "end")

  col_names <- c(
    "node", "depth", "head", "moisture", "k", "c", "flux", "sink",
    "kappa", "v_ks_top", "temp"
  )

  blocks <- lapply(seq_along(time_idx), function(i) {
    blk_start <- time_idx[i]
    blk_end   <- end_idx[end_idx > blk_start][1]

    blk_lines <- lines[blk_start:blk_end]

    # Find the "Node  Depth" header within this block
    hdr_local <- grep("Node", blk_lines)[1]

    # Data starts 3 lines after the header (header + units line + blank)
    data_start_local <- hdr_local + 3
    # Data ends one line before "end"
    data_end_local   <- length(blk_lines) - 1

    if (data_start_local > data_end_local) return(NULL)

    data_lines <- blk_lines[data_start_local:data_end_local]
    data_lines <- data_lines[nzchar(trimws(data_lines))]

    if (length(data_lines) == 0L) return(NULL)

    df <- read.table(
      text    = paste(data_lines, collapse = "\n"),
      header  = FALSE,
      stringsAsFactors = FALSE
    )

    # Assign column names up to the number present
    names(df)[seq_len(min(ncol(df), length(col_names)))] <-
      col_names[seq_len(min(ncol(df), length(col_names)))]

    df$time <- time_vals[i]
    df
  })

  do.call(rbind, blocks)
}
