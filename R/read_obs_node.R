#' Read HYDRUS-1D Obs_Node.out file
#'
#' Parses the Obs_Node.out output file produced by HYDRUS-1D, which stores
#' pressure head, water content, and flux at user-specified observation nodes
#' for every output time.  Returns a long-format data frame (one row per
#' time-node combination).
#'
#' @param hydrus_output_path Path to the directory with HYDRUS output.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{time}{Simulation time [T]}
#'     \item{node}{Observation node number (integer)}
#'     \item{h}{Pressure head [L]}
#'     \item{theta}{Volumetric water content [-]}
#'     \item{flux}{Water flux [L/T]}
#'   }
#'
#' @export
read_obs_node <- function(hydrus_output_path) {
  file <- paste0(hydrus_output_path, "/Obs_Node.out")
  if (!file.exists(file)) stop("Obs_Node.out was not found in ", hydrus_output_path)
  lines <- readLines(file, warn = FALSE)

  # Find the node-list header line (first line containing "Node(")
  node_line_idx <- grep("Node\\(", lines)[1]
  node_line     <- lines[node_line_idx]

  # Extract all integers from the node header line as node numbers
  node_nums <- as.integer(
    regmatches(node_line, gregexpr("[0-9]+", node_line))[[1]]
  )

  n_nodes <- length(node_nums)

  # Find the column-header line (starts with whitespace then "time")
  col_hdr_idx <- grep("^\\s*time\\s+h\\s+theta", lines)[1]

  # Data starts on the line after the column header
  data_start <- col_hdr_idx + 1
  data_lines <- lines[data_start:length(lines)]
  data_lines <- data_lines[nzchar(trimws(data_lines))]

  df <- read.table(
    text    = paste(data_lines, collapse = "\n"),
    header  = FALSE,
    stringsAsFactors = FALSE
  )

  # Build wide column names: time, then (h, theta, flux) per node
  wide_names <- c(
    "time",
    unlist(lapply(node_nums, function(nd) {
      c(paste0("h_",     nd),
        paste0("theta_", nd),
        paste0("flux_",  nd))
    }))
  )

  # Assign only as many names as there are columns
  names(df)[seq_len(min(ncol(df), length(wide_names)))] <-
    wide_names[seq_len(min(ncol(df), length(wide_names)))]

  # Reshape to long format
  long_list <- lapply(node_nums, function(nd) {
    data.frame(
      time  = df[["time"]],
      node  = nd,
      h     = df[[paste0("h_",     nd)]],
      theta = df[[paste0("theta_", nd)]],
      flux  = df[[paste0("flux_",  nd)]],
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, long_list)
}
