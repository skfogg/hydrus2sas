#' Read HYDRUS-1D Obs_Node.out file
#'
#' Parses the Obs_Node.out output file produced by HYDRUS-1D, which stores
#' pressure head, water content, flux, and (when solute transport is active)
#' solute concentration at user-specified observation nodes for every output
#' time.  Returns a long-format data frame (one row per time-node combination).
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
#'     \item{conc}{Solute concentration [M/L3] — present only when solute
#'       transport is active}
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

  # Detect variable names per node from the column header
  hdr_tokens  <- strsplit(trimws(lines[col_hdr_idx]), "\\s+")[[1]]
  # Tokens after "time" repeat once per node; infer vars per node
  vars_per_node <- (length(hdr_tokens) - 1L) / n_nodes
  node_var_names <- tolower(hdr_tokens[seq(2, 1L + vars_per_node)])

  # Data starts on the line after the column header
  data_start <- col_hdr_idx + 1
  data_lines <- lines[data_start:length(lines)]
  data_lines <- data_lines[trimws(data_lines) != "end"]
  data_lines <- data_lines[nzchar(trimws(data_lines))]

  df <- read.table(
    text    = paste(data_lines, collapse = "\n"),
    header  = FALSE,
    stringsAsFactors = FALSE
  )

  # Build wide column names: time, then node_var_names per node
  wide_names <- c(
    "time",
    unlist(lapply(node_nums, function(nd) {
      paste0(node_var_names, "_", nd)
    }))
  )

  names(df)[seq_len(min(ncol(df), length(wide_names)))] <-
    wide_names[seq_len(min(ncol(df), length(wide_names)))]

  # Reshape to long format
  do.call(rbind, lapply(node_nums, function(nd) {
    row <- data.frame(
      time = df[["time"]],
      node = nd,
      stringsAsFactors = FALSE
    )
    for (v in node_var_names) {
      row[[v]] <- df[[paste0(v, "_", nd)]]
    }
    row
  }))
}
