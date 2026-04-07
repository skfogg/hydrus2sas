#' Read HYDRUS-1D Nod_Inf.out file
#'
#' Parses the Nod_Inf.out output file produced by HYDRUS-1D, which contains
#' nodal state variables at multiple saved time levels.  Returns a single
#' long-format data frame combining all time blocks.
#'
#' Column names are derived directly from the header line in the file, cleaned
#' to lowercase with non-alphanumeric characters replaced by underscores.
#' When the file contains a variable-length placeholder (e.g.
#' \code{Conc(1...NS)}), the placeholder is expanded into numbered columns
#' (\code{conc_1_ns_1}, \code{conc_1_ns_2}, …) to match the actual number of
#' data columns.  A \code{time} column is always appended.
#'
#' @param hydrus_output_path Path to the directory with HYDRUS output
#'
#' @return A data frame with one row per node per print time.  Column names
#'   match the header in the file (cleaned to snake_case), plus \code{time}.
#'
#' @export
read_nod_inf <- function(hydrus_output_path) {
  file <- paste0(hydrus_output_path, "/Nod_Inf.out")
  if (!file.exists(file)) stop("Nod_Inf.out was not found in ", hydrus_output_path)
  lines <- readLines(file, warn = FALSE)

  # Locate all "Time:" lines and extract the numeric time value
  time_idx  <- grep("^\\s*Time:\\s", lines)
  time_vals <- as.numeric(sub("^\\s*Time:\\s+", "", lines[time_idx]))

  # Locate all "end" markers
  end_idx <- which(trimws(lines) == "end")

  # Extract and clean column names from the header line in the first block
  blk1_lines <- lines[time_idx[1]:end_idx[1]]
  hdr_local  <- grep("^\\s*Node\\s", blk1_lines)[1]
  raw_names  <- strsplit(trimws(blk1_lines[hdr_local]), "\\s+")[[1]]
  clean_names <- gsub("_+$|^_+", "", gsub("[^a-z0-9]+", "_", tolower(raw_names)))

  blocks <- lapply(seq_along(time_idx), function(i) {
    blk_start <- time_idx[i]
    blk_end   <- end_idx[end_idx > blk_start][1]

    blk_lines <- lines[blk_start:blk_end]

    # Find the "Node  Depth" header within this block
    hdr_local <- grep("^\\s*Node\\s", blk_lines)[1]

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

    # Expand trailing placeholder when data has more columns than header tokens
    col_names <- clean_names
    n_data    <- ncol(df)
    n_hdr     <- length(col_names)
    if (n_data > n_hdr) {
      base      <- col_names[n_hdr]
      n_extra   <- n_data - n_hdr + 1L
      col_names <- c(col_names[-n_hdr], paste0(base, "_", seq_len(n_extra)))
    }

    names(df)[seq_len(min(n_data, length(col_names)))] <-
      col_names[seq_len(min(n_data, length(col_names)))]

    df$time <- time_vals[i]
    df
  })

  do.call(rbind, blocks)
}
