#' Parse a HYDRUS-1D PartAge.out file
#'
#' Reads the particle-age tracking output written by HYDRUS-1D and returns a
#' long-format data frame with one row per particle per time step.
#'
#' @param hydrus_output_path Path to the directory with HYDRUS output.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{\code{time}}{Simulation time \code{[T]}.}
#'   \item{\code{n_top}}{Cumulative count of particles that have left from the top boundary.}
#'   \item{\code{n_bot}}{Cumulative count of particles that have left from the bottom boundary.}
#'   \item{\code{rwu_age}}{Root-water-uptake weighted age \code{[T]}.}
#'   \item{\code{particle_index}}{Integer index of the particle (1-based, renumbered after exits).}
#'   \item{\code{age}}{Age of the particle \code{[T]}.}
#' }
#'
#' @export
read_part_age <- function(hydrus_output_path) {
  file <- paste0(hydrus_output_path, "/PartAge.out")
  if (!file.exists(file)) stop("PartAge.out was not found in ", hydrus_output_path)
  lines <- readLines(file, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]

  header_idx <- grep("^\\s*Time\\s+NP\\s+nTop", lines)
  data_lines  <- lines[(header_idx + 1):length(lines)]

  do.call(rbind, lapply(data_lines, function(line) {
    vals <- as.numeric(strsplit(trimws(line), "\\s+")[[1]])
    np   <- as.integer(vals[2])
    data.frame(
      time           = vals[1],
      n_top          = as.integer(vals[3]),
      n_bot          = as.integer(vals[4]),
      rwu_age        = vals[5],
      particle_index = seq_len(np),
      age            = vals[6:(5L + np)],
      stringsAsFactors = FALSE
    )
  }))
}


#' Compute the storage age distribution from HYDRUS-1D particle tracking output
#'
#' At each output time step, HYDRUS-1D records the age of every water particle
#' still present in the domain.  This function treats those ages as an
#' empirical, equal-weight distribution and returns summary statistics and
#' a long-format table suitable for plotting.
#'
#' @param hydrus_output_path Path to the directory with HYDRUS output.
#' @param times Optional numeric vector of simulation times (in \code{[T]}).
#'   The closest available output time is used for each requested value.  When
#'   \code{NULL} (the default) all time steps are returned.
#'
#' @return A list with two data frames:
#' \describe{
#'   \item{\code{summary}}{One row per time step with columns:
#'     \code{time}, \code{n_particles}, \code{mean_age}, \code{sd_age},
#'     \code{min_age}, \code{q25}, \code{median_age}, \code{q75},
#'     \code{max_age}.  Ages are in the same time unit as the simulation
#'     (\code{[T]}).}
#'   \item{\code{particles}}{Long-format table with columns \code{time},
#'     \code{particle_index}, and \code{age} — one row per particle per time
#'     step.  Useful for kernel-density or histogram plots of the full
#'     distribution.}
#' }
#'
#' @examples
#' d <- system.file("hydrus_output", package = "hydrus2sas")
#' sad <- storage_age_distribution(d)
#' head(sad$summary)
#'
#' # Distribution at a specific time (e.g., day 100)
#' sad100 <- storage_age_distribution(d, times = 100)
#' hist(sad100$particles$age, main = "Storage age distribution at t=100 d",
#'      xlab = "Age [d]")
#'
#' @export
storage_age_distribution <- function(hydrus_output_path, times = NULL) {
  particles <- read_part_age(hydrus_output_path)

  if (!is.null(times)) {
    all_times <- unique(particles$time)
    keep      <- vapply(times, function(t) all_times[which.min(abs(all_times - t))], numeric(1))
    particles <- particles[particles$time %in% keep, ]
  }

  summary_df <- do.call(rbind, lapply(split(particles, particles$time), function(d) {
    a <- d$age
    data.frame(
      time        = d$time[1],
      n_particles = nrow(d),
      mean_age    = mean(a),
      sd_age      = if (length(a) > 1) sd(a) else NA_real_,
      min_age     = min(a),
      q25         = unname(quantile(a, 0.25)),
      median_age  = median(a),
      q75         = unname(quantile(a, 0.75)),
      max_age     = max(a),
      stringsAsFactors = FALSE
    )
  }))
  rownames(summary_df) <- NULL

  list(summary = summary_df, particles = particles)
}
