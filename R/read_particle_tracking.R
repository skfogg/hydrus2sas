# Internal parser for Particles.out (depths only).
.read_particles <- function(hydrus_output_path) {
  file <- paste0(hydrus_output_path, "/Particles.out")
  if (!file.exists(file)) stop("Particles.out was not found in ", hydrus_output_path)
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
      particle_index = seq_len(np),
      depth          = vals[5:(4L + np)],
      stringsAsFactors = FALSE
    )
  }))
}


#' Read HYDRUS-1D particle tracking output (depth and age)
#'
#' Combines \code{Particles.out} (particle depths) and \code{PartAge.out}
#' (particle ages) into a single long-format data frame with one row per
#' particle per time step.
#'
#' @param hydrus_output_path Path to the directory with HYDRUS output.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{\code{time}}{Simulation time \code{[T]}.}
#'   \item{\code{n_top}}{Cumulative count of particles that left from the top boundary.}
#'   \item{\code{n_bot}}{Cumulative count of particles that left from the bottom boundary.}
#'   \item{\code{particle_index}}{Integer index of the particle (1-based).}
#'   \item{\code{depth}}{Particle depth \code{[L]} (negative downward).}
#'   \item{\code{age}}{Particle age \code{[T]}.}
#' }
#'
#' @examples
#' pt <- read_particle_tracking(system.file("hydrus_output", package = "hydrus2sas"))
#' head(pt)
#'
#' @export
read_particle_tracking <- function(hydrus_output_path) {
  depths <- .read_particles(hydrus_output_path)
  ages   <- read_part_age(hydrus_output_path)[, c("time", "particle_index", "age")]

  merged <- merge(depths, ages, by = c("time", "particle_index"), sort = FALSE)
  merged[order(merged$time, merged$particle_index), ]
}
