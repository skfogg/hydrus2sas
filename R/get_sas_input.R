#' Convert output from a HYDRUS model to input used in a SAS model
#'
#' @param hydrus_output_path path to directory where HYDRUS output is
#' @param depths numeric vector of the minimum and maximum soil depths of the HYDRUS model
#' @param times numeric vector of the minimum and maximum times to use for SAS input
#' @param node_spacing numeric vector of the
#'
#' @returns
#' @export
#'
#' @examples
get_sas_input <- function(hydrus_output_path,
                          depths,
                          times,
                          node_spacing){

  node_output <- read_nod_inf(hydrus_output_path)
  soil_column <- subset(node_output, abs(depth) >= abs(min(depths)) & abs(depth) <= abs(max(depths)) & time >= min(times) & time <= max(times))

  delta_depth <- node_spacing

  # S, Storage of the soil column:
  total_moisture <- numeric(length(min(times):max(times)))
  for(j in unique(soil_column$time)){
    one_time <- soil_column[soil_column$time == j,]

    mean_moisture <- numeric(nrow(one_time))
    for(i in 1:(nrow(one_time)-1)){
      mean_moisture[i] <- mean(c(one_time$moisture[i], one_time$moisture[i+1]))
    }

    total_moisture[j-(min(times)-1)] <- sum(mean_moisture * delta_depth)
  }

  ## J and ET, surface water fluxes:
  hydrus_output_path <- "inst/hydrus_output_one_year_more_particles"
  atmosph_in <- readLines(paste0(hydrus_output_path, "/ATMOSPH.IN"))

  if(length(stringr::str_split(stringr::str_trim(atmosph_in[grep("tAtm",atmosph_in)]), "\\s+")[[1]]) == 9){
    atm_inputs <- readr::read_fwf(paste0(hydrus_output_path, "/ATMOSPH.IN"),
                                  skip = 9,
                                  col_positions = fwf_positions(start = c(3,12,24,36,48,60,72,84,96),
                                                                end = c(11,23,35,47,59,71,83,95,NA)),
                                  n_max = grep("end", atmosph_in)-1,
                                  show_col_types = FALSE)
    colnames(atm_inputs) <- stringr::str_split(stringr::str_trim(atmosph_in[grep("tAtm",atmosph_in)]), "\\s+")[[1]]
  }else{
    stop("Debugging needed at fwf_positions() function of read_fwf() used to read in ATMOSPH.IN file.")
  }

  ET <- as.numeric(atm_inputs$rSoil) + as.numeric(atm_inputs$rRoot)
  J <- as.numeric(atm_inputs$Prec)

  # Q, subsurface water fluxes:
  max(depths)
  Q <- soil_column[soil_column$node == max(depths), "flux"]

  data.frame(t = unique(soil_column$time) - min(soil_column$time)+1,
             S = total_moisture,
             J = J,
             ET = ET,
             Q = Q)

}
