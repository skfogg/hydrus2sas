#' Convert output from a HYDRUS model to input used in a SAS model
#'
#' @param hydrus_output_path path to directory where HYDRUS output is
#' @param depths numeric vector of the minimum and maximum soil depths of the HYDRUS model
#' @param model_times numeric vector of the minimum and maximum model_times to use for SAS input
#' @param node_spacing numeric vector of the
#'
#' @returns a data frame with the following columns:
#' \describe{
#'    \item{t}{time}
#'    \item{S}{soil column storage}
#'    \item{J}{water input at the soil surface (precipitation & irrigation)}
#'    \item{ET}{evapotranspiration water output at the soil surface}
#'    \item{Q}{water output at the maximum depth at the soil column (deep percolation)}
#' }
#' @export
#'
#' @examples get_sas_input("examples/hydrus_output",
#'                         depths = c(0, 150),
#'                         model_times = c(497, 861),
#'                         node_spacing = 1.75)
get_sas_input <- function(hydrus_output_path,
                          min_node = 0,
                          max_node,
                          model_times,
                          start_date = "01/01/2021 00:00:00",
                          node_spacing){
  ## TO DO: make a switching parameter that would allow the user to switch between using the node inf and obs node data

  cat("Gathering output from observation nodes...\n")
  obs_nodes <- read_obs_node(hydrus_output_path)
  observation_nodes <- unique(obs_nodes$node)

  cat("Calculating daily mean concentration of max node...\n")
  bottom_node <- subset(obs_nodes, node == observation_nodes[max_node])
  ## TO DO: make so if time is in units other than days, the correct calc happens here:
  real_time <- lubridate::mdy_hms(start_date) + (bottom_node$time*86400)
  bottom_node_xts <- suppressWarnings(xts::xts(zoo::zoo(bottom_node, order.by = real_time)))
  daily_mean_conc_out <- xts::apply.daily(bottom_node_xts$conc, colMeans)

  # Get daily average theta (water content)
  cat("Calculating daily mean water storage...\n")
  daily_mean_theta <- list(0)
  for(i in 1:max_node){
    this_node <- subset(obs_nodes, node == unique(obs_nodes$node)[i])
    this_node_theta_xts <- suppressWarnings(xts::xts(zoo::zoo(this_node$theta, order.by = real_time)))
    daily_mean_theta[[i]] <- xts::apply.daily(this_node_theta_xts$x, colMeans)
  }
  daily_mean_theta <- as.data.frame(daily_mean_theta)

  # Integrate to get the water storage of the soil column
  vwc <- numeric(nrow(daily_mean_theta))
  for(i in 1:nrow(daily_mean_theta)){
    vwc[i] <- pracma::trapz(x = observation_nodes[1:max_node]*1.75,
                            y = unlist(daily_mean_theta[i,]))
  }


  # daily_mean_theta <- list(0)
  # for(i in 1:length(unique(obs_nodes$node)[1:max_node])){
  #   this_node <- subset(obs_nodes, node == unique(obs_nodes$node)[i])
  #   this_node_xts <- suppressWarnings(xts::xts(zoo::zoo(this_node, order.by = real_time)))
  #   daily_mean_theta[[i]] <- xts::apply.daily(this_node_xts$theta, colMeans)
  # }
  # theta_endpoints <- c(0, observation_nodes)
  # for(i in 1:(length(theta_endpoints)-1)){
  #   theta_endpoints[i+1] <- observation_nodes[i] + ((observation_nodes[i+1]-observation_nodes[i])/2)
  # }
  # theta_endpoints[length(theta_endpoints)] <- tail(observation_nodes, 1)
  # theta_startpoints <- theta_endpoints[1:(length(theta_endpoints)-1)]
  # theta_endpoints <- theta_endpoints[2:length(theta_endpoints)]
  #
  # node_representation_range <- (theta_endpoints - theta_startpoints)*node_spacing
  #
  # obs_nodes[obs_nodes$node == 2, "theta"]

  # bottom_node_xts <- suppressWarnings(xts::xts(zoo::zoo(bottom_node, order.by = real_time)))
  # daily_mean_conc_out <- xts::apply.daily(bottom_node_xts$conc, colMeans)
  #
  # vwc <- numeric(length(unique(obs_nodes$time)))
  # for(j in unique(obs_nodes$time)){
  #   one_time <- obs_nodes[obs_nodes$time == j,]
  #   vwc[(j-min(unique(obs_nodes$time)))+1] <- pracma::trapz(x = one_time$node*node_spacing,
  #                                                           y = one_time$theta)
  #   }


  # node_output <- read_nod_inf(hydrus_output_path)
  # soil_column <- subset(node_output,
  #                       node >= min_node & node <= max_node & time >= min(model_times) & time <= max(model_times))
  model_inputs <- read_atmosph_in(hydrus_output_path)

  # delta_depth <- node_spacing

  # # S, Storage of the soil column:
  # total_moisture <- numeric(length(min(model_times):max(model_times)))
  # for(j in unique(soil_column$time)){
  #   one_time <- soil_column[soil_column$time == j,]
  #
  #   mean_moisture <- numeric(nrow(one_time))
  #   for(i in 1:(nrow(one_time)-1)){
  #     mean_moisture[i] <- mean(c(one_time$moisture[i], one_time$moisture[i+1]))
  #   }
  #
  #   total_moisture[j-(min(model_times)-1)] <- sum(mean_moisture * delta_depth)
  # }

  ## J and ET, surface water fluxes:
  # ET <- (as.numeric(model_inputs$rsoil) + as.numeric(model_inputs$rroot))[min(model_times):max(model_times)]
  # J <- as.numeric(model_inputs$prec)[min(model_times):max(model_times)]
  cat("Gathering surface inputs...\n")
  ET <- (as.numeric(model_inputs$rsoil) + as.numeric(model_inputs$rroot))
  J <- as.numeric(model_inputs$prec)


  # Q, subsurface water fluxes:
  # Q <- soil_column[soil_column$node == max_node, "flux"]
  # grep("flux", colnames(soil_column))

  daily_mean_flux_out <- xts::apply.daily(bottom_node_xts$flux, colMeans)
  Q <- daily_mean_flux_out

  # Concentration_in:
  # C_in <- dplyr::filter(model_inputs, tatm >= model_times[1] & tatm <= model_times[2])$ctop
  C_in <- model_inputs$ctop


  # Concentration out:
  # soil_column_bottom <- dplyr::filter(soil_column, node == max(node))
  # C_out <- rowSums(soil_column_bottom[, grep("conc", colnames(soil_column_bottom))])
  C_out <- zoo::coredata(daily_mean_conc_out)

  sas_dataframe <- data.frame(t = 1:nrow(daily_mean_conc_out),
                             S = vwc, #total_moisture,
                             J = c(0,J),
                             ET = c(0,ET),
                             Q = abs(Q),
                             C_in = c(0,C_in),
                             C_out = C_out)

  colnames(sas_dataframe) <- c("t", "S", "J", "ET", "Q", "C_in", "C_out")

  row.names(sas_dataframe) <- NULL
  return(sas_dataframe)

}
