library(tidyverse)
library(lubridate)
library(zoo)
library(xts)


kbr_obs <- read_obs_node("inst/hydrus_output_fairfield_kbr")
unique(kbr_obs$node)
plot(conc ~ time, subset(kbr_obs, node == unique(kbr_obs$node)[8]))
unique(kbr_obs$node)[8]
plot(subset(kbr_obs, node == unique(kbr_obs$node)[8])$time)


bottom_node <- subset(kbr_obs, node == unique(kbr_obs$node)[8])
real_time <- mdy_hms("01/01/2021 00:00:00") + (bottom_node$time*86400)
bottom_node_xts <- xts(zoo(bottom_node, order.by = real_time))
plot.zoo(bottom_node_xts$conc)

plot.zoo(apply.daily(bottom_node_xts$conc, Colmeans), type = "p")

numerical_times <- unique(kbr_obs$time)
vwc <- numeric(length(numerical_times))

# test_vwc <- tapply(kbr_obs, kbr_obs$time, pracma::trapz, x = kbr_obs$node*1.75, y = kbr_obs$theta)

for(j in numerical_times){
  one_time <- kbr_obs[kbr_obs$time == j,]
  vwc[(j-min(unique(kbr_obs$time)))+1] <- pracma::trapz(x = one_time$node*1.75,
                                                        y = one_time$theta)
}

max_node = 8
real_time <- mdy_hms("01/01/2021 00:00:00") + (bottom_node$time*86400)

start_time <- Sys.time()
daily_mean_theta <- list(0)
for(i in 1:max_node){
  this_node <- subset(kbr_obs, node == unique(kbr_obs$node)[i])
  this_node_theta_xts <- xts(zoo(this_node$theta, order.by = real_time))
  daily_mean_theta[[i]] <- apply.daily(this_node_theta_xts$x, colMeans)
}
end_time <- Sys.time()
end_time - start_time

daily_mean_theta <- as.data.frame(daily_mean_theta)
vwc <- numeric(nrow(daily_mean_theta))
for(i in 1:nrow(daily_mean_theta)){
  vwc[i] <- pracma::trapz(x = unique(kbr_obs$node)[1:max_node]*1.75,
                          y = unlist(daily_mean_theta[i,]))
}



sas_input_kbr <- get_sas_input("inst/hydrus_output_fairfield_kbr",
                               max_node = 8,
                               node_spacing = 1.75)
write.csv(sas_input_kbr, "inst/hydrus_output_fairfield_kbr/sas_input_kbr.csv")

plot(sas_input_kbr$C_out, type = "l")
lines(sas_input_kbr$C_in, col = "red")

plot(sas_input_kbr$J, type = "l", col = "blue")
lines(sas_input_kbr$ET, col = "green")
lines(sas_input_kbr$Q, col = "violet")

plot(sas_input_kbr$S, type = "l")


kbr <- read_solute("inst/hydrus_output_fairfield_kbr", solute = 1)
head(kbr)

kbr_node <- read_obs_node("inst/hydrus_output_fairfield_kbr")


kbr_node_inf <-read_nod_inf("inst/hydrus_output_fairfield_kbr")

unique(kbr_node_inf$time)

kbr_node_inf <- kbr_node_inf %>%
  filter(time > 0)

plot(depth ~ conc_1_ns_1, data = subset(kbr_node_inf, time == 1334),
     type = "l")
mapply(function(x,c) lines(depth ~ conc_1_ns_1, data = subset(kbr_node_inf, time == x), col = c),
       x = unique(kbr_node_inf$time),
       c = hcl.colors(365))

kbr_inputs <- read_atmosph_in("inst/hydrus_output_fairfield_kbr")

sas_input_kbr <- get_sas_input("inst/hydrus_output_fairfield_kbr",
                               depths = c(0, 100),
                               times = c(1334, 1698),
                               node_spacing = 1.75)
plot(S ~ t, sas_input_kbr, type = "l", ylim = c(0, 26))
lines(J ~ t, sas_input_kbr, col = "blue")
lines(ET ~ t, sas_input_kbr, col = "forestgreen")
lines(Q ~ t, sas_input_kbr, col = "red")

particle_ages <- read_part_age("inst/hydrus_output_one_year")


plot(age~time, data = subset(particle_ages, particle_index == "1"))

plot(age~particle_index, data = subset(particle_ages, time == 555.0000),
     type = "o")

one_yr <- storage_age_distribution("inst/hydrus_output_one_year", times = 365)




ts1 <- subset(node_output, time == 497)




plot(depth ~ moisture, data = subset(node_output, depth > -150 & time == 497), type = "l", xlim = c(0.1,0.5))
mapply(function(t,c) lines(depth ~ moisture, data = subset(node_output, depth > -150 & time == t), type = "l", col = c),
       t = 498:861,
       c = rainbow(364))

## total storage of soil column over time:
node_output <- read_nod_inf("inst/hydrus_output_one_year_more_particles")
soil_column <- subset(node_output, depth > -150 & time > 400)

delta_depth <- 1.75

total_moisture <- numeric(365)
for(j in unique(soil_column$time)){
  one_time <- soil_column[soil_column$time == j,]

  mean_moisture <- numeric(nrow(one_time))
  for(i in 1:(nrow(one_time)-1)){
    mean_moisture[i] <- mean(c(one_time$moisture[i], one_time$moisture[i+1]))
  }

  total_moisture[j-496] <- sum(mean_moisture * delta_depth)
}

plot(total_moisture, type = "l")


## c = water age
soil_column[soil_column$time == 497, "c"] * total_moisture[1]


## Q out: deep percolation:
plot(node_output[node_output$node == 87 & node_output$time > 400, "flux"], type = "l")
lines(node_output[node_output$node == 84 & node_output$time > 400, "flux"], col = "red")

## Q in :
a_level <- read_a_level("inst/hydrus_output_one_year_more_particles")
plot(a_level$sum_v_top, type = "l")
plot(a_level$sum_v_root, type = "l")
plot(a_level$sum_v_bot, type = "l")

plot(node_output[node_output$node == 1 & node_output$time > 400, "flux"], type = "l")

library(readr)
inputs <- read_fwf("inst/hydrus_output_one_year_more_particles/ATMOSPH.IN",
                   skip=9,
                   col_positions = fwf_positions(start = c(8,19,29,40,53,70,82,94),
                                                 end = c(11,23,35,47,59,71,83,NA)))
inputs <- inputs[1:861,]


colnames(inputs) <- c("tAtm", "Prec", "rSoil", "rRoot", "hCritA", "rB", "hB", "ht")

plot(inputs$Prec[497:861], type = "l")
plot(inputs$rSoil[497:861], type = "l", ylim = c(0,0.8))
lines(inputs$rRoot[497:861], type = "l", col = "orange")

ET <- as.numeric(inputs$rSoil) + as.numeric(inputs$rRoot)
plot(ET, type = "l")

plot(inputs$Prec, type = "l", col = "blue")
lines(ET, type = "l", col = "green3")

## uptake
uptake <- read_uptake("inst/hydrus_output_one_year_more_particles")

plot(uptake[uptake$time > 496, "uptake"], type = "l")

## age-ranked storage:
particles <- read_particle_tracking("inst/hydrus_output_one_year_more_particles")

particles_year <- particles[particles$time >=  497,]

t1 <- unique(particles_year$time)[1]

plot(particles_year[particles_year$time == t1,"age"], type = "l")

plot(age ~ I(abs(depth)), data = particles_year[particles_year$time == t1,],
     type = "o")

particles_t1 <- particles_year[particles_year$time == t1,]

particles_t1$age * abs(particles_t1$depth)

cumsum(abs(particles_t1$depth))


plot(total_moisture, type = "o")
points(unique(particles_year$time) - 497, rep(40, times = length(unique(particles_year$time))))



sas_water_input <- get_sas_input(hydrus_output_path = "inst/hydrus_output_one_year_more_particles",
              depths = c(0,8.75),
              times = c(497, 861),
              node_spacing = 1.75)

plot(with(sas_water_input, S +J - ET +Q),
     type = "l")
lines(sas_water_input$S, col = "blue")

# Material 1 0-8.75
# Material 2 10.5-150.5




