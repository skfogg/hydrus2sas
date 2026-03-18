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



get_sas_input(hydrus_output_path = "inst/hydrus_output_one_year_more_particles",
              depths = c(0,150),
              times = c(497, 861),
              node_spacing = 1.75)
