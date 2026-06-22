library(imuGAP)
library(data.table)
library(plyr)

#step 1: load the data


school <- read.csv("~/Downloads/MMEDGit/VPD-Immunity-Estimation/data/all-schools.csv")

nc_locations <- read.csv("~/Downloads/MMEDGit/VPD-Immunity-Estimation/data/derived/nc_locations.csv")

nc_observations <- read.csv("~/Downloads/MMEDGit/VPD-Immunity-Estimation/data/derived/nc_observations.csv")
setDT(nc_observations)

nc_populations <- read.csv("~/Downloads/MMEDGit/VPD-Immunity-Estimation/data/derived/nc_populations.csv")

canonical_locations <-canonicalize_locations(nc_locations)

canonical_observations <- canonicalize_observations(nc_observations)

canonical_populations <- canonicalize_populations(populations = nc_populations, observations = nc_observations, locations = nc_locations)


head(canonical_observations)

head(canonical_locations)

head(canonical_populations)

max_layer <- max(canonical_locations$layer)

head(nc_locations)

layer_4_list <- canonical_locations[layer == max_layer, loc_id]



### fit the model


fit_sim <- sampling(
  
  nc_observations,
  nc_populations,
  nc_locations,
  stan_opts = stan_options(
    iter = 2000, chains = 4, refresh = 0, seed = 1L
  )
)
canonical_observations <- canonicalize_observations(observations_sim)
head(canonical_observations)

nc_observations
