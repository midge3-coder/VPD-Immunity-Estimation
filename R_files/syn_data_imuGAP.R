library(imuGAP)
library(data.table)
library(plyr)
library(ggplot2)

# Consistent report styling for coverage plots.
coverage_theme <- function(base_size = 16) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 3),
      plot.subtitle    = element_text(color = "gray35", size = base_size - 1),
      axis.title       = element_text(face = "bold"),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

county_colors <- c(
  "Scruggs" = "#1b9e77",
  "Simone"  = "#d95f02",
  "Watson"  = "#7570b3"
)

#step 1: load the data

# Start from imuGAP's built-in synthetic fixture to learn the expected schemas.
data("locations_sim", package = "imuGAP")
head(locations_sim)

# Canonicalize and validate
canonical_locations <- canonicalize_locations(locations_sim)
head(canonical_locations)

data("observations_sim", package = "imuGAP")
head(observations_sim[, .(obs_id, loc_id, positive, sample_n, censored)])

# Canonicalize and validate
canonical_observations <- canonicalize_observations(observations_sim)
head(canonical_observations)

data("populations_sim", package = "imuGAP")
head(populations_sim)

# Canonicalize and validate
canonical_populations <- canonicalize_populations(
  populations_sim, observations_sim, locations_sim
)
head(canonical_populations)
# Define base directory (change as needed; here assumes script run from project root, or set to your data root)
base_dir <- file.path("data")
derived_dir <- file.path(base_dir, "derived")




data("locations_sim", package = "imuGAP")
head(locations_sim)

# Canonicalize and validate
canonical_locations <- canonicalize_locations(locations_sim)
head(canonical_locations)

# Reuse the package fixture as the working "nc_*" inputs for this baseline fit.
nc_locations <- locations_sim
nc_observations <- observations_sim
nc_populations <- populations_sim

canonical_locations <-canonicalize_locations(nc_locations)

canonical_observations <- canonicalize_observations(nc_observations)

canonical_populations <- canonicalize_populations(populations = nc_populations, observations = nc_observations, locations = nc_locations)




### fit the model


fit_sim <- sampling(
  
  nc_observations,
  nc_populations,
  nc_locations,
  stan_opts = stan_options(
    iter = 2000, chains = 4, refresh = 0, seed = 1L
  )
)

## Save the fitted model to an RDS file

saveRDS(fit_sim, "fit_sim2.rds")

##target_simulation

# Build prediction targets for all hierarchy levels, ages, and doses.
target_sim <- create_target(
  fit = fit_sim, location = unique(nc_locations$loc_id), age = 1:18,
  cohort = max(nc_populations$cohort) - 18, dose = c(1, 2), mode = "snapshot"
)
head(target_sim)

nc_observations

##prediction 

predict_sim <- predict(object = fit_sim, target = target_sim, posterior_size = 100)



# Calculate the posterior mean coverage probability for each location and dose at age 5
summary_predict <- summary(predict_sim)
head(summary_predict)

mcv1_baseline <- subset(summary_predict, dose == 1)
# Save the one-dose baseline for downstream comparison with reduced-coverage scenarios.
write.csv(mcv1_baseline, "mcv1_coverage_baseline.csv", row.names = FALSE)

summary_predict |>
  subset(loc_id == "State" & dose == 1 & age > 4) |>
  ggplot() +
  aes(x = age) +
  geom_ribbon(aes(ymin = q2_5, ymax = q97_5, fill = "95% credible interval"),
              alpha = 0.22, color = NA) +
  geom_line(aes(y = q50, color = "Median"), linewidth = 1.3) +
  scale_x_continuous(breaks = 5:18, minor_breaks = NULL) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  scale_color_manual(NULL, values = c("Median" = "#1b9e77")) +
  scale_fill_manual(NULL, values = c("95% credible interval" = "#1b9e77")) +
  coverage_theme() +
  labs(
    x = "Age", y = "Coverage",
    title = "State-level MCV1 coverage",
    subtitle = "Median posterior estimate with 95% credible interval"
  )

summary_predict |>
  subset(loc_id %in% c("Scruggs", "Simone", "Watson") & dose == 1 & age > 4) |>
  ggplot() +
  aes(x = age) +
  geom_ribbon(aes(ymin = q2_5, ymax = q97_5, fill = loc_id), alpha = 0.16, color = NA) +
  geom_line(aes(y = q50, color = loc_id), linewidth = 1.25) +
  scale_x_continuous(breaks = 5:18, minor_breaks = NULL) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  scale_color_manual("County", values = county_colors, aesthetics = c("color", "fill")) +
  coverage_theme() +
  labs(
    x = "Age", y = "Coverage",
    title = "County-level MCV1 coverage",
    subtitle = "Median posterior estimate with uncertainty ribbons"
  )

  scruggs_schools <- locations_sim[parent_id == "Scruggs", loc_id]
summary_predict |>
  subset(
    loc_id %in% scruggs_schools & dose == 1 & age > 4
  ) |>
  ggplot() +
  geom_boxplot(aes(x = factor(age), y = q50),
               fill = "#9ecae1", color = "#08519c",
               linewidth = 0.6, outlier.alpha = 0.6) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  coverage_theme() +
  labs(
    x = "Age", y = "Median coverage",
    title = "School-level MCV1 coverage in Scruggs County",
    subtitle = "Distribution across schools by age"
  )


  schools <- c(
  "Towhee Children's Academy", # ~380 per grade
  "Flycatcher Elementary", # ~110 per grade
  "Sparrow School" # ~60 per grade
)

# Subset to targets of interest (all retained posterior draws)
predict_sub <- predict_sim |>
  subset(loc_id %in% schools & dose == 1 & age > 4)

# Get the pre-computed background coverage matching the subsetted target
latent_ref <- copy(predict_sub$target)
latent_ref$coverage <- latent_params_sim$coverage[predict_sub$target$obs_id]


# Convert predictions to a long-format data.frame
draws_df <- as.data.frame(predict_sub)

# Now plot it all
ggplot() +
  aes(age, coverage, color = loc_id) +
  geom_point(
    data = draws_df,
    alpha = 0.08, shape = 16, size = 1.3,
    position = position_jitterdodge(
      dodge.width = 0.5,
      jitter.width = 0.15
    )
  ) +
  geom_point(
    data = latent_ref,
    mapping = aes(shape = "Latent true value"),
    fill = "white",
    color = "black",
    size = 3,
    stroke = 1.1
  ) +
  scale_shape_manual(
    name = "",
    values = c("Latent true value" = 24)
  ) +
  scale_color_discrete(NULL, aesthetics = c("color", "fill")) +
  scale_x_continuous(breaks = 5:18, minor_breaks = NULL) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  coverage_theme() +
  labs(
    color = "School", x = "Age", y = "Coverage",
    title = "Posterior draws for selected schools",
    subtitle = "Jittered draws with latent true values highlighted"
  )