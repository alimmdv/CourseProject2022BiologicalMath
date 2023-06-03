library(pacman)
library(pracma)
p_load(tidyverse)
library(tidyverse)
library(MathBioSim)
library(plot3D)

# Define the range of standardized radii 
sigma_m_values <- round(logspace(log10(0.001), log10(0.1), 11), digits = 4)
sigma_w_values <- round(logspace(log10(0.001), log10(0.1), 11), digits = 4)


# Initialize an empty data frame to store results
results <- data.frame()

# Loop over the parameter space
for(sigma_m in sigma_m_values) {
  for(sigma_w in sigma_w_values) {
    sim <- initialize_simulator(
      area_length_x = 100, 
      dd = 0.01,
      initial_population_x = c(10),
      death_r = 3,
      death_y = dnorm(seq(0,5,length.out = 1001), sd = sigma_w), # assuming sigma_w represents the competition radius
      birth_ircdf_y = qnorm(seq(0.5,1-1e-6,length.out = 101), sd = sigma_m), # assuming sigma_m represents the dispersal radius
      realtime_limit = 30
    )
    dd <- 0.01
    lambda = 1 / dd  # Calculate lambda
    
    n = 1 # Assuming n is the dimensionality of the habitat, and it's a 1D habitat in this case.
    
    # Calculate standardized radii
    standardized_sigma_m = sigma_m * (lambda^(1/n))
    standardized_sigma_w = sigma_w * (lambda^(1/n))
    
    sim_results <- run_simulation(sim, 4000, calculate.pcf = TRUE)
    final_population = sim_results$population$pop[length(sim_results$population$pop)]  # Get final population
    carrying_capacity = final_population / lambda  # Calculate carrying capacity
    
    results <- rbind(results, data.frame(
      StDispersalRadius = standardized_sigma_m,
      StCompetitionRadius = standardized_sigma_w,
      StCarryingCapacity = carrying_capacity  # Use the new definition of carrying capacity
    ))
  }
}

View(results)

ggplot(results, aes(x = StDispersalRadius, y = StCompetitionRadius)) +
  geom_raster(aes(fill = StCarryingCapacity)) +
  scale_color_gradientn(colours = rainbow(5)) +
  theme_minimal() +
  labs(fill = "Standardized Carrying Capacity",
       x = "Standardized Dispersal Radius",
       y = "Standardized Competition Radius",
       title = "Heatmap of Standardized Carrying Capacity for 1D") +
  scale_x_log10() + 
  scale_y_log10() 

