setwd("C:\\Users\\anupd\\OneDrive\\Documents\\5 Semester\\MA 568\\Project")
install.packages("sf")
library(sf)
library(ggplot2)
library(dplyr)
install.packages("MASS")
library(MASS)
install.packages("spatstat")
install.packages("spatstat.geom")
install.packages("spatstat.core")
library(spatstat)
library(spatstat.geom)

mse <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    stop("Arrays must have the same length.")
  }
  
  mean((actual - predicted)^2)
}




starbucks_data <- read.csv('directory.csv')
starbucks_data <- as.data.frame(starbucks_data)

starbucks_data <- starbucks_data[starbucks_data$City == "London",]
starbucks_data[starbucks_data$Latitude < 51.85,] -> starbucks_data



shapefile_data <- st_read("GBR_adm2.shp")

plot(st_geometry(shapefile_data))

localAuthoritiesLondon = c("Barking and Dagenham","Barnet","Bexley","Brent","Bromley","Camden","Croydon","Ealing","Enfield",
                           "Greenwich","Hackney","Hammersmith and Fulham","Haringey","Harrow","Havering","Hillingdon","Hounslow","Islington",
                           "Kensington and Chelsea","Kingston upon Thames","Lambeth","Lewisham","Merton","Newham","Redbridge","Richmond upon Thames",
                           "Southwark","Sutton","Tower Hamlets","Waltham Forest","Wandsworth","Westminster")

shapefile_data[which(shapefile_data$NAME_2 %in% localAuthoritiesLondon),] -> shapeLondon


plot(shapeLondon, col="lightblue")



starbucks_sf <- st_as_sf(starbucks_data, coords = c("Longitude", "Latitude"), crs = st_crs(shapefile_data))
ggplot() +
  geom_sf(data = shapeLondon, fill = "lightblue", color = "black") +  # Plot London map
  geom_sf(data = starbucks_sf, color = "darkgreen", size = 2) +           # Plot Starbucks locations
  labs(title = "Starbucks Locations in London", x = "Longitude", y = "Latitude") +
  theme_minimal()


# Remove London Background and convert longitude and latitude to 0-n scale of units distance. 

# Define the original range
min_value <- min(starbucks_data$Latitude)
max_value <- max(starbucks_data$Latitude)

starbucks_data$Scaled_Lat <- (starbucks_data$Latitude - min_value) / (max_value - min_value) * 10

min_value <- min(starbucks_data$Longitude)
max_value <- max(starbucks_data$Longitude)

starbucks_data$Scaled_Long <- (starbucks_data$Longitude - min_value) / (max_value - min_value) * 10

head(starbucks_data)

# Basic scatter plot
plot(starbucks_data$Scaled_Long, starbucks_data$Scaled_Lat, 
     col = "darkgreen", pch = 16, 
     xlab = "Scaled Longitude", ylab = "Scaled Latitude", 
     main = "Starbucks Locations")
grid()



#Computing Lambda Poisson Homogenous


count_neighbors_scaled <- function(index, data, radius = 1) {
  # Calculate Euclidean distance from the current point to all other points
  distances <- sqrt((data$Scaled_Long - data$Scaled_Long[index])^2 + 
                      (data$Scaled_Lat - data$Scaled_Lat[index])^2)
  # Count the number of points within the radius (excluding itself)
  sum(distances <= radius & distances > 0)
}

# Apply the function to each row in the starbucks data
starbucks_data$Neighbor_Count <- sapply(1:nrow(starbucks_data), 
                                   count_neighbors_scaled, 
                                   data = starbucks_data, 
                                   radius = 1)

lambda <- mean(starbucks_data$Neighbor_Count)

#We can declare a potential lambda rate function as distance till nearest Starbucks. This analysis will happen later

#We can declare a potential lambda rate function therefore as 30.38 Starbucks per circle of radius 1 around the location.
#That is what is shown immedialy below


observed_counts <- starbucks_data$Neighbor_Count

#Simulate expected neighbor counts using Weibull
set.seed = (123)
expected_counts <- rweibull(length(observed_counts), shape = 2, scale = lambda)


#Create a Q-Q plot
qq_data <- data.frame(
  Observed = sort(observed_counts),
  Expected = sort(expected_counts)
)



# Q-Q Plot with ggplot2
ggplot(qq_data, aes(x = Expected, y = Observed)) +
  geom_point(color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Starbucks Q-Q Plot: Observed Neighbor Counts vs. Weibull Simulated",
       x = "Theoretical Quantiles (Weibull)",
       y = "Observed Quantiles") +
  theme_minimal()


mse(sort(observed_counts), sort(expected_counts))


#This was all lambda as per how many are within 1 radius. Finish with 3d plot


library(plotly)

grid_resolution <- 100  # Number of points per axis

# Generate a grid over the scaled long and lat ranges
long_seq <- seq(min(starbucks_data$Scaled_Long), max(starbucks_data$Scaled_Long), length.out = grid_resolution)
lat_seq <- seq(min(starbucks_data$Scaled_Lat), max(starbucks_data$Scaled_Lat), length.out = grid_resolution)

# Create an empty matrix to store lambda values
lambda_grid <- matrix(0, nrow = grid_resolution, ncol = grid_resolution)

# Function to calculate neighbor counts for a given grid point
calculate_lambda <- function(grid_long, grid_lat, data, radius = 1) {
  distances <- sqrt((data$Scaled_Long - grid_long)^2 + (data$Scaled_Lat - grid_lat)^2)
  sum(distances <= radius)
}

# Calculate lambda for each grid point
for (i in seq_along(long_seq)) {
  for (j in seq_along(lat_seq)) {
    lambda_grid[i, j] <- calculate_lambda(long_seq[i], lat_seq[j], starbucks_data)
  }
}

p <- plot_ly(
  x = long_seq,
  y = lat_seq,
  z = lambda_grid,
  type = "surface",
  colors = colorRamp(c("blue", "green", "yellow", "red"))
)

p <- p %>%
  layout(
    title = list(text = "Starbucks 3D Lambda (Neighbor Counts) Intensity Field"),
    scene = list(
      xaxis = list(title = "Scaled Longitude"),
      yaxis = list(title = "Scaled Latitude"),
      zaxis = list(title = "Lambda (Neighbor Counts)")
    )
  )

p


# Normalize the lambda grid so that its total sum equals 1
lambda_grid_normalized <- lambda_grid / sum(lambda_grid)

extract_lambda <- function(long, lat, grid_long, grid_lat, lambda_grid) {
  long_index <- which.min(abs(grid_long - long))
  lat_index <- which.min(abs(grid_lat - lat))
  lambda_grid[long_index, lat_index]
}

# Apply the function just amde above to each Starbucks location
starbucks_data$Lambda_Observed <- mapply(
  extract_lambda,
  long = starbucks_data$Scaled_Long,
  lat = starbucks_data$Scaled_Lat,
  MoreArgs = list(grid_long = long_seq, grid_lat = lat_seq, lambda_grid = lambda_grid_normalized)
)

# Compute the total probability as the product of lambda values at observed locations
mean_probability <- mean(starbucks_data$Lambda_Observed)

print(paste("Total Probability of Observed Points under the Normalized Lambda Field:", mean_probability))






#Also do lambda based on how far till nearest store and lambda can be average how far till nearest store: 

compute_nearest_distance <- function(index, data) {
  # Calculate Euclidean distance from the current point to all others
  distances <- sqrt((data$Scaled_Long - data$Scaled_Long[index])^2 +
                      (data$Scaled_Lat - data$Scaled_Lat[index])^2)
  # Return the minimum distance (excluding 0, the distance to itself)
  min(distances[distances > 0])
}

# Apply the function to each row in the statbucks data
starbucks_data$Nearest_Distance <- sapply(1:nrow(starbucks_data), 
                                     compute_nearest_distance, 
                                     data = starbucks_data)


mean(starbucks_data$Nearest_Distance)


#Compute the mean of nearest distances (to be used as lambda)
lambda <- mean(starbucks_data$Nearest_Distance)

# Generate expected counts using the Weibull distribution (k=2, scale=lambda)
set.seed(123)
expected_counts <- rweibull(length(starbucks_data$Nearest_Distance), shape = 2, scale = lambda)


# Prepare the data for Q-Q plot
qq_data <- data.frame(
  Observed = sort(starbucks_data$Nearest_Distance),
  Expected = sort(expected_counts)
)

# Q-Q plot
ggplot(qq_data, aes(x = Expected, y = Observed)) +
  geom_point(color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Starbucks Q-Q Plot: Observed Nearest Distances vs. Weibull Modeled",
       x = "Theoretical Quantiles (Weibull)",
       y = "Observed Quantiles") +
  theme_minimal()


mse(sort(starbucks_data$Nearest_Distance), sort(expected_counts))


# Visualize the fit with a histogram
hist(starbucks_data$Nearest_Distance, breaks = 20, probability = TRUE, 
     col = "lightblue", main = "Nearest Starbucks Distances with Weibull Fit", 
     xlab = "Distance")

#Add Weibulll
curve(dweibull(x, shape = 2, scale = lambda), 
      col = "red", lwd = 2, add = TRUE)




#Papangelou Calculations

mu <- mean(starbucks_data$Neighbor_Count) 
alpha <- 2                                

# Compute pairwise distances
library(stats) 
scaled_coords <- as.matrix(starbucks_data[, c("Scaled_Long", "Scaled_Lat")])
pairwise_distances <- as.matrix(dist(scaled_coords)) 

print(pairwise_distances)

interaction_matrix <- exp(-alpha * pairwise_distances)

diag(interaction_matrix) <- 0

# Compute Papangelou intensity
lambda_papangelou <- mu + rowSums(interaction_matrix)

print(rowSums(interaction_matrix))

starbucks_data$Lambda_Papangelou <- lambda_papangelou


# Map of Starbucks locations
ggplot(starbucks_data, aes(x = Scaled_Long, y = Scaled_Lat)) +
  geom_point(aes(size = Lambda_Papangelou), color = "darkgreen", alpha = 0.8) +
  scale_size_continuous(range = c(2, 10), name = "Papangelou Intensity") + # Adjust dot size range
  labs(
    title = "Starbucks Locations with Papangelou Conditional Intensity",
    x = "Scaled Longitude",
    y = "Scaled Latitude"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_equal()


# Map of Starbucks locations with color scale for Papangelou Intensity
ggplot(starbucks_data, aes(x = Scaled_Long, y = Scaled_Lat)) +
  geom_point(aes(color = Lambda_Papangelou), size = 3, alpha = 0.8) +
  scale_color_gradient(low = "lightgreen", high = "red", name = "Papangelou Intensity") +
  labs(
    title = "Starbucks Locations with Papangelou Conditional Intensity",
    x = "Scaled Longitude",
    y = "Scaled Latitude"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_equal()

#QQ Plot 

qq_data <- data.frame(
  Observed = sort(starbucks_data$Neighbor_Count),
  Papangelou = sort(starbucks_data$Lambda_Papangelou)
)


ggplot(qq_data, aes(x = Papangelou, y = Observed)) +
  geom_point(color = "darkgreen", size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Starbucks Q-Q Plot: Observed vs. Papangelou Conditional Intensity",
       x = "Papangelou Intensity (Theoretical)",
       y = "Observed Neighbor Count (Empirical)") +
  theme_minimal()

mse(sort(starbucks_data$Neighbor_Count), sort(starbucks_data$Lambda_Papangelou))



#Instead of Neighbors Number vs Papangelou, Let us find Nearest Neighbor using lambda Papangelou

mu <- mean(starbucks_data$Nearest_Distance) 
alpha <- 2                                 

library(stats) 
scaled_coords <- as.matrix(starbucks_data[, c("Scaled_Long", "Scaled_Lat")])
pairwise_distances <- as.matrix(dist(scaled_coords)) 

print(pairwise_distances)

interaction_matrix <- exp(-alpha * pairwise_distances)

diag(interaction_matrix) <- 0

lambda_papangelou <- mu + rowSums(interaction_matrix)

print(rowSums(interaction_matrix))

starbucks_data$Lambda_Papangelou <- lambda_papangelou


#Perform the calculation to find the dist to 1 Starbucks 

starbucks_data$ProjectedNNDist <- 1 / sqrt(starbucks_data$Lambda_Papangelou * pi)


sorted_actual_NN <- sort(starbucks_data$Nearest_Distance)
sorted_projected_NN <- sort(starbucks_data$ProjectedNNDist)

qq_data <- data.frame(
  Actual = sorted_actual_NN,
  Projected = sorted_projected_NN
)

# Q-Q plot
ggplot(qq_data, aes(x = Projected, y = Actual)) +
  geom_point(color = "darkgreen", size = 2) + 
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Starbucks: Q-Q Plot: Actual vs Projected Nearest Neighbor Distances",
    x = "Projected Nearest Neighbor Distances",
    y = "Actual Nearest Neighbor Distances"
  ) +
  theme_minimal()


mse(sorted_actual_NN, sorted_projected_NN)


#Papangelou Across the Space

library(plotly)

grid_resolution <- 100  

long_seq <- seq(0, 10, length.out = grid_resolution)
lat_seq <- seq(0, 10, length.out = grid_resolution)

lambda_field <- matrix(0, nrow = grid_resolution, ncol = grid_resolution)

for (i in seq_along(long_seq)) {
  for (j in seq_along(lat_seq)) {
    grid_point <- c(long_seq[i], lat_seq[j])
    distances <- sqrt((starbucks_data$Scaled_Lat - grid_point[2])^2 + 
                        (starbucks_data$Scaled_Long - grid_point[1])^2)
    lambda_field[i, j] <- sum(1 / (1 + distances))
  }
}

# Create a 3D  plot
plot_ly(
  x = long_seq, 
  y = lat_seq, 
  z = lambda_field, 
  type = "surface",
  colors = colorRamp(c("blue", "green", "yellow", "red"))
) %>%
  layout(
    title = "3D Papangelou Intensity Field",
    scene = list(
      xaxis = list(title = "Scaled Longitude"),
      yaxis = list(title = "Scaled Latitude"),
      zaxis = list(title = "Lambda (Papangelou Intensity)")
    )
  )

# Normalize the lambda field so that the sum over all grid points equals 1
lambda_field_normalized <- lambda_field / sum(lambda_field)

extract_papangelou_lambda <- function(long, lat, grid_long, grid_lat, lambda_field) {
  long_index <- which.min(abs(grid_long - long))
  lat_index <- which.min(abs(grid_lat - lat))
  lambda_field[long_index, lat_index]
}


#Apply the function to each Starbucks location
starbucks_data$Lambda_Papangelou <- mapply(
  extract_papangelou_lambda,
  long = starbucks_data$Scaled_Long,
  lat = starbucks_data$Scaled_Lat,
  MoreArgs = list(grid_long = long_seq, grid_lat = lat_seq, lambda_field = lambda_field_normalized)
)

# Compute the total probability as the product of lambda values at observed locations
mean_probability_papangelou <- mean(starbucks_data$Lambda_Papangelou)

print(paste("Total Probability of Observed Points under the Papangelou Intensity Field:", mean_probability_papangelou))




#Matern's Papangelou Calculations



r_hardcore <- 1  

starbucks_data_sim <- starbucks_data

starbucks_data_sim <- starbucks_data_sim[order(starbucks_data_sim$Lambda_Papangelou, decreasing = TRUE), ]

accepted_points <- list()

for (i in seq_len(nrow(starbucks_data_sim))) {
  point <- starbucks_data_sim[i, ]
  distances <- sqrt((point$Scaled_Lat - unlist(lapply(accepted_points, `[[`, "Scaled_Lat")))^2 +
                      (point$Scaled_Long - unlist(lapply(accepted_points, `[[`, "Scaled_Long")))^2)
  if (all(distances >= r_hardcore, na.rm = TRUE)) {
    accepted_points <- append(accepted_points, list(point))
  }
}

accepted_points_df <- do.call(rbind, accepted_points)

par(mfrow = c(1, 2))

# Original points
plot(starbucks_data$Scaled_Long, starbucks_data$Scaled_Lat, 
     col = "darkgreen", pch = 16, 
     xlab = "Scaled Longitude", ylab = "Scaled Latitude",
     main = "Original Starbucks Locations")
grid()

# Simulated Matern's Papangelou Hardcore Process
plot(accepted_points_df$Scaled_Long, accepted_points_df$Scaled_Lat, 
     col = "blue", pch = 16, 
     xlab = "Scaled Longitude", ylab = "Scaled Latitude",
     main = "Matern's Papangelou Hardcore Process")
grid()

print(length(accepted_points))









#Begin Pub Data Analysis










pubs_data <- read.csv('open_pubs.csv')
pubs_data <- as.data.frame(pubs_data)


pubs_data %>% filter(local_authority %in% localAuthoritiesLondon) -> pubs_data

pubs_data$latitude <- as.numeric(as.character(pubs_data$latitude))  
pubs_data$longitude <- as.numeric(as.character(pubs_data$longitude))  

pubs_data <- pubs_data[!is.na(pubs_data$latitude), ]


shapefile_data <- st_read("GBR_adm2.shp")
# Basic plot
plot(st_geometry(shapefile_data))

localAuthoritiesLondon = c("Barking and Dagenham","Barnet","Bexley","Brent","Bromley","Camden","Croydon","Ealing","Enfield",
                           "Greenwich","Hackney","Hammersmith and Fulham","Haringey","Harrow","Havering","Hillingdon","Hounslow","Islington",
                           "Kensington and Chelsea","Kingston upon Thames","Lambeth","Lewisham","Merton","Newham","Redbridge","Richmond upon Thames",
                           "Southwark","Sutton","Tower Hamlets","Waltham Forest","Wandsworth","Westminster")

shapefile_data[which(shapefile_data$NAME_2 %in% localAuthoritiesLondon),] -> shapeLondon


plot(shapeLondon, col="lightblue")



pubs_sf<- st_as_sf(pubs_data, coords = c("longitude", "latitude"), crs = st_crs(shapefile_data))
ggplot() +
  geom_sf(data = shapeLondon, fill = "lightblue", color = "black") +  # Plot London map
  geom_sf(data = pubs_sf, color = "red", size = 2) +           # Plot Starbucks locations
  labs(title = "Pubs Locations in London", x = "Longitude", y = "Latitude") +
  theme_minimal()



min_value <- min(pubs_data$latitude)
max_value <- max(pubs_data$latitude)

print(pubs_data$latitude)

pubs_data$Scaled_Lat <- (pubs_data$latitude - min_value) / (max_value - min_value) * 10

min_value <- min(pubs_data$longitude)
max_value <- max(pubs_data$longitude)

pubs_data$Scaled_Long <- (pubs_data$longitude - min_value) / (max_value - min_value) * 10

head(pubs_data)

plot(pubs_data$Scaled_Long, pubs_data$Scaled_Lat, 
     col = "red", pch = 16, 
     xlab = "Scaled Longitude", ylab = "Scaled Latitude", 
     main = "Pubs Locations")

grid()


#Computing Lambda Poisson Homogenous


# Function to count points within a radius for each point
count_neighbors_scaled <- function(index, data, radius = 1) {
  # Calculate Euclidean distance from the current point to all other points
  distances <- sqrt((data$Scaled_Long - data$Scaled_Long[index])^2 + 
                      (data$Scaled_Lat - data$Scaled_Lat[index])^2)
  # Count the number of points within the radius (excluding itself)
  sum(distances <= radius & distances > 0)
}

# Apply the function to each row in the dataset
pubs_data$Neighbor_Count <- sapply(1:nrow(pubs_data), 
                                        count_neighbors_scaled, 
                                        data = pubs_data, 
                                        radius = 1)

lambda <- mean(pubs_data$Neighbor_Count)

#We can declare a potential lambda rate function therefore as 0.04 distance till nearest pub. Analysis for this done later

#We can declare a potential lambda rate function therefore as 424.4 Pubs per circle of radius 1 around the location.
#That is what is shown immedialy below


observed_counts <- pubs_data$Neighbor_Count

# Simulate expected neighbor counts using Weibull
set.seed = (123)
expected_counts <- rweibull(length(observed_counts), shape = 2, scale = lambda)


#Create a Q-Q plot
qq_data <- data.frame(
  Observed = sort(observed_counts),
  Expected = sort(expected_counts)
)



ggplot(qq_data, aes(x = Expected, y = Observed)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Pubs Q-Q Plot: Observed Neighbor Counts vs. Weibull Simulated",
       x = "Theoretical Quantiles (Weibull)",
       y = "Observed Quantiles") +
  theme_minimal()


mse(sort(observed_counts), sort(expected_counts))


#This was all lambda as per how many are within 1 radius

#3D Plot Creation for this model

library(plotly)

grid_resolution <- 100  

long_seq <- seq(min(pubs_data$Scaled_Long), max(pubs_data$Scaled_Long), length.out = grid_resolution)
lat_seq <- seq(min(pubs_data$Scaled_Lat), max(pubs_data$Scaled_Lat), length.out = grid_resolution)

lambda_grid <- matrix(0, nrow = grid_resolution, ncol = grid_resolution)

calculate_lambda <- function(grid_long, grid_lat, data, radius = 1) {
  distances <- sqrt((data$Scaled_Long - grid_long)^2 + (data$Scaled_Lat - grid_lat)^2)
  sum(distances <= radius)
}

for (i in seq_along(long_seq)) {
  for (j in seq_along(lat_seq)) {
    lambda_grid[i, j] <- calculate_lambda(long_seq[i], lat_seq[j], pubs_data)
  }
}

p <- plot_ly(
  x = long_seq,
  y = lat_seq,
  z = lambda_grid,
  type = "surface",
  colors = colorRamp(c("blue", "green", "yellow", "red"))
)

p <- p %>%
  layout(
    title = list(text = "Pubs 3D Lambda (Neighbor Counts) Intensity Field"),
    scene = list(
      xaxis = list(title = "Scaled Longitude"),
      yaxis = list(title = "Scaled Latitude"),
      zaxis = list(title = "Lambda (Neighbor Counts)")
    )
  )

p


# Normalize the lambda grid so that its total sum equals 1
lambda_grid_normalized <- lambda_grid / sum(lambda_grid)

extract_lambda <- function(long, lat, grid_long, grid_lat, lambda_grid) {
  long_index <- which.min(abs(grid_long - long))
  lat_index <- which.min(abs(grid_lat - lat))
  lambda_grid[long_index, lat_index]
}

# Apply the function to each Starbucks location
pubs_data$Lambda_Observed <- mapply(
  extract_lambda,
  long = pubs_data$Scaled_Long,
  lat = pubs_data$Scaled_Lat,
  MoreArgs = list(grid_long = long_seq, grid_lat = lat_seq, lambda_grid = lambda_grid_normalized)
)

# Compute the total probability as the product of lambda values at observed locations
mean_probability <- mean(pubs_data$Lambda_Observed)

print(paste("Total Probability of Observed Points under the Normalized Lambda Field:", mean_probability))






#Also do lambda based on how far till nearest store and lambda can be average how far till nearest store: 

compute_nearest_distance <- function(index, data) {
  # Calculate Euclidean distance from the current point to all others
  distances <- sqrt((data$Scaled_Long - data$Scaled_Long[index])^2 +
                      (data$Scaled_Lat - data$Scaled_Lat[index])^2)
  # Return the minimum distance (excluding 0, the distance to itself)
  min(distances[distances > 0])
}

# Apply the function to each row in the dataset
pubs_data$Nearest_Distance <- sapply(1:nrow(pubs_data), 
                                          compute_nearest_distance, 
                                          data = pubs_data)


mean(pubs_data$Nearest_Distance)


lambda <- mean(pubs_data$Nearest_Distance)

# Generate expected counts using the Weibull distribution (k=2, scale=lambda)
set.seed(123)
expected_counts <- rweibull(length(pubs_data$Nearest_Distance), shape = 2, scale = lambda)


#  Q-Q plot
qq_data <- data.frame(
  Observed = sort(pubs_data$Nearest_Distance),
  Expected = sort(expected_counts)
)

ggplot(qq_data, aes(x = Expected, y = Observed)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Pubs: Q-Q Plot: Observed Nearest Distances vs. Weibull Modeled",
       x = "Theoretical Quantiles (Weibull)",
       y = "Observed Quantiles") +
  theme_minimal()


mse(sort(pubs_data$Nearest_Distance), sort(expected_counts))


# Visualize the fit with a histogram
hist(pubs_data$Nearest_Distance, breaks = 20, probability = TRUE, 
     col = "lightblue", main = "Nearest Pub Distances with Weibull Fit", 
     xlab = "Distance")

# Plot Weibull distribution
curve(dweibull(x, shape = 2, scale = lambda), 
      col = "red", lwd = 2, add = TRUE)




#Papangelou Calculations

mu <- mean(pubs_data$Neighbor_Count) 
alpha <- 2                                

library(stats) 
scaled_coords <- as.matrix(pubs_data[, c("Scaled_Long", "Scaled_Lat")])
pairwise_distances <- as.matrix(dist(scaled_coords)) 

print(pairwise_distances)

interaction_matrix <- exp(-alpha * pairwise_distances)

diag(interaction_matrix) <- 0

lambda_papangelou <- mu + rowSums(interaction_matrix)

print(rowSums(interaction_matrix))

pubs_data$Lambda_Papangelou <- lambda_papangelou


# Map of Pubs locations
ggplot(pubs_data, aes(x = Scaled_Long, y = Scaled_Lat)) +
  geom_point(aes(size = Lambda_Papangelou), color = "darkgreen", alpha = 0.8) +
  scale_size_continuous(range = c(2, 10), name = "Papangelou Intensity") + # Adjust dot size range
  labs(
    title = "Pubs Locations with Papangelou Conditional Intensity",
    x = "Scaled Longitude",
    y = "Scaled Latitude"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_equal()


# Map of Pubs locations with color scale for Papangelou Intensity
ggplot(pubs_data, aes(x = Scaled_Long, y = Scaled_Lat)) +
  geom_point(aes(color = Lambda_Papangelou), size = 3, alpha = 0.8) +
  scale_color_gradient(low = "lightgreen", high = "red", name = "Papangelou Intensity") +
  labs(
    title = "Pubs Locations with Papangelou Conditional Intensity",
    x = "Scaled Longitude",
    y = "Scaled Latitude"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_equal()

#QQ Plot 

qq_data <- data.frame(
  Observed = sort(pubs_data$Neighbor_Count),
  Papangelou = sort(pubs_data$Lambda_Papangelou)
)


ggplot(qq_data, aes(x = Papangelou, y = Observed)) +
  geom_point(color = "blue", size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Pubs Q-Q Plot: Observed vs. Papangelou Conditional Intensity",
       x = "Papangelou Intensity (Theoretical)",
       y = "Observed Neighbor Count (Empirical)") +
  theme_minimal()

mse(sort(pubs_data$Neighbor_Count),sort(pubs_data$Lambda_Papangelou))



#Instead of Neighbors Number vs Papangelou, Let us find Nearest Neighbor using lambda Papangelou

mu <- mean(pubs_data$Nearest_Distance) 
alpha <- 2                                

library(stats)
scaled_coords <- as.matrix(pubs_data[, c("Scaled_Long", "Scaled_Lat")])
pairwise_distances <- as.matrix(dist(scaled_coords)) 

print(pairwise_distances)

interaction_matrix <- exp(-alpha * pairwise_distances)

diag(interaction_matrix) <- 0

lambda_papangelou <- mu + rowSums(interaction_matrix)

print(rowSums(interaction_matrix))

pubs_data$Lambda_Papangelou <- lambda_papangelou

#Perform calculation to find Dist to 1
pubs_data$ProjectedNNDist <- 1 / sqrt(pubs_data$Lambda_Papangelou * pi)


sorted_actual_NN <- sort(pubs_data$Nearest_Distance)
sorted_projected_NN <- sort(pubs_data$ProjectedNNDist)

#  Q-Q plot
qq_data <- data.frame(
  Actual = sorted_actual_NN,
  Projected = sorted_projected_NN
)

ggplot(qq_data, aes(x = Projected, y = Actual)) +
  geom_point(color = "blue", size = 2) + 
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Q-Q Plot: Actual vs Projected Nearest Neighbor Distances",
    x = "Projected Nearest Neighbor Distances",
    y = "Actual Nearest Neighbor Distances"
  ) +
  theme_minimal()


mse(sorted_actual_NN, sorted_projected_NN)


#Papangelou Across the Space

library(plotly)

grid_resolution <- 100  

long_seq <- seq(0, 10, length.out = grid_resolution)
lat_seq <- seq(0, 10, length.out = grid_resolution)

lambda_field <- matrix(0, nrow = grid_resolution, ncol = grid_resolution)

for (i in seq_along(long_seq)) {
  for (j in seq_along(lat_seq)) {
    grid_point <- c(long_seq[i], lat_seq[j])
    distances <- sqrt((pubs_data$Scaled_Lat - grid_point[2])^2 + 
                        (pubs_data$Scaled_Long - grid_point[1])^2)
    lambda_field[i, j] <- sum(1 / (1 + distances))
  }
}

# Create a 3D  plot
plot_ly(
  x = long_seq, 
  y = lat_seq, 
  z = lambda_field, 
  type = "surface",
  colors = colorRamp(c("blue", "green", "yellow", "red"))
) %>%
  layout(
    title = "3D Papangelou Intensity Field",
    scene = list(
      xaxis = list(title = "Scaled Longitude"),
      yaxis = list(title = "Scaled Latitude"),
      zaxis = list(title = "Lambda (Papangelou Intensity)")
    )
  )

# Normalize the lambda field so that the sum over all grid points equals 1
lambda_field_normalized <- lambda_field / sum(lambda_field)

# Function to extract normalized lambda values at specific Starbucks locations
extract_papangelou_lambda <- function(long, lat, grid_long, grid_lat, lambda_field) {
  long_index <- which.min(abs(grid_long - long))
  lat_index <- which.min(abs(grid_lat - lat))
  lambda_field[long_index, lat_index]
}


#Apply the function to each Puvb location
pubs_data$Lambda_Papangelou <- mapply(
  extract_papangelou_lambda,
  long = pubs_data$Scaled_Long,
  lat = pubs_data$Scaled_Lat,
  MoreArgs = list(grid_long = long_seq, grid_lat = lat_seq, lambda_field = lambda_field_normalized)
)

# Compute the total probability as the product of lambda values at observed locations
mean_probability_papangelou <- mean(pubs_data$Lambda_Papangelou)

print(paste("Total Probability of Observed Points under the Papangelou Intensity Field:", mean_probability_papangelou))




#Matern's Papangelou Calculations



r_hardcore <- 1  

pubs_data_sim <- pubs_data

pubs_data_sim <- pubs_data_sim[order(pubs_data_sim$Lambda_Papangelou, decreasing = TRUE), ]

accepted_points <- list()

for (i in seq_len(nrow(pubs_data_sim))) {
  point <- pubs_data_sim[i, ]
  distances <- sqrt((point$Scaled_Lat - unlist(lapply(accepted_points, `[[`, "Scaled_Lat")))^2 +
                      (point$Scaled_Long - unlist(lapply(accepted_points, `[[`, "Scaled_Long")))^2)
  if (all(distances >= r_hardcore, na.rm = TRUE)) {
    accepted_points <- append(accepted_points, list(point))
  }
}

accepted_points_df <- do.call(rbind, accepted_points)

par(mfrow = c(1, 2))

# Original points
plot(pubs_data_sim$Scaled_Long, pubs_data_sim$Scaled_Lat, 
     col = "red", pch = 16, 
     xlab = "Scaled Longitude", ylab = "Scaled Latitude",
     main = "Original Pubs Locations")
grid()

# Simulated Matern's Papangelou Hardcore Process
plot(accepted_points_df$Scaled_Long, accepted_points_df$Scaled_Lat, 
     col = "blue", pch = 16, 
     xlab = "Scaled Longitude", ylab = "Scaled Latitude",
     main = "Matern's Pap. Hardcore Process")
grid()

print(length(accepted_points))








