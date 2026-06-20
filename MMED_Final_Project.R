setwd() 
#maybe have to change this to the directory where the data is stored on your computer
school <- read.csv("~/Downloads/MMEDGit/VPD-Immunity-Estimation/all-schools.csv")

#summary of the data
summary(school)

#plotting the coordinates of the students
student_coordinates <- data.frame(school$lat, school$lon)
student_coordinates
head(student_coordinates)
plot(student_coordinates, main = "Student Coordinates", xlab = "Latitude", ylab = "Longitude", pch = 19, col = "blue")
#NA_values
NA_values <- is.na(student_coordinates)
cleaned_coordinates <- student_coordinates[!NA_values, ]
plot(cleaned_coordinates, main = "Cleaned Student Coordinates", xlab = "Latitude", ylab = "Longitude", pch = 19, col = "green")

#cleaned version of the data frame with only complete cases
cleaned_school <- school[complete.cases(school), ]



#code that generates synthetic coordinates based on the cleaned data to get an idea of how we can simulate outbreaks
linear_regression_coordinates <- lm(lat ~ lon, data = cleaned_school)

synthetic_coordinates <- data.frame(
  lon = runif(n = 1000, min = min(cleaned_school$lon), max = max(cleaned_school$lon))
)

mu <- predict(linear_regression_coordinates,
              newdata = synthetic_coordinates)

sigma <- summary(linear_regression_coordinates)$sigma

simulated_lat <- mu + rnorm(length(mu), mean = 0, sd = sigma)

simulated_coordinates <- data.frame(
  lon = synthetic_coordinates$lon,
  lat = simulated_lat
)

plot(simulated_coordinates, main = "Simulated Student Coordinates", xlab = "Longitude", ylab = "Latitude", pch = 19, col = "red")


