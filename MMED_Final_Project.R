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

