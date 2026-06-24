library(ggplot2)
library(tidyr)
library(dplyr)
library(knitr)

base_dir <- "."
setwd(base_dir)
source("./Phase_2_Dynamical_part/Reed_Frost_model.R")

# Load baseline coverage data
average1 <- readRDS("average_1.rds")
average4 <- readRDS("average_4.rds")

# Scale by cohort size (30 students per grade) over 14 grades
ave1 <- 30 * average1$mean_coverage   # length 14
ave4 <- 30 * average4$mean_coverage   # length 14
ave  <- c(ave1, ave4)                 # length 28

generations_count <- 20
total_pop         <- 14 * 30          # 420
p_contact         <- 0.04

all_rf <- vector("list", 13) # Explicitly sized for the 17 rolling windows

for (t in 1:13) {
  idx <- t:(t + 13)
  idx <- idx[idx <= length(ave)]
  R0  <- round(sum(ave[idx]))
  
  R0 <- min(R0, total_pop - 2)
  
  # Initialize compartments based on background immunity (R0)
  y0_rf <- c(S = total_pop - R0 - 1, E = 1, I = 0, R = R0)
  
  number_of_simulation <-100

  step_t_rf <- vector("list",number_of_simulation)

   for (i in 1:number_of_simulation){
  rf_data <- simulate_reed_frost_seir(generations_count, y0_rf, p = p_contact)
  
  # Calculate cumulative infections: current R minus initial immune cluster
  rf_data <- rf_data |> 
    mutate(Cumulative_Infections = R - first(R))
    step_t_rf[[i]] <- rf_data["Cumulative_Infections"]
  all_rf[[t]] <- step_t_rf
    }
}


final_values <- sapply(step_t_rf, function(df) tail(df[[t]], 1))

summary_table <- data.frame(
  List_Item = 1:length(step_t_rf),
  Final_Infections = final_values
)

#function that get the list of final value
list_for <- function(t){
  final_values <-rep(1,length.out = 100)
   for (j in 1:100){
    final_values[j]<-tail(all_rf[[t]][[j]],n= 1)[1]}
  summary_table <- data.frame(
    List_Item = 1:length(step_t_rf),
    Final_Infections = final_values)
  return(unlist(final_values))
}

list_for(1)
#function lpot
plot_histogram_for <-function(t)
{   df <- data.frame(value = list_for(t))
    ggplot(df, aes(x = value)) + 
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram with ggplot2")
}

#change the argument to plot the hoistogram (argument between 1 and 13)

#How are we going to plot the result for all the 13 years
plot_histogram_for(1)
plot_histogram_for(4)
plot_histogram_for(7)
plot_histogram_for(10)
plot_histogram_for(13)

for()
as.data.frame(all_rf[[ii]])[21,]

df <- as.data.frame(all_rf)

head(df)
all_rf_df

dim(df)
all_rf
