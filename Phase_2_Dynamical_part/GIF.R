library(ggplot2)
library(dplyr)

base_dir <- "."
setwd(base_dir)
source("./Phase_2_Dynamical_part/Reed_Frost_model.R")

# --- same setup as histogram.R ---
average1 <- readRDS("average_1.rds")
average4 <- readRDS("average_4.rds")

ave1 <- 30 * average1$mean_coverage
ave4 <- 30 * average4$mean_coverage
ave  <- c(ave1, ave4)

generations_count    <- 20
total_pop            <- 14 * 30
p_contact            <- 0.04
window_width         <- 14
number_of_simulation <- 1000
number_of_windows    <- length(ave) - window_width + 1
binwidth             <- 1

add_cumulative_infections <- function(rf_data) {
  rf_data |>
    mutate(
      Cumulative_Infections = .data$E + .data$I + .data$R - first(.data$R)
    )
}

all_rf <- vector("list", number_of_windows)

for (t in seq_len(number_of_windows)) {
  idx <- t:(t + window_width - 1)
  R0  <- round(sum(ave[idx]))
  R0  <- min(R0, total_pop - 2)
  y0_rf <- c(S = total_pop - R0 - 1, E = 1, I = 0, R = R0)

  step_t_rf <- vector("list", number_of_simulation)
  for (i in seq_len(number_of_simulation)) {
    rf_data <- simulate_reed_frost_seir(generations_count, y0_rf, p = p_contact)
    rf_data <- add_cumulative_infections(rf_data)
    step_t_rf[[i]] <- rf_data["Cumulative_Infections"]
  }
  all_rf[[t]] <- step_t_rf
}

list_for <- function(t) {
  final_values <- rep(1, length.out = number_of_simulation)
  for (j in seq_len(number_of_simulation)) {
    final_values[j] <- tail(all_rf[[t]][[j]]$Cumulative_Infections, n = 1)
  }
  unlist(final_values)
}

# Collect all window data and compute shared axis limits
all_values <- unlist(lapply(seq_len(number_of_windows), list_for))
x_max <- max(all_values)
x_min <- 0

max_y <- 0
for (t in seq_len(number_of_windows)) {
  counts <- as.numeric(table(cut(list_for(t),
                                 breaks = seq(x_min, x_max + binwidth, by = binwidth),
                                 include.lowest = TRUE)))
  max_y <- max(max_y, max(counts, na.rm = TRUE))
}
y_max <- ceiling(max_y * 1.08)

# --- output paths ---
frames_dir <- "./Phase_2_Dynamical_part/gif_frames"
gif_path   <- "./Phase_2_Dynamical_part/cumulative_infection_histogram.gif"
dir.create(frames_dir, recursive = TRUE, showWarnings = FALSE)

plot_histogram_for <- function(t) {
  df <- data.frame(value = list_for(t))

  slider_frac <- (t - 1) / (number_of_windows - 1)
  y_head      <- y_max * 0.24
  y_top       <- y_max + y_head
  track_y0    <- y_max + y_head * 0.50
  track_y1    <- y_max + y_head * 0.64
  track_x0    <- x_min
  track_x1    <- x_max
  thumb_x     <- track_x0 + slider_frac * (track_x1 - track_x0)
  thumb_y     <- (track_y0 + track_y1) / 2
  label_y     <- track_y1 + y_head * 0.18

  ggplot(df, aes(x = .data$value)) +
    geom_histogram(
      binwidth  = binwidth,
      fill      = "#377eb8",
      color     = "white",
      linewidth = 0.25,
      boundary  = 0
    ) +
    annotate(
      "rect",
      xmin = track_x0, xmax = track_x1,
      ymin = track_y0, ymax = track_y1,
      fill = "#e8e8e8", color = "#cccccc", linewidth = 0.4
    ) +
    annotate(
      "rect",
      xmin = track_x0, xmax = thumb_x,
      ymin = track_y0, ymax = track_y1,
      fill = "#377eb8", color = NA
    ) +
    annotate(
      "point",
      x = thumb_x, y = thumb_y,
      size = 5.5, shape = 21,
      fill = "white", color = "#377eb8", stroke = 2
    ) +
    annotate(
      "text",
      x = track_x0, y = label_y,
      label = paste("Year:", t),
      hjust = 0, vjust = 0,
      fontface = "bold", size = 6, color = "gray20"
    ) +
    scale_x_continuous(limits = c(x_min, x_max), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, y_top), expand = c(0, 0)) +
    labs(
      title    = "Cumulative infections",
      subtitle = paste(
        number_of_simulation, "stochastic Reed-Frost simulations ·",
        "N =", total_pop, "· p =", p_contact
      ),
      x = "Cumulative infections",
      y = "Simulation count"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title         = element_text(face = "bold", size = 20),
      plot.subtitle      = element_text(color = "gray35", size = 14),
      axis.title         = element_text(face = "bold"),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.margin        = margin(10, 12, 8, 10)
    )
}

for (t in seq_len(number_of_windows)) {
  frame_path <- file.path(frames_dir, sprintf("frame_%03d.png", t))
  ggsave(
    frame_path,
    plot_histogram_for(t),
    width  = 10,
    height = 6,
    dpi    = 150,
    bg     = "white"
  )
  message("Saved ", frame_path)
}

# 200 ms per frame => 5 fps
fps <- 5
ffmpeg_cmd <- sprintf(
  "ffmpeg -y -framerate %d -i %s/frame_%%03d.png -vf \"fps=%d,scale=1200:-1:flags=lanczos,palettegen=stats_mode=diff\" %s/palette.png",
  fps, frames_dir, fps, frames_dir
)
system(ffmpeg_cmd)

ffmpeg_gif_cmd <- sprintf(
  "ffmpeg -y -framerate %d -i %s/frame_%%03d.png -i %s/palette.png -lavfi \"fps=%d,scale=1200:-1:flags=lanczos[x];[x][1:v]paletteuse=dither=bayer:bayer_scale=3\" %s",
  fps, frames_dir, frames_dir, fps, gif_path
)
status <- system(ffmpeg_gif_cmd)

if (status != 0) {
  stop("ffmpeg failed — is ffmpeg installed? (brew install ffmpeg)")
}

message("GIF saved to ", gif_path)
