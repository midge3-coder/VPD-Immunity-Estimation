
initial_condiction_R <- function(vaccination_coverage, N, I0, R0 , E) {
  R0 <- round(sum(vaccination_coverage * N))
  S0 <- N - R0 - I0 - E
  return(c(S = S0, E = E, I = I0, R = R0))
}

reed_frost_model <- function(t0,tfinal,initial_conditions, beta ,vactination_coverage,) {
  times <- seq(t0, tfinal, by = 1)
  
}